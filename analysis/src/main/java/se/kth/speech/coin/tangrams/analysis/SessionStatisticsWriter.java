/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.analysis.
 *
 *  se.kth.speech.coin.tangrams-restricted.analysis is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/
package se.kth.speech.coin.tangrams.analysis;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.math.BigDecimal;
import java.math.MathContext;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileVisitOption;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Duration;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NavigableMap;
import java.util.Properties;
import java.util.TreeMap;
import java.util.function.BinaryOperator;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.xml.bind.JAXBException;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.TimestampArithmetic;
import se.kth.speech.coin.tangrams.analysis.io.SessionDataManager;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 2 Aug 2017
 *
 */
final class SessionStatisticsWriter
		implements Consumer<Collection<? extends Entry<Path, ? extends Map<String, GameSummary>>>> { // NO_UCD
																										// (unused
																										// code)

	private enum Parameter implements Supplier<Option> {
		HELP("?") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("help").desc("Prints this message.").build();
			}
		},
		MINUTES("m") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("mintes")
						.desc("Prints statistics in \"minutes:seconds\" notation rather than in seconds as decimal fractions.")
						.build();
			};
		};

		private static final Options OPTIONS = createOptions();

		private static Options createOptions() {
			final Options result = new Options();
			Arrays.stream(Parameter.values()).map(Parameter::get).forEach(result::addOption);
			return result;
		}

		private static void printHelp() {
			final HelpFormatter formatter = new HelpFormatter();
			formatter.printHelp(SegmentTimedUtteranceWriter.class.getSimpleName() + " INPATHS...", OPTIONS);
		}

		protected final String optName;

		private Parameter(final String optName) {
			this.optName = optName;
		}

	}

	private static final BinaryOperator<BigDecimal> BIG_DECIMAL_SUMMER = (augend, addend) -> augend.add(addend);

	private static final List<String> COLUMN_HEADERS = Arrays.asList("INPATH", "GAME_ID", "GAME_DURATION",
			"ROUND_COUNT", "UTT_COUNT");

	private static final BinaryOperator<Duration> DURATION_SUMMER = (augend, addend) -> augend.plus(addend);

	private static final Logger LOGGER = LoggerFactory.getLogger(SessionStatisticsWriter.class);

	private static final MathContext MEAN_DIVISION_CTX = MathContext.DECIMAL64;

	private static final Charset OUTPUT_ENCODING = StandardCharsets.UTF_8;

	private static final Collector<CharSequence, ?, String> ROW_CELL_JOINER = Collectors.joining("\t");

	private static final String ROW_DELIMITER = System.lineSeparator();

	public static void main(final String[] args) throws JAXBException, IOException, ParseException {
		final CommandLineParser parser = new DefaultParser();
		final CommandLine cl = parser.parse(Parameter.OPTIONS, args);
		main(cl);
	}

	private static NavigableMap<String, GameSummary> createSessionGameSummaries(final SessionDataManager sessionData)
			throws JAXBException, IOException {
		final SessionGameManager sessionDiagMgr = new SessionGameManager(sessionData);
		final SessionGame canonicalGame = sessionDiagMgr.getCanonicalGame();
		final GameSummary summary = new GameSummary(canonicalGame.getHistory(), canonicalGame.getEventDialogues());
		final NavigableMap<String, GameSummary> result = new TreeMap<>();
		result.put(canonicalGame.getGameId(), summary);
		return result;
	}

	private static String formatDurationAsSeconds(final Duration duration) {
		final BigDecimal durationInSecs = TimestampArithmetic.toDecimalSeconds(duration);
		return durationInSecs.toString();
	}

	private static Stream<GameSummary> getGameSummaries(
			final Collection<? extends Entry<Path, ? extends Map<String, GameSummary>>> sessionSummaries) {
		return sessionSummaries.stream().map(Entry::getValue).map(Map::values).flatMap(Collection::stream);
	}

	private static void main(final CommandLine cl) throws JAXBException, IOException {
		if (cl.hasOption(Parameter.HELP.optName)) {
			Parameter.printHelp();
		} else {
			final Path[] inpaths = cl.getArgList().stream().map(String::trim).filter(path -> !path.isEmpty())
					.map(Paths::get).toArray(Path[]::new);
			if (inpaths.length < 1) {
				throw new IllegalArgumentException(
						String.format("Usage: %s INPATHS...", SessionStatisticsWriter.class.getSimpleName()));
			} else {
				final NavigableMap<Path, NavigableMap<String, GameSummary>> sessionSummaries = new TreeMap<>();
				for (final Path inpath : inpaths) {
					LOGGER.info("Will read batch job data from \"{}\".", inpath);
					putSessionSummaries(inpath, sessionSummaries);
				}

				final Function<Duration, String> durationFormatter = cl.hasOption(Parameter.MINUTES.optName)
						? TimestampArithmetic::formatDurationHours : SessionStatisticsWriter::formatDurationAsSeconds;
				try (PrintWriter outputWriter = new PrintWriter(new OutputStreamWriter(System.out, OUTPUT_ENCODING))) {
					final SessionStatisticsWriter writer = new SessionStatisticsWriter(outputWriter, durationFormatter);
					writer.accept(sessionSummaries.entrySet());
				}
			}
		}
	}

	private static void putSessionSummaries(final Path inpath,
			final Map<? super Path, ? super NavigableMap<String, GameSummary>> sessionSummaries)
			throws JAXBException, IOException {
		final Path[] infilePaths = Files.walk(inpath, FileVisitOption.FOLLOW_LINKS).filter(Files::isRegularFile)
				.filter(filePath -> filePath.getFileName().toString().endsWith(".properties")).toArray(Path[]::new);
		for (final Path infilePath : infilePaths) {
			LOGGER.info("Reading batch job properties from \"{}\".", infilePath);
			final Properties props = new Properties();
			try (final InputStream propsInstream = Files.newInputStream(infilePath)) {
				props.load(propsInstream);
			}
			final SessionDataManager sessionData = SessionDataManager.create(infilePath);
			final NavigableMap<String, GameSummary> sessionSummary = createSessionGameSummaries(sessionData);
			sessionSummaries.put(infilePath, sessionSummary);
		}
	}

	private final Function<? super Duration, String> durationFormatter;

	private final PrintWriter outputWriter;

	public SessionStatisticsWriter(final PrintWriter outputWriter,
			final Function<? super Duration, String> durationFormatter) {
		this.outputWriter = outputWriter;
		this.durationFormatter = durationFormatter;
	}

	@Override
	public void accept(final Collection<? extends Entry<Path, ? extends Map<String, GameSummary>>> sessionSummaries) {
		outputWriter.print(COLUMN_HEADERS.stream().collect(ROW_CELL_JOINER));

		for (final Entry<Path, ? extends Map<String, GameSummary>> sessionSummary : sessionSummaries) {
			outputWriter.print(ROW_DELIMITER);

			final Path inpath = sessionSummary.getKey();
			final Map<String, GameSummary> gameSummaries = sessionSummary.getValue();
			for (final Entry<String, GameSummary> gameSummary : gameSummaries.entrySet()) {
				final String gameId = gameSummary.getKey();
				final GameSummary summary = gameSummary.getValue();
				final Duration duration = summary.getDuration();
				final String durationRepr = durationFormatter.apply(duration);
				final List<Object> rowCellVals = Arrays.asList(inpath, gameId, durationRepr,
						summary.getCompletedRoundCount(), summary.getUttCount());
				outputWriter.print(rowCellVals.stream().map(Object::toString).collect(ROW_CELL_JOINER));
			}

		}

		outputWriter.print(ROW_DELIMITER);
		writeAggregates(sessionSummaries);
	}

	private void writeAggregates(
			final Collection<? extends Entry<Path, ? extends Map<String, GameSummary>>> sessionSummaries) {
		{
			// Min vals
			final String minDurationRepr = getGameSummaries(sessionSummaries).map(GameSummary::getDuration)
					.min(Comparator.naturalOrder()).map(durationFormatter).get();
			final long minRoundCount = getGameSummaries(sessionSummaries).mapToLong(GameSummary::getCompletedRoundCount)
					.min().getAsLong();
			final long minUttCount = getGameSummaries(sessionSummaries).mapToLong(GameSummary::getUttCount).min()
					.getAsLong();
			final List<Object> minVals = Arrays.asList("MIN", "", minDurationRepr, minRoundCount, minUttCount);
			outputWriter.print(minVals.stream().map(Object::toString).collect(ROW_CELL_JOINER));
			outputWriter.print(ROW_DELIMITER);
		}

		{
			// Max vals
			final String maxDurationRepr = getGameSummaries(sessionSummaries).map(GameSummary::getDuration)
					.max(Comparator.naturalOrder()).map(durationFormatter).get();
			final long maxRoundCount = getGameSummaries(sessionSummaries).mapToLong(GameSummary::getCompletedRoundCount)
					.max().getAsLong();
			final long maxUttCount = getGameSummaries(sessionSummaries).mapToLong(GameSummary::getUttCount).max()
					.getAsLong();
			final List<Object> maxVals = Arrays.asList("MAX", "", maxDurationRepr, maxRoundCount, maxUttCount);
			outputWriter.print(maxVals.stream().map(Object::toString).collect(ROW_CELL_JOINER));
			outputWriter.print(ROW_DELIMITER);
		}

		{
			// Mean vals
			final BigDecimal gameSessionCount = new BigDecimal(getGameSummaries(sessionSummaries).count());
			final Duration totalDuration = getGameSummaries(sessionSummaries).map(GameSummary::getDuration)
					.reduce(DURATION_SUMMER).get();
			final String meanDurationRepr = durationFormatter
					.apply(totalDuration.dividedBy(gameSessionCount.longValueExact()));
			final long totalRoundCount = getGameSummaries(sessionSummaries)
					.mapToLong(GameSummary::getCompletedRoundCount).sum();
			final BigDecimal meanRoundCount = new BigDecimal(totalRoundCount).divide(gameSessionCount,
					MEAN_DIVISION_CTX);
			final long totalUttCount = getGameSummaries(sessionSummaries).mapToLong(GameSummary::getUttCount).sum();
			final BigDecimal meanUttCount = new BigDecimal(totalUttCount).divide(gameSessionCount, MEAN_DIVISION_CTX);
			final List<Object> meanVals = Arrays.asList("MEAN", "", meanDurationRepr, meanRoundCount, meanUttCount);
			outputWriter.print(meanVals.stream().map(Object::toString).collect(ROW_CELL_JOINER));
			outputWriter.print(ROW_DELIMITER);
		}

		{
			// Summed vals
			final String totalDurationRepr = getGameSummaries(sessionSummaries).map(GameSummary::getDuration)
					.reduce(DURATION_SUMMER).map(durationFormatter).get();
			final long totalRoundCount = getGameSummaries(sessionSummaries)
					.mapToLong(GameSummary::getCompletedRoundCount).sum();
			final long totalUttCount = getGameSummaries(sessionSummaries).mapToLong(GameSummary::getUttCount).sum();
			final List<Object> maxVals = Arrays.asList("SUM", "", totalDurationRepr, totalRoundCount, totalUttCount);
			outputWriter.print(maxVals.stream().map(Object::toString).collect(ROW_CELL_JOINER));
			outputWriter.print(ROW_DELIMITER);
		}
	}

}
