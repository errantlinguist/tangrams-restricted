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
import java.util.DoubleSummaryStatistics;
import java.util.EnumSet;
import java.util.IntSummaryStatistics;
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
import java.util.stream.DoubleStream;
import java.util.stream.LongStream;
import java.util.stream.Stream;

import javax.xml.bind.JAXBException;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.OptionGroup;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;

import se.kth.speech.TimestampArithmetic;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
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
		HOURS(HOURS_OPT_NAME) {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("hours")
						.desc("Prints statistics in \"HH:mm:ss.SSS\" notation rather than in seconds as decimal fractions.")
						.build();
			};
		},
		LATEX("l") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("latex").desc("Prints results using tabular LaTeX syntax.")
						.build();
			};
		},
		MINUTES(MINUTES_OPT_NAME) {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("mintes")
						.desc("Prints statistics in \"mm:ss.SSS\" notation rather than in seconds as decimal fractions.")
						.build();
			};
		};

		private static final OptionGroup DURATION_OPTS;

		private static final EnumSet<Parameter> DURATION_PARAMS = EnumSet.of(MINUTES, HOURS);

		private static final Options OPTIONS;

		static {
			OPTIONS = new Options();
			EnumSet.complementOf(Parameter.DURATION_PARAMS).stream().map(Parameter::get).forEach(OPTIONS::addOption);
			DURATION_OPTS = new OptionGroup();
			Parameter.DURATION_PARAMS.stream().map(Parameter::get).forEach(DURATION_OPTS::addOption);
			OPTIONS.addOptionGroup(DURATION_OPTS);
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

	private static final List<String> COLUMN_HEADERS = Arrays.asList("INPATH", "GAME_ID", "GAME_DURATION",
			"ROUND_COUNT", "UTT_COUNT", "TOKEN_COUNT", "UTT_TOKENS_MIN", "UTT_TOKENS_MAX", "UTT_TOKENS_MEAN",
			"UTT_DURATION_MIN", "UTT_DURATION_MAX", "UTT_DURATION_MEAN");

	private static final BinaryOperator<Duration> DURATION_SUMMER = (augend, addend) -> augend.plus(addend);

	private static final String HOURS_OPT_NAME = "h";

	private static final Collector<CharSequence, ?, String> LATEX_ROW_CELL_JOINER = Collectors.joining("\t&\t", "",
			" \\\\");

	private static final Logger LOGGER = LoggerFactory.getLogger(SessionStatisticsWriter.class);

	private static final MathContext MEAN_DIVISION_CTX = MathContext.DECIMAL64;

	private static final String MINUTES_OPT_NAME = "m";

	private static final Charset OUTPUT_ENCODING = StandardCharsets.UTF_8;

	private static final String ROW_DELIMITER = System.lineSeparator();

	private static final Collector<CharSequence, ?, String> TAB_ROW_CELL_JOINER = Collectors.joining("\t");

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

	private static String formatDurationSeconds(final Duration duration) {
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

				final Function<Duration, String> durationFormatter = parseDurationFormatter();
				final Collector<CharSequence, ?, String> rowCellJoiner = cl.hasOption(Parameter.LATEX.optName)
						? LATEX_ROW_CELL_JOINER : TAB_ROW_CELL_JOINER;
				try (PrintWriter outputWriter = new PrintWriter(new OutputStreamWriter(System.out, OUTPUT_ENCODING))) {
					final SessionStatisticsWriter writer = new SessionStatisticsWriter(outputWriter, durationFormatter,
							rowCellJoiner, sessionSummaries.size());
					writer.accept(sessionSummaries.entrySet());
				}
			}
		}
	}

	private static Function<Duration, String> parseDurationFormatter() {
		final Function<Duration, String> result;
		final String selectedDurationOpt = Parameter.DURATION_OPTS.getSelected();
		if (selectedDurationOpt == null) {
			result = SessionStatisticsWriter::formatDurationSeconds;
		} else {
			switch (selectedDurationOpt) {
			case MINUTES_OPT_NAME: {
				result = TimestampArithmetic::formatDurationMinutes;
				break;
			}
			case HOURS_OPT_NAME: {
				result = TimestampArithmetic::formatDurationHours;
				break;
			}
			default: {
				throw new AssertionError("No logic for case statement val \"" + selectedDurationOpt + "\".");
			}
			}
		}
		return result;
	}

	private static void putSessionSummaries(final Path inpath,
			final Map<? super Path, ? super NavigableMap<String, GameSummary>> sessionSummaries)
			throws JAXBException, IOException {
		final Iterable<Path> infilePaths = Files.walk(inpath, FileVisitOption.FOLLOW_LINKS).filter(Files::isRegularFile)
				.filter(filePath -> filePath.getFileName().toString().endsWith(".properties"))::iterator;
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

	private final Collector<? super String, ?, String> rowCellJoiner;

	private final LoadingCache<GameSummary, DoubleSummaryStatistics> sessionUtteranceDurationStats;

	private final LoadingCache<GameSummary, IntSummaryStatistics> sessionUtteranceTokenCountStats;

	public SessionStatisticsWriter(final PrintWriter outputWriter,
			final Function<? super Duration, String> durationFormatter,
			final Collector<? super String, ?, String> rowCellJoiner, final int expectedUniqueSessionCount) {
		this.outputWriter = outputWriter;
		this.durationFormatter = durationFormatter;
		this.rowCellJoiner = rowCellJoiner;

		sessionUtteranceTokenCountStats = CacheBuilder.newBuilder().initialCapacity(expectedUniqueSessionCount)
				.build(CacheLoader.from(summary -> {
					return summary.getUtterances().map(Utterance::getTokens).mapToInt(Collection::size)
							.summaryStatistics();
				}));

		sessionUtteranceDurationStats = CacheBuilder.newBuilder().initialCapacity(expectedUniqueSessionCount)
				.build(CacheLoader.from(summary -> {
					final DoubleStream uttDurations = summary.getUtterances()
							.mapToDouble(utt -> utt.getEndTime() - utt.getStartTime());
					return uttDurations.summaryStatistics();
				}));
	}

	@Override
	public void accept(final Collection<? extends Entry<Path, ? extends Map<String, GameSummary>>> sessionSummaries) {
		outputWriter.print(COLUMN_HEADERS.stream().collect(rowCellJoiner));

		for (final Entry<Path, ? extends Map<String, GameSummary>> sessionSummary : sessionSummaries) {
			outputWriter.print(ROW_DELIMITER);

			final Path inpath = sessionSummary.getKey();
			final Map<String, GameSummary> gameSummaries = sessionSummary.getValue();
			for (final Entry<String, GameSummary> gameSummary : gameSummaries.entrySet()) {
				final String gameId = gameSummary.getKey();
				final GameSummary summary = gameSummary.getValue();
				final String durationRepr = durationFormatter.apply(summary.getDuration());
				final IntSummaryStatistics tokenCountStats = fetchUtteranceTokenCountStats(summary);
				final DoubleSummaryStatistics uttDurationStats = fetchUtteranceDurationStats(summary);
				final List<Object> rowCellVals = Arrays.asList(inpath, gameId, durationRepr,
						summary.getCompletedRoundCount(), tokenCountStats.getCount(), tokenCountStats.getSum(),
						tokenCountStats.getMin(), tokenCountStats.getMax(), tokenCountStats.getAverage(),
						uttDurationStats.getMin(), uttDurationStats.getMax(), uttDurationStats.getAverage());
				outputWriter.print(rowCellVals.stream().map(Object::toString).collect(rowCellJoiner));
			}

		}

		outputWriter.print(ROW_DELIMITER);
		writeAggregates(sessionSummaries);
	}

	private DoubleSummaryStatistics fetchUtteranceDurationStats(final GameSummary summary) {
		return sessionUtteranceDurationStats.getUnchecked(summary);
	}

	private IntSummaryStatistics fetchUtteranceTokenCountStats(final GameSummary summary) {
		return sessionUtteranceTokenCountStats.getUnchecked(summary);
	}

	private void writeAggregateMaxima(
			final Collection<? extends Entry<Path, ? extends Map<String, GameSummary>>> sessionSummaries,
			final Collection<IntSummaryStatistics> tokenCountStats,
			final Collection<DoubleSummaryStatistics> utteranceDurationStats) {
		final String durationRepr = getGameSummaries(sessionSummaries).map(GameSummary::getDuration)
				.max(Comparator.naturalOrder()).map(durationFormatter).get();
		final long roundCount = getGameSummaries(sessionSummaries).mapToLong(GameSummary::getCompletedRoundCount).max()
				.getAsLong();

		final long uttCount = tokenCountStats.stream().mapToLong(IntSummaryStatistics::getCount).max().getAsLong();
		final long tokenCount = tokenCountStats.stream().mapToLong(IntSummaryStatistics::getSum).max().getAsLong();
		final int minUttTokenCount = tokenCountStats.stream().mapToInt(IntSummaryStatistics::getMin).max().getAsInt();
		final int maxUttTokenCount = tokenCountStats.stream().mapToInt(IntSummaryStatistics::getMax).max().getAsInt();
		final double meanUttTokenCount = tokenCountStats.stream().mapToDouble(IntSummaryStatistics::getAverage).max()
				.getAsDouble();

		final double minUttDuration = utteranceDurationStats.stream().mapToDouble(DoubleSummaryStatistics::getMin).max()
				.getAsDouble();
		final double maxUttDuration = utteranceDurationStats.stream().mapToDouble(DoubleSummaryStatistics::getMax).max()
				.getAsDouble();
		final double meanUttDuration = utteranceDurationStats.stream().mapToDouble(DoubleSummaryStatistics::getAverage)
				.max().getAsDouble();

		final List<Object> vals = Arrays.asList("MAX", "", durationRepr, roundCount, uttCount, tokenCount,
				minUttTokenCount, maxUttTokenCount, meanUttTokenCount, minUttDuration, maxUttDuration, meanUttDuration);
		outputWriter.print(vals.stream().map(Object::toString).collect(rowCellJoiner));
		outputWriter.print(ROW_DELIMITER);
	}

	private void writeAggregateMeans(
			final Collection<? extends Entry<Path, ? extends Map<String, GameSummary>>> sessionSummaries,
			final Collection<IntSummaryStatistics> tokenCountStats,
			final Collection<DoubleSummaryStatistics> utteranceDurationStats) {
		final BigDecimal gameSessionCount = new BigDecimal(getGameSummaries(sessionSummaries).count());
		final Duration totalDuration = getGameSummaries(sessionSummaries).map(GameSummary::getDuration)
				.reduce(DURATION_SUMMER).get();
		final String meanDurationRepr = durationFormatter
				.apply(totalDuration.dividedBy(gameSessionCount.longValueExact()));
		final BigDecimal totalRoundCount = new BigDecimal(
				getGameSummaries(sessionSummaries).mapToLong(GameSummary::getCompletedRoundCount).sum());
		final BigDecimal meanRoundCount = totalRoundCount.divide(gameSessionCount, MEAN_DIVISION_CTX);

		final long[] uttCounts = tokenCountStats.stream().mapToLong(IntSummaryStatistics::getCount).toArray();
		final BigDecimal uttCountArrayLength = new BigDecimal(uttCounts.length);
		final BigDecimal totalUttCount = new BigDecimal(Arrays.stream(uttCounts).sum());
		final BigDecimal meanUttCount = totalUttCount.divide(uttCountArrayLength, MEAN_DIVISION_CTX);

		final LongStream tokenCounts = tokenCountStats.stream().mapToLong(IntSummaryStatistics::getSum);
		final BigDecimal totalTokenCount = new BigDecimal(tokenCounts.sum());
		final BigDecimal meanTokenCount = totalTokenCount.divide(uttCountArrayLength, MEAN_DIVISION_CTX);

		final BigDecimal minUttTokenCount = new BigDecimal(
				tokenCountStats.stream().mapToInt(IntSummaryStatistics::getMin).sum()).divide(uttCountArrayLength,
						MEAN_DIVISION_CTX);
		final BigDecimal maxUttTokenCount = new BigDecimal(
				tokenCountStats.stream().mapToInt(IntSummaryStatistics::getMax).sum()).divide(uttCountArrayLength,
						MEAN_DIVISION_CTX);
		final BigDecimal meanUttTokenCount = new BigDecimal(
				tokenCountStats.stream().mapToDouble(IntSummaryStatistics::getAverage).sum())
						.divide(uttCountArrayLength, MEAN_DIVISION_CTX);

		final double minUttDuration = utteranceDurationStats.stream().mapToDouble(DoubleSummaryStatistics::getMin)
				.average().getAsDouble();
		final double maxUttDuration = utteranceDurationStats.stream().mapToDouble(DoubleSummaryStatistics::getMax)
				.average().getAsDouble();
		final double meanUttDuration = utteranceDurationStats.stream().mapToDouble(DoubleSummaryStatistics::getAverage)
				.average().getAsDouble();

		final List<Object> vals = Arrays.asList("MEAN", "", meanDurationRepr, meanRoundCount, meanUttCount,
				meanTokenCount, minUttTokenCount, maxUttTokenCount, meanUttTokenCount, minUttDuration, maxUttDuration,
				meanUttDuration);
		outputWriter.print(vals.stream().map(Object::toString).collect(rowCellJoiner));
		outputWriter.print(ROW_DELIMITER);
	}

	private void writeAggregateMinima(
			final Collection<? extends Entry<Path, ? extends Map<String, GameSummary>>> sessionSummaries,
			final Collection<IntSummaryStatistics> tokenCountStats,
			final Collection<DoubleSummaryStatistics> utteranceDurationStats) {
		final String durationRepr = getGameSummaries(sessionSummaries).map(GameSummary::getDuration)
				.min(Comparator.naturalOrder()).map(durationFormatter).get();
		final long roundCount = getGameSummaries(sessionSummaries).mapToLong(GameSummary::getCompletedRoundCount).min()
				.getAsLong();

		final long uttCount = tokenCountStats.stream().mapToLong(IntSummaryStatistics::getCount).min().getAsLong();
		final long tokenCount = tokenCountStats.stream().mapToLong(IntSummaryStatistics::getSum).min().getAsLong();
		final int minUttTokenCount = tokenCountStats.stream().mapToInt(IntSummaryStatistics::getMin).min().getAsInt();
		final int maxUttTokenCount = tokenCountStats.stream().mapToInt(IntSummaryStatistics::getMax).min().getAsInt();
		final double meanUttTokenCount = tokenCountStats.stream().mapToDouble(IntSummaryStatistics::getAverage).min()
				.getAsDouble();

		final double minUttDuration = utteranceDurationStats.stream().mapToDouble(DoubleSummaryStatistics::getMin).min()
				.getAsDouble();
		final double maxUttDuration = utteranceDurationStats.stream().mapToDouble(DoubleSummaryStatistics::getMax).min()
				.getAsDouble();
		final double meanUttDuration = utteranceDurationStats.stream().mapToDouble(DoubleSummaryStatistics::getAverage)
				.min().getAsDouble();

		final List<Object> vals = Arrays.asList("MIN", "", durationRepr, roundCount, uttCount, tokenCount,
				minUttTokenCount, maxUttTokenCount, meanUttTokenCount, minUttDuration, maxUttDuration, meanUttDuration);
		outputWriter.print(vals.stream().map(Object::toString).collect(rowCellJoiner));
		outputWriter.print(ROW_DELIMITER);
	}

	private void writeAggregates(
			final Collection<? extends Entry<Path, ? extends Map<String, GameSummary>>> sessionSummaries) {
		final List<IntSummaryStatistics> tokenCountStats = Arrays.asList(getGameSummaries(sessionSummaries)
				.map(this::fetchUtteranceTokenCountStats).toArray(IntSummaryStatistics[]::new));
		final List<DoubleSummaryStatistics> utteranceDurationStats = Arrays.asList(getGameSummaries(sessionSummaries)
				.map(this::fetchUtteranceDurationStats).toArray(DoubleSummaryStatistics[]::new));
		writeAggregateMinima(sessionSummaries, tokenCountStats, utteranceDurationStats);
		writeAggregateMaxima(sessionSummaries, tokenCountStats, utteranceDurationStats);
		writeAggregateMeans(sessionSummaries, tokenCountStats, utteranceDurationStats);
		writeAggregateSums(sessionSummaries, tokenCountStats, utteranceDurationStats);
	}

	private void writeAggregateSums(
			final Collection<? extends Entry<Path, ? extends Map<String, GameSummary>>> sessionSummaries,
			final Collection<IntSummaryStatistics> tokenCountStats,
			final Collection<DoubleSummaryStatistics> utteranceDurationStats) {
		final String durationRepr = getGameSummaries(sessionSummaries).map(GameSummary::getDuration)
				.reduce(DURATION_SUMMER).map(durationFormatter).get();
		final long roundCount = getGameSummaries(sessionSummaries).mapToLong(GameSummary::getCompletedRoundCount).sum();

		final long uttCount = tokenCountStats.stream().mapToLong(IntSummaryStatistics::getCount).sum();
		final long tokenCount = tokenCountStats.stream().mapToLong(IntSummaryStatistics::getSum).sum();
		final int minUttTokenCount = tokenCountStats.stream().mapToInt(IntSummaryStatistics::getMin).sum();
		final int maxUttTokenCount = tokenCountStats.stream().mapToInt(IntSummaryStatistics::getMax).sum();
		final double meanUttTokenCount = tokenCountStats.stream().mapToDouble(IntSummaryStatistics::getAverage).sum();

		final double minUttDuration = utteranceDurationStats.stream().mapToDouble(DoubleSummaryStatistics::getMin)
				.sum();
		final double maxUttDuration = utteranceDurationStats.stream().mapToDouble(DoubleSummaryStatistics::getMax)
				.sum();
		final double meanUttDuration = utteranceDurationStats.stream().mapToDouble(DoubleSummaryStatistics::getAverage)
				.sum();

		final List<Object> vals = Arrays.asList("SUM", "", durationRepr, roundCount, uttCount, tokenCount,
				minUttTokenCount, maxUttTokenCount, meanUttTokenCount, minUttDuration, maxUttDuration, meanUttDuration);
		outputWriter.print(vals.stream().map(Object::toString).collect(rowCellJoiner));
		outputWriter.print(ROW_DELIMITER);
	}

}
