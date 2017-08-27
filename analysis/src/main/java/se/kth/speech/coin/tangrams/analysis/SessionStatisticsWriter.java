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
import java.io.PrintStream;
import java.math.BigDecimal;
import java.math.MathContext;
import java.nio.file.FileVisitOption;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NavigableMap;
import java.util.Properties;
import java.util.TreeMap;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.xml.bind.JAXBException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.TimestampArithmetic;
import se.kth.speech.coin.tangrams.analysis.io.SessionDataManager;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 2 Aug 2017
 *
 */
final class SessionStatisticsWriter {

	private static final List<String> COLUMN_HEADERS = Arrays.asList("INPATH", "GAME_ID", "GAME_DURATION", "ROUND_COUNT",
			"UTT_COUNT");

	private static final Logger LOGGER = LoggerFactory.getLogger(SessionStatisticsWriter.class);

	private static final MathContext MEAN_DIVISION_CTX = MathContext.DECIMAL64;

	private static final Collector<CharSequence, ?, String> ROW_CELL_JOINER = Collectors.joining("\t");

	private static final String ROW_DELIMITER = System.lineSeparator();

	public static void main(final String[] args) throws JAXBException, IOException {
		final Path[] inpaths = Arrays.stream(args).map(String::trim).filter(path -> !path.isEmpty()).map(Paths::get)
				.toArray(Path[]::new);
		if (inpaths.length < 1) {
			throw new IllegalArgumentException(
					String.format("Usage: %s INPATHS...", SessionStatisticsWriter.class.getSimpleName()));
		} else {
			final NavigableMap<Path, NavigableMap<String, GameSummary>> sessionSummaries = new TreeMap<>();
			for (final Path inpath : inpaths) {
				LOGGER.info("Will read batch job data from \"{}\".", inpath);
				putSessionSummaries(inpath, sessionSummaries);
			}
			write(sessionSummaries.entrySet(), System.out);
		}

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

	private static Stream<GameSummary> getGameSummaries(
			final Collection<? extends Entry<Path, ? extends Map<String, GameSummary>>> sessionSummaries) {
		return sessionSummaries.stream().map(Entry::getValue).map(Map::values).flatMap(Collection::stream);
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

	private static void write(
			final Collection<? extends Entry<Path, ? extends Map<String, GameSummary>>> sessionSummaries,
			final PrintStream out) {
		out.print(COLUMN_HEADERS.stream().collect(ROW_CELL_JOINER));

		for (final Entry<Path, ? extends Map<String, GameSummary>> sessionSummary : sessionSummaries) {
			out.print(ROW_DELIMITER);

			final Path inpath = sessionSummary.getKey();
			final Map<String, GameSummary> gameSummaries = sessionSummary.getValue();
			for (final Entry<String, GameSummary> gameSummary : gameSummaries.entrySet()) {
				final String gameId = gameSummary.getKey();
				final GameSummary summary = gameSummary.getValue();
				final BigDecimal durationInSecs = TimestampArithmetic.toDecimalSeconds(summary.getDuration());
				final List<Object> rowCellVals = Arrays.asList(inpath, gameId, durationInSecs, summary.getCompletedRoundCount(),
						summary.getUttCount());
				out.print(rowCellVals.stream().map(Object::toString).collect(ROW_CELL_JOINER));
			}

		}

		out.print(ROW_DELIMITER);
		writeAggregates(sessionSummaries, out);
	}

	private static void writeAggregates(
			final Collection<? extends Entry<Path, ? extends Map<String, GameSummary>>> sessionSummaries,
			final PrintStream output) {
		{
			// Min vals
			final BigDecimal minDuration = getGameSummaries(sessionSummaries).map(GameSummary::getDuration)
					.min(Comparator.naturalOrder()).map(TimestampArithmetic::toDecimalSeconds).get();
			final long minRoundCount = getGameSummaries(sessionSummaries).mapToLong(GameSummary::getCompletedRoundCount).min()
					.getAsLong();
			final long minUttCount = getGameSummaries(sessionSummaries).mapToLong(GameSummary::getUttCount).min()
					.getAsLong();
			final List<Object> minVals = Arrays.asList("MIN", "", minDuration, minRoundCount, minUttCount);
			output.print(minVals.stream().map(Object::toString).collect(ROW_CELL_JOINER));
			output.print(ROW_DELIMITER);
		}

		{
			// Max vals
			final BigDecimal maxDuration = getGameSummaries(sessionSummaries).map(GameSummary::getDuration)
					.max(Comparator.naturalOrder()).map(TimestampArithmetic::toDecimalSeconds).get();
			final long maxRoundCount = getGameSummaries(sessionSummaries).mapToLong(GameSummary::getCompletedRoundCount).max()
					.getAsLong();
			final long maxUttCount = getGameSummaries(sessionSummaries).mapToLong(GameSummary::getUttCount).max()
					.getAsLong();
			final List<Object> maxVals = Arrays.asList("MAX", "", maxDuration, maxRoundCount, maxUttCount);
			output.print(maxVals.stream().map(Object::toString).collect(ROW_CELL_JOINER));
			output.print(ROW_DELIMITER);
		}

		{
			// Mean vals
			final BigDecimal gameSessionCount = new BigDecimal(getGameSummaries(sessionSummaries).count());
			final BigDecimal totalDuration = getGameSummaries(sessionSummaries).map(GameSummary::getDuration)
					.map(TimestampArithmetic::toDecimalSeconds).reduce((augend, addend) -> augend.add(addend)).get();
			final BigDecimal meanDuration = totalDuration.divide(gameSessionCount, MEAN_DIVISION_CTX);
			final long totalRoundCount = getGameSummaries(sessionSummaries).mapToLong(GameSummary::getCompletedRoundCount).sum();
			final BigDecimal meanRoundCount = new BigDecimal(totalRoundCount).divide(gameSessionCount,
					MEAN_DIVISION_CTX);
			final long totalUttCount = getGameSummaries(sessionSummaries).mapToLong(GameSummary::getUttCount).sum();
			final BigDecimal meanUttCount = new BigDecimal(totalUttCount).divide(gameSessionCount, MEAN_DIVISION_CTX);
			final List<Object> meanVals = Arrays.asList("MEAN", "", meanDuration, meanRoundCount, meanUttCount);
			output.print(meanVals.stream().map(Object::toString).collect(ROW_CELL_JOINER));
			output.print(ROW_DELIMITER);
		}
	}

	private SessionStatisticsWriter() {
	}

}
