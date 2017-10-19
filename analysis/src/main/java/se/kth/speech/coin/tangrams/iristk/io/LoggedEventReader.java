/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.analysis.
 *
 *  tangrams is free software: you can redistribute it and/or modify
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
package se.kth.speech.coin.tangrams.iristk.io;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.Charset;
import java.nio.file.FileVisitOption;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Collection;
import java.util.EnumMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.HashBasedTable;
import com.google.common.collect.Maps;
import com.google.common.collect.Table;

import iristk.system.Event;
import iristk.util.Record;
import iristk.util.Record.JsonToRecordException;
import se.kth.speech.coin.tangrams.analysis.GameHistory;
import se.kth.speech.coin.tangrams.analysis.GameHistoryCollector;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;
import se.kth.speech.coin.tangrams.iristk.EventTimes;
import se.kth.speech.coin.tangrams.iristk.GameEvent;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;
import se.kth.speech.coin.tangrams.iristk.GameStateDescriptions;
import se.kth.speech.coin.tangrams.iristk.events.HashableModelDescription;
import se.kth.speech.coin.tangrams.iristk.events.ImageVisualizationInfoDescription;
import se.kth.speech.coin.tangrams.iristk.events.ModelDescription;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Apr 5, 2017
 *
 */
public final class LoggedEventReader {

	public static final Charset CHARSET = Record.JSON_CHARSET;

	private static final Predicate<Event> DEFAULT_EVENT_FILTER = event -> true;

	private static final Pattern EMPTY_OR_WHITESPACE_PATTERN = Pattern.compile("\\s*");

	private static final Pattern LOGGED_EVENT_FILE_NAME_PATTERN = Pattern.compile("events-(.+?)\\.txt");

	private static final Logger LOGGER = LoggerFactory.getLogger(LoggedEventReader.class);

	/**
	 *
	 * @param lines
	 *            The logged events to parse, one on each line.
	 * @return The successfully-parsed {@link Event} instances.
	 */
	public static Stream<Event> parseLoggedEvents(final Stream<String> lines) {
		return lines.filter(line -> !EMPTY_OR_WHITESPACE_PATTERN.matcher(line).matches()).flatMap(line -> {
			Stream<Event> result = Stream.empty();
			try {
				final Record record = Record.fromJSON(line);
				if (record instanceof Event) {
					result = Stream.of((Event) record);
				}
			} catch (final JsonToRecordException e) {
				throw new UncheckedIOException(e);
			}
			return result;
		});
	}

	/**
	 *
	 * @param eventLogPath
	 *            A {@link Path} to the logged events to parse, one on each
	 *            line.
	 * @return The successfully-parsed {@link Event} instances.
	 * @throws IOException
	 *             If an I/O error occurs while opening the file.
	 */
	public static Stream<Event> readLoggedEvents(final Path eventLogPath) throws IOException {
		final Stream<String> lines = readLines(eventLogPath);
		return parseLoggedEvents(lines);
	}

	private static Map<GameManagementEvent.Attribute, Object> createGameAttrMap(final Event event) {
		final Map<GameManagementEvent.Attribute, Object> result = new EnumMap<>(GameManagementEvent.Attribute.class);
		for (final GameManagementEvent.Attribute gameAttr : GameManagementEvent.Attribute.values()) {
			final Object attrValue = event.get(gameAttr.toString());
			if (attrValue != null) {
				result.put(gameAttr, attrValue);
			}
		}
		return result;
	}

	private static GameEvent createGameEvent(final Event event) {
		return new GameEvent(event.getName(), event.getSender(), event.getId(), event.getString("system"),
				EventTimes.parseEventTime(event.getTime()), createGameAttrMap(event));
	}

	private static Stream<String> readLines(final Path eventLogPath) throws IOException {
		return Files.lines(eventLogPath, CHARSET);
	}

	private final Function<ImageVisualizationInfoDescription, ImageVisualizationInfo> imgVizInfoFactory;

	private final Function<ModelDescription, HashableModelDescription> modelDescFactory;

	public LoggedEventReader(final int expectedUniqueGameModels, final int expectedUniqueImgVizInfoData) {
		// final Map<ModelDescription, HashableModelDescription> modelDescs =
		// Maps
		// .newHashMapWithExpectedSize(expectedUniqueGameModels);
		modelDescFactory = HashableModelDescription::new;
		// final Map<ImageVisualizationInfoDescription, ImageVisualizationInfo>
		// imgVizInfo = Maps
		// .newHashMapWithExpectedSize(expectedUniqueImgVizInfoData);
		imgVizInfoFactory = ImageVisualizationInfoDescription::toHashable;
	}

	/**
	 *
	 * @param loggedEvents
	 *            The logged events to process.
	 * @return A new {@link Map} of game IDs to their respective
	 *         {@link GameHistory histories}.
	 */
	public Map<String, GameHistory> createGameHistoryMap(final Stream<Event> loggedEvents) {
		final Event[] loggedEventArray = loggedEvents.toArray(Event[]::new);
		final Supplier<Map<String, GameHistory>> mapFactory = () -> Maps
				.newHashMapWithExpectedSize(loggedEventArray.length);
		return Arrays.stream(loggedEventArray).map(LoggedEventReader::createGameEvent)
				.collect(new GameHistoryCollector(mapFactory, modelDescFactory, imgVizInfoFactory));
	}

	/**
	 *
	 * @param sessionLogDir
	 *            A {@link Path} denoting the session log directory to process.
	 * @param expectedEventLogFileCount
	 *            The expected number of event logs in the directory to process.
	 * @return A new {@link Map} of player IDs mapped to a {@link Path} pointing
	 *         to the player's respective event log.
	 * @throws IOException
	 *             If an error occurs while
	 *             {@link Files#walk(Path, FileVisitOption...) walking} through
	 *             the given directory.
	 */
	public Map<String, Path> createPlayerEventLogFileMap(final Path sessionLogDir, final int expectedEventLogFileCount)
			throws IOException {
		final Map<String, Path> result = Maps.newHashMapWithExpectedSize(expectedEventLogFileCount);
		try (Stream<Path> filePaths = Files.walk(sessionLogDir, FileVisitOption.FOLLOW_LINKS)) {
			filePaths.forEach(filePath -> {
				final Matcher m = LOGGED_EVENT_FILE_NAME_PATTERN.matcher(filePath.getFileName().toString());
				if (m.matches()) {
					final String playerId = m.group(1);
					result.put(playerId, filePath);
				}
			});
		}
		final int playerEventLogFileCount = result.size();
		if (playerEventLogFileCount < expectedEventLogFileCount) {
			throw new IllegalArgumentException(
					String.format("Expected to find data files for at least %d unique player(s) but found %d instead.",
							expectedEventLogFileCount, playerEventLogFileCount));
		}
		return result;
	}

	/**
	 *
	 * @param playerEventLogFilePaths
	 *            A mapping of log {@link Path paths} to read for each player
	 *            ID.
	 * @return A new {@link Table}, with game ID as the row key and player ID as
	 *         the column key, mapping to the relevant {@link GameHistory}
	 *         representing the logged event history for a given game from the
	 *         perspective of a given player.
	 * @throws IOException
	 *             If an error occurs while reading one of the provided event
	 *             log file paths.
	 */
	public Table<String, String, GameHistory> createPlayerGameHistoryTable(
			final Collection<Entry<String, Path>> playerEventLogFilePaths) throws IOException {
		return createPlayerGameHistoryTable(playerEventLogFilePaths, DEFAULT_EVENT_FILTER);
	}

	/**
	 *
	 * @param playerEventLogFilePaths
	 *            A mapping of log {@link Path paths} to read for each player
	 *            ID.
	 * @param expectedUniqueGameCount
	 *            The number of unique games represented in the file(s) to
	 *            parse.
	 * @return A new {@link Table}, with game ID as the row key and player ID as
	 *         the column key, mapping to the relevant {@link GameHistory}
	 *         representing the logged event history for a given game from the
	 *         perspective of a given player.
	 * @throws IOException
	 *             If an error occurs while reading one of the provided event
	 *             log file paths.
	 */
	public Table<String, String, GameHistory> createPlayerGameHistoryTable(
			final Collection<Entry<String, Path>> playerEventLogFilePaths, final int expectedUniqueGameCount)
			throws IOException {
		return createPlayerGameHistoryTable(playerEventLogFilePaths, expectedUniqueGameCount, DEFAULT_EVENT_FILTER);
	}

	/**
	 *
	 * @param playerEventLogFilePaths
	 *            A mapping of log {@link Path paths} to read for each player
	 *            ID.
	 * @param expectedUniqueGameCount
	 *            The number of unique games represented in the file(s) to
	 *            parse.
	 * @param eventFilter
	 *            A positive (i.e.&nbsp;whitelisting) filter for the
	 *            {@link Event events} to include.
	 * @return A new {@link Table}, with game ID as the row key and player ID as
	 *         the column key, mapping to the relevant {@link GameHistory}
	 *         representing the logged event history for a given game from the
	 *         perspective of a given player.
	 * @throws IOException
	 *             If an error occurs while reading one of the provided event
	 *             log file paths.
	 */
	public Table<String, String, GameHistory> createPlayerGameHistoryTable(
			final Collection<Entry<String, Path>> playerEventLogFilePaths, final int expectedUniqueGameCount,
			final Predicate<? super Event> eventFilter) throws IOException {
		final Table<String, String, GameHistory> result = HashBasedTable.create(playerEventLogFilePaths.size(),
				expectedUniqueGameCount);
		putPlayerGameHistories(result, playerEventLogFilePaths, eventFilter);
		return result;
	}

	/**
	 *
	 * @param playerEventLogFilePaths
	 *            A mapping of log {@link Path paths} to read for each player
	 *            ID.
	 * @param eventFilter
	 *            A positive (i.e.&nbsp;whitelisting) filter for the
	 *            {@link Event events} to include.
	 * @return A new {@link Table}, with game ID as the row key and player ID as
	 *         the column key, mapping to the relevant {@link GameHistory}
	 *         representing the logged event history for a given game from the
	 *         perspective of a given player.
	 * @throws IOException
	 *             If an error occurs while reading one of the provided event
	 *             log file paths.
	 */
	public Table<String, String, GameHistory> createPlayerGameHistoryTable(
			final Collection<Entry<String, Path>> playerEventLogFilePaths, final Predicate<? super Event> eventFilter)
			throws IOException {
		final Table<String, String, GameHistory> result = HashBasedTable.create();
		putPlayerGameHistories(result, playerEventLogFilePaths, eventFilter);
		return result;
	}

	/**
	 *
	 * @param lines
	 *            The logged events to parse, one on each line.
	 * @return A new {@link Map} of game IDs to their respective
	 *         {@link GameHistory histories}.
	 */
	public Map<String, GameHistory> parseGameHistories(final Stream<String> lines) {
		return parseGameHistories(lines, DEFAULT_EVENT_FILTER);
	}

	/**
	 *
	 * @param lines
	 *            The logged events to parse, one on each line.
	 * @param eventFilter
	 *            A positive (i.e.&nbsp;whitelisting) filter for the
	 *            {@link Event events} to include.
	 * @return A new {@link Map} of game IDs to their respective
	 *         {@link GameHistory histories}.
	 */
	public Map<String, GameHistory> parseGameHistories(final Stream<String> lines,
			final Predicate<? super Event> eventFilter) {
		final Stream<Event> loggedEvents = parseLoggedEvents(lines).filter(eventFilter);
		return createGameHistoryMap(loggedEvents);
	}

	/**
	 *
	 * @param eventLogPath
	 *            A {@link Path} to the logged events to parse, one on each
	 *            line.
	 * @return A new {@link Map} of game IDs to their respective
	 *         {@link GameHistory histories}.
	 * @throws IOException
	 *             If an I/O error occurs while opening the file.
	 */
	public Map<String, GameHistory> readGameHistories(final Path eventLogPath) throws IOException {
		return readGameHistories(eventLogPath, DEFAULT_EVENT_FILTER);
	}

	/**
	 *
	 * @param eventLogPath
	 *            A {@link Path} to the logged events to parse, one on each
	 *            line.
	 * @param eventFilter
	 *            A positive (i.e.&nbsp;whitelisting) filter for the
	 *            {@link Event events} to include.
	 * @return A new {@link Map} of game IDs to their respective
	 *         {@link GameHistory histories}.
	 * @throws IOException
	 *             IOException If an I/O error occurs while opening the file.
	 */
	public Map<String, GameHistory> readGameHistories(final Path eventLogPath,
			final Predicate<? super Event> eventFilter) throws IOException {
		try (final Stream<String> lines = readLines(eventLogPath)) {
			return parseGameHistories(lines, eventFilter);
		}
	}

	/**
	 *
	 * @param playerGameHistories
	 *            A {@link Table}, with game ID as the row key and player ID as
	 *            the column key, mapping to the relevant {@link GameHistory}
	 *            representing the logged event history for a given game from
	 *            the perspective of a given player.
	 * @param playerEventLogFilePaths
	 *            A mapping of log {@link Path paths} to read for each player
	 *            ID.
	 * @param eventFilter
	 *            A positive (i.e.&nbsp;whitelisting) filter for the
	 *            {@link Event events} to include.
	 * @throws IOException
	 *             If an error occurs while reading one of the provided event
	 *             log file paths.
	 */
	private void putPlayerGameHistories(final Table<String, String, GameHistory> playerGameHistories,
			final Collection<Entry<String, Path>> playerEventLogFilePaths, final Predicate<? super Event> eventFilter)
			throws IOException {
		for (final Entry<String, Path> playerEventLogFilePath : playerEventLogFilePaths) {
			final String playerId = playerEventLogFilePath.getKey();
			LOGGER.info("Reading session event log for player \"{}\".", playerId);
			final Path eventLogFile = playerEventLogFilePath.getValue();
			try (final Stream<String> lines = readLines(eventLogFile)) {
				final Map<String, GameHistory> gameHistories = parseGameHistories(lines, eventFilter);
				gameHistories.forEach((gameId, history) -> {
					playerGameHistories.put(gameId, playerId, history);
				});
			}
		}

		final Map<String, Map<String, GameHistory>> playerHistoriesByGameId = playerGameHistories.rowMap();
		playerHistoriesByGameId.values().stream().map(Map::values).map(Collection::stream).forEach(gameHistories -> {
			// For each set of player game histories for each game ID, ensure
			// that the initial states are all equivalent
			GameStateDescriptions
					.findAnyEquivalentGameState(gameHistories.map(GameHistory::getInitialState).iterator());
		});
	}

}
