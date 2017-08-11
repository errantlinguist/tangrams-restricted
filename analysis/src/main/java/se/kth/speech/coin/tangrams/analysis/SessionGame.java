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
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Maps;

import iristk.system.Event;
import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.coin.tangrams.iristk.EventTypeMatcher;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;
import se.kth.speech.coin.tangrams.iristk.io.LoggedEvents;

public final class SessionGame {

	/**
	 * This is used to create a sequence of individual event dialogues from a
	 * sequence of individual utterances combined with the history of the game
	 * session in which these utterances were made.
	 */
	private static final EventDialogueFactory EVT_DIAG_FACTORY = new EventDialogueFactory(
			new EventTypeMatcher(GameManagementEvent.NEXT_TURN_REQUEST));

	private static final Logger LOGGER = LoggerFactory.getLogger(SessionGame.class);

	private static SessionGame createGame(final Stream<Event> eventStream, final List<Utterance> utts)
			throws IOException {
		final Map<String, GameHistory> gameHistories = LoggedEvents.createGameHistoryMap(eventStream);
		final Entry<String, GameHistory> gameIdHistory = ensureSingleGame(gameHistories);
		final String gameId = gameIdHistory.getKey();
		LOGGER.debug("Creating {} instance for game ID \"{}\".", SessionGame.class.getSimpleName(), gameId);
		final GameHistory history = gameIdHistory.getValue();
		return new SessionGame(gameId, history, Collections.unmodifiableList(
				Arrays.asList(EVT_DIAG_FACTORY.apply(utts.listIterator(), history).toArray(EventDialogue[]::new))));
	}

	/**
	 * <strong>TODO:</strong> Support multiple games in one session
	 *
	 * @param gameHistories
	 *            The {@link Map} to check for exactly one {@link GameHistory}
	 *            instance.
	 * @return The single entry.
	 * @throws IllegalArgumentException
	 *             If there is not exactly one entry in the map.
	 */
	private static Entry<String, GameHistory> ensureSingleGame(final Map<String, GameHistory> gameHistories) {
		final Entry<String, GameHistory> result;
		final int gameCount = gameHistories.size();
		switch (gameCount) {
		case 0: {
			throw new IllegalArgumentException(String.format("Event log contains no games."));
		}
		case 1: {
			result = gameHistories.entrySet().iterator().next();
			LOGGER.debug("Created history for game \"{}\".", result.getKey());
			break;
		}
		default: {
			throw new IllegalArgumentException(
					String.format("Event log contains multiple games; Not (currently) supported."));
		}
		}
		return result;
	}

	static SessionGame createGame(final Path eventLogPath, final List<Utterance> utts) throws IOException {
		LOGGER.info("Reading game histories from \"{}\".", eventLogPath);
		try (Stream<Event> eventStream = LoggedEvents.readLoggedEvents(eventLogPath)) {
			return createGame(eventStream, utts);
		}
	}

	static Map<String, SessionGame> createPlayerPerspectiveGameMap(
			final Collection<Entry<String, Path>> playerEventLogPaths, final List<Utterance> utts) throws IOException {
		final Map<String, SessionGame> result = Maps.newHashMapWithExpectedSize(playerEventLogPaths.size());
		for (final Entry<String, Path> playerEventLogPath : playerEventLogPaths) {
			final String playerId = playerEventLogPath.getKey();
			final Path eventLogPath = playerEventLogPath.getValue();
			final SessionGame sessionGame = createGame(eventLogPath, utts);
			result.put(playerId, sessionGame);
		}
		return result;
	}

	private final List<EventDialogue> eventDialogues;

	private final String gameId;

	private final GameHistory history;

	private SessionGame(final String gameId, final GameHistory history, final List<EventDialogue> eventDialogues) {
		this.gameId = gameId;
		this.history = history;
		this.eventDialogues = eventDialogues;
	}

	public List<EventDialogue> getEventDialogues() {
		return eventDialogues;
	}

	/**
	 * @return the gameId
	 */
	public String getGameId() {
		return gameId;
	}

	public GameHistory getHistory() {
		return history;
	}
}