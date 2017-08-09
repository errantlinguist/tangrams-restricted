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

import javax.xml.bind.JAXBException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Maps;

import se.kth.speech.coin.tangrams.iristk.EventTypeMatcher;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;
import se.kth.speech.coin.tangrams.iristk.io.LoggedEvents;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 12 May 2017
 *
 */
public final class SessionEventDialogueManager {

	public static final class SessionGame {

		private final String gameId;

		private final GameHistory history;

		private final List<EventDialogue> uttDialogues;

		private SessionGame(final String gameId, final GameHistory history, final List<EventDialogue> uttDialogues) {
			this.gameId = gameId;
			this.history = history;
			this.uttDialogues = uttDialogues;
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

		public List<EventDialogue> getUttDialogues() {
			return uttDialogues;
		}
	}

	/**
	 * This is used to create a sequence of individual event dialogues from a
	 * sequence of individual utterances combined with the history of the game
	 * session in which these utterances were made.
	 */
	private static final EventDialogueFactory EVT_DIAG_FACTORY = new EventDialogueFactory(
			new EventTypeMatcher(GameManagementEvent.NEXT_TURN_REQUEST));

	private static final Logger LOGGER = LoggerFactory.getLogger(SessionEventDialogueManager.class);

	private static SessionGame createGame(final Path eventLogPath, final List<Utterance> utts) throws IOException {
		final Entry<String, GameHistory> gameIdHistory = loadGameHistory(eventLogPath);
		final GameHistory history = gameIdHistory.getValue();
		return new SessionGame(gameIdHistory.getKey(), history, Collections.unmodifiableList(
				Arrays.asList(EVT_DIAG_FACTORY.apply(utts.listIterator(), history).toArray(EventDialogue[]::new))));
	}

	private static Map<String, SessionGame> createPlayerPerspectiveGameMap(
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

	private static Entry<String, GameHistory> loadGameHistory(final Path eventLogPath) throws IOException {
		Entry<String, GameHistory> result;
		LOGGER.info("Reading game histories from \"{}\".", eventLogPath);
		final Map<String, GameHistory> gameHistories = LoggedEvents.readGameHistories(eventLogPath,
				LoggedEvents.VALID_MODEL_MIN_REQUIRED_EVENT_MATCHER);
		final int gameCount = gameHistories.size();
		// TODO: Support multiple games in one session
		switch (gameCount) {
		case 0: {
			throw new IllegalArgumentException(String.format("Event log \"%s\" contains no games.", eventLogPath));
		}
		case 1: {
			result = gameHistories.entrySet().iterator().next();
			LOGGER.debug("Parsed history for game \"{}\".", result.getKey());
			break;
		}
		default: {
			throw new IllegalArgumentException(String
					.format("Event log \"%s\" contains multiple games; Not (currently) supported.", eventLogPath));
		}
		}
		return result;
	}

	private final SessionGame canonicalGame;

	private final Map<String, Path> playerEventLogs;

	private final List<Utterance> utts;

	SessionEventDialogueManager(final SessionDataManager sessionData) throws IOException, JAXBException {
		utts = SessionUtterances.createUtteranceList(sessionData);
		LOGGER.debug("Creating dialogues for {} annotated utterance(s).", utts.size());

		canonicalGame = createGame(sessionData.getCanonicalEventLogPath(), utts);
		playerEventLogs = sessionData.getPlayerData().getPlayerEventLogs();
	}

	public Map<String, SessionGame> createPlayerPerspectiveGameMap() throws IOException {
		return createPlayerPerspectiveGameMap(playerEventLogs.entrySet(), utts);
	}

	public SessionGame getCanonicalGame() {
		return canonicalGame;
	}

}
