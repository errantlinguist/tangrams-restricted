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
import java.util.EnumSet;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import iristk.system.Event;
import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.coin.tangrams.iristk.EventTypeMatcher;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;
import se.kth.speech.coin.tangrams.iristk.io.LoggedEventReader;

public final class SessionGame {

	private static final Set<GameManagementEvent> DEFAULT_EVENT_DIAG_DELIMITERS = EnumSet
			.of(GameManagementEvent.NEXT_TURN_REQUEST);

	private static final Logger LOGGER = LoggerFactory.getLogger(SessionGame.class);

	/**
	 *
	 * @param eventLogPath
	 *            The {@link Path} to read the event log from.
	 * @param utts
	 *            The {@link Utterance utterances} for the session.
	 * @param evtDiagFactory
	 *            The {@link BiFunction} used to create a sequence of individual
	 *            event dialogues from a sequence of individual utterances
	 *            combined with the history of the game session in which these
	 *            utterances were made.
	 * @return A new {@link SessionGame} representing the session game.
	 * @throws IOException
	 *             If an I/O error occurs while reading the event log.
	 */
	static SessionGame create(final Path eventLogPath, final List<Utterance> utts,
			final BiFunction<? super ListIterator<Utterance>, ? super GameHistory, Stream<EventDialogue>> evtDiagFactory,
			final LoggedEventReader eventReader) throws IOException {
		LOGGER.info("Reading game histories from \"{}\".", eventLogPath);
		try (Stream<Event> eventStream = LoggedEventReader.readLoggedEvents(eventLogPath)) {
			final List<Event> events = Arrays.asList(eventStream.toArray(Event[]::new));
			return new SessionGame(events, utts, evtDiagFactory, eventReader);
		}
	}

	/**
	 *
	 * @param eventLogPath
	 *            The {@link Path} to read the event log from.
	 * @param utts
	 *            The {@link Utterance utterances
	 * @param eventDiagDelimiters
	 *            A {@link Collection} of the {@link GameManagementEvent}
	 *            instances which delimit individual {@link EventDialogue event
	 *            dialogues} during the session.
	 * @return A new {@link SessionGame} representing the session game.
	 * @throws IOException
	 *             If an I/O error occurs while reading the event log.
	 */
	static SessionGame create(final Path eventLogPath, final List<Utterance> utts,
			final Collection<GameManagementEvent> eventDiagDelimiters, final LoggedEventReader eventReader)
			throws IOException {
		return create(eventLogPath, utts, new EventDialogueFactory(new EventTypeMatcher(eventDiagDelimiters)),
				eventReader);
	}

	/**
	 *
	 * @param eventLogPath
	 *            The {@link Path} to read the event log from.
	 * @param utts
	 *            The {@link Utterance utterances} for the session.
	 * @throws IOException
	 *             If an I/O error occurs while reading the event log.
	 */
	static SessionGame create(final Path eventLogPath, final List<Utterance> utts, final LoggedEventReader eventReader)
			throws IOException {
		return create(eventLogPath, utts, DEFAULT_EVENT_DIAG_DELIMITERS, eventReader);
	}

	private final List<EventDialogue> eventDialogues;

	private final String gameId;

	private final GameHistory history;

	/**
	 *
	 * @param events
	 *            The {@link Event events} which occurred during the game
	 *            session.
	 * @param utts
	 *            The {@link Utterance utterances} for the session.
	 * @param evtDiagFactory
	 *            The {@link BiFunction} used to create a sequence of individual
	 *            event dialogues from a sequence of individual utterances
	 *            combined with the history of the game session in which these
	 *            utterances were made.
	 */
	public SessionGame(final List<Event> events, final List<Utterance> utts,
			final BiFunction<? super ListIterator<Utterance>, ? super GameHistory, Stream<EventDialogue>> evtDiagFactory,
			final LoggedEventReader eventReader) {
		final Map<String, GameHistory> gameHistories = eventReader.createGameHistoryMap(events.stream());
		final Entry<String, GameHistory> gameIdHistory = GameHistory.ensureSingleGame(gameHistories);
		gameId = gameIdHistory.getKey();
		LOGGER.debug("Creating {} instance for game ID \"{}\".", SessionGame.class.getSimpleName(), gameId);
		history = gameIdHistory.getValue();
		eventDialogues = Collections.unmodifiableList(
				Arrays.asList(evtDiagFactory.apply(utts.listIterator(), history).toArray(EventDialogue[]::new)));
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(final Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (!(obj instanceof SessionGame)) {
			return false;
		}
		final SessionGame other = (SessionGame) obj;
		if (eventDialogues == null) {
			if (other.eventDialogues != null) {
				return false;
			}
		} else if (!eventDialogues.equals(other.eventDialogues)) {
			return false;
		}
		if (gameId == null) {
			if (other.gameId != null) {
				return false;
			}
		} else if (!gameId.equals(other.gameId)) {
			return false;
		}
		if (history == null) {
			if (other.history != null) {
				return false;
			}
		} else if (!history.equals(other.history)) {
			return false;
		}
		return true;
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

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (eventDialogues == null ? 0 : eventDialogues.hashCode());
		result = prime * result + (gameId == null ? 0 : gameId.hashCode());
		result = prime * result + (history == null ? 0 : history.hashCode());
		return result;
	}
}