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
import se.kth.speech.coin.tangrams.iristk.io.LoggedEvents;

public final class SessionGame {

	private static final Set<GameManagementEvent> DEFAULT_EVENT_DIAG_DELIMITERS = EnumSet
			.of(GameManagementEvent.NEXT_TURN_REQUEST);

	private static final Logger LOGGER = LoggerFactory.getLogger(SessionGame.class);

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

	/**
	 *
	 * @param eventLogPath
	 *            The {@link Path} to read the event log from.
	 * @param utts
	 *            The {@link Utterance utterances} for the session.
	 * @throws IOException
	 *             If an I/O error occurs while reading the event log.
	 */
	static SessionGame create(final Path eventLogPath, final List<Utterance> utts) throws IOException {
		return create(eventLogPath, utts, DEFAULT_EVENT_DIAG_DELIMITERS);
	}

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
			final BiFunction<? super ListIterator<Utterance>, ? super GameHistory, Stream<EventDialogue>> evtDiagFactory)
			throws IOException {
		LOGGER.info("Reading game histories from \"{}\".", eventLogPath);
		try (Stream<Event> eventStream = LoggedEvents.readLoggedEvents(eventLogPath)) {
			final List<Event> events = Arrays.asList(eventStream.toArray(Event[]::new));
			return new SessionGame(events, utts, evtDiagFactory);
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
			final Collection<GameManagementEvent> eventDiagDelimiters) throws IOException {
		return create(eventLogPath, utts, new EventDialogueFactory(new EventTypeMatcher(eventDiagDelimiters)));
	}

	private final List<EventDialogue> eventDialogues;

	private final List<Event> events;

	private final String gameId;

	private final GameHistory history;

	private final List<Utterance> utts;

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
			final BiFunction<? super ListIterator<Utterance>, ? super GameHistory, Stream<EventDialogue>> evtDiagFactory) {
		this.events = Collections.unmodifiableList(events);
		this.utts = Collections.unmodifiableList(utts);
		final Map<String, GameHistory> gameHistories = LoggedEvents.createGameHistoryMap(events.stream());
		final Entry<String, GameHistory> gameIdHistory = ensureSingleGame(gameHistories);
		gameId = gameIdHistory.getKey();
		LOGGER.debug("Creating {} instance for game ID \"{}\".", SessionGame.class.getSimpleName(), gameId);
		history = gameIdHistory.getValue();
		eventDialogues = Collections.unmodifiableList(
				Arrays.asList(evtDiagFactory.apply(utts.listIterator(), history).toArray(EventDialogue[]::new)));
	}

	public List<EventDialogue> getEventDialogues() {
		return eventDialogues;
	}

	/**
	 * @return the events
	 */
	public List<Event> getEvents() {
		return events;
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

	/**
	 * @return the utts
	 */
	public List<Utterance> getUtterances() {
		return utts;
	}
}