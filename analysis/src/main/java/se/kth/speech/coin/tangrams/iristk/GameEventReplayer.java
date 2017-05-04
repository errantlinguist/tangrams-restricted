/*
 *  This file is part of se.kth.speech.coin.tangrams.playback.
 *
 *  se.kth.speech.coin.tangrams.playback is free software: you can redistribute it and/or modify
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
package se.kth.speech.coin.tangrams.iristk;

import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import iristk.system.Event;
import se.kth.speech.coin.tangrams.game.Controller;
import se.kth.speech.coin.tangrams.iristk.events.Move;
import se.kth.speech.coin.tangrams.iristk.events.Selection;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 27 Jan 2017
 *
 */
public final class GameEventReplayer {

	private static final Logger LOGGER = LoggerFactory.getLogger(GameEventReplayer.class);

	private static Event createUndoEvent(final Event event, final String time) {
		// NOTE: Record.deepClone() doesn't work for some reason; Need to
		// deep-copy manually!
		// final Event result = (Event) event.deepClone();
		final Event result = new Event(event.getName(), event);
		result.setId(event.getId() + "-UNDO");
		result.setSender(event.getSender());
		result.setTime(time);
		final String eventName = result.getName();
		final GameManagementEvent gameEventType = GameManagementEvent.getEventType(eventName);
		if (gameEventType == null) {
			LOGGER.info("No need to undo non-game event \"{}\" received at \"{}\".", eventName, event.getTime());
		} else {
			final String submittingPlayerId = event.getString(GameManagementEvent.Attribute.PLAYER_ID.toString());
			switch (gameEventType) {
			case COMPLETED_TURN_REQUEST: {
				LOGGER.debug("Received game event reporting a completed turn, submitted by \"{}\".",
						submittingPlayerId);
				final Move moveToUndo = (Move) event.get(GameManagementEvent.Attribute.MOVE.toString());
				final Move undoMove = createUndoMove(moveToUndo);
				break;
			}
			case NEXT_TURN_REQUEST: {
				LOGGER.debug("Received game event reporting the submission of a new move, submitted by \"{}\".",
						submittingPlayerId);
				final Move moveToUndo = (Move) event.get(GameManagementEvent.Attribute.MOVE.toString());
				final Move undoMove = createUndoMove(moveToUndo);
				break;
			}
			case SELECTION_REQUEST: {
				// The same event instance can be used for undoing because the
				// selection event signifies a selection "toggling"
				break;
			}
			default: {
				throw new IllegalArgumentException(
						String.format("Cannot create an undo version of event type \"%s\".", gameEventType));
			}
			}
		}

		return result;
	}

	private static Move createUndoMove(final Move moveToUndo) {
		final Move result = new Move(moveToUndo);
		result.setSource(moveToUndo.getTarget());
		result.setTarget(moveToUndo.getSource());
		return result;
	}

	private final Controller controller;

	private LocalDateTime currentTime;

	private int eventCount;

	private final ListIterator<Event> eventIter;

	private final List<Event> events;

	private final LocalDateTime initialTime;

	public GameEventReplayer(final List<Event> events, final LocalDateTime initialTime, final Controller controller) {
		if (EventTimes.parseEventTime(events.iterator().next().getTime()).isBefore(initialTime)) {
			throw new IllegalArgumentException(
					"The first event in the provided list has a timestamp before the provided initial time.");
		}
		this.events = events;
		eventIter = events.listIterator();
		this.initialTime = initialTime;
		this.controller = controller;
		currentTime = initialTime;
	}

	public Event applyNextEvent() {
		synchronized (eventIter) {
			final Event nextEvent = eventIter.next();
			applyEvent(nextEvent);
			return nextEvent;
		}
	}

	public List<Event> getAppliedEvents() {
		return events.subList(0, eventCount);
	}

	/**
	 * @return the currentTime
	 */
	public LocalDateTime getCurrentTime() {
		return currentTime;
	}

	public Event peekNextEvent() {
		return events.get(eventCount);
	}

	public Event peekPreviousEvent() {
		return events.get(eventCount - 1);
	}

	public Event undoLastEvent() {
		synchronized (eventIter) {
			final Event eventToUndo = eventIter.previous();
			final String precedingEventTimeStr = getPrecedingEventTime();
			undoEvent(eventToUndo, precedingEventTimeStr);
			return eventToUndo;
		}
	}

	/**
	 * @param startTime
	 * @return
	 */
	public synchronized List<Event> undoTo(final LocalDateTime startTime) {
		final List<Event> result = new ArrayList<>(eventCount);
		// Retreat in the replay history
		final int oldEventCount = eventCount;
		while (eventIter.hasPrevious()) {
			final Event previousEvent = eventIter.previous();
			final String previousEventTimeStr = previousEvent.getTime();
			final LocalDateTime previousEventTime = EventTimes.parseEventTime(previousEvent.getTime());
			if (previousEventTime.isBefore(startTime)) {
				// Put the iterator pointer back to where it was before
				eventIter.next();
				LOGGER.debug(
						"Found event with timestamp \"{}\", which is less than the supplied end time; Breaking out of iteration.",
						previousEventTimeStr);
				break;
			} else {
				result.add(previousEvent);
				// Update the event count BEFORE notifying the others
				// because it is necessary to get a list of them all at once
				// rather than notify once for each item
				eventCount--;
			}
		}
		final Iterator<String> precedingEventTimes;
		if (eventCount < 1) {
			final List<String> precedingEventTimeList = new ArrayList<>(result.size());
			precedingEventTimeList.add(initialTime.toString());
			precedingEventTimeList.addAll(events.subList(eventCount, oldEventCount - 1).stream().map(Event::getTime)
					.collect(Collectors.toList()));
			precedingEventTimes = precedingEventTimeList.iterator();
			if (!initialTime.toString().equals(precedingEventTimeList.iterator().next())) {
				throw new AssertionError();
			}
		} else {
			// Get the event before the earliest event to undo
			precedingEventTimes = events.subList(eventCount - 1, oldEventCount - 1).stream().map(Event::getTime)
					.collect(Collectors.toList()).iterator();

		}
		if (result.isEmpty()) {
			LOGGER.debug("No events to update for.");
		} else {
			final List<Event> undoEvents = result.stream().map(eventToUndo -> {
				final String precedingEventTime = precedingEventTimes.next();
				return createUndoEvent(eventToUndo, precedingEventTime);
			}).collect(Collectors.toList());
			if (!initialTime.toString().equals(undoEvents.listIterator().next().getTime())) {
				throw new AssertionError();
			}
			notifyEvents(undoEvents);
			final Event lastEvent = undoEvents.listIterator().next();
			currentTime = EventTimes.parseEventTime(lastEvent.getTime());
			LOGGER.debug("Set current time to \"{}\".", currentTime);
		}
		return result;
	}

	/**
	 * @param endTime
	 * @return
	 */
	public synchronized List<Event> updateTo(final LocalDateTime endTime) {
		final List<Event> result;
		// Advance in the replay history
		final int oldEventCount = eventCount;
		while (eventIter.hasNext()) {
			final Event nextEvent = eventIter.next();
			final String nextEventTimeStr = nextEvent.getTime();
			final LocalDateTime nextEventTime = EventTimes.parseEventTime(nextEvent.getTime());
			if (nextEventTime.isAfter(endTime)) {
				// Put the iterator pointer back to where it was before
				eventIter.previous();
				LOGGER.debug(
						"Found event with timestamp \"{}\", which is greater than the supplied end time; Breaking out of iteration.",
						nextEventTimeStr);
				break;
			} else {
				// Update the event count BEFORE notifying the others
				// because it is necessary to get a list of them all at once
				// rather than notify once for each item
				eventCount++;
			}
		}
		result = events.subList(oldEventCount, eventCount);
		if (result.isEmpty()) {
			LOGGER.debug("No events to update for.");
		} else {
			notifyEvents(result);
			final Event lastEvent = result.listIterator(result.size()).previous();
			currentTime = EventTimes.parseEventTime(lastEvent.getTime());
			LOGGER.debug("Set current time to \"{}\".", currentTime);
		}
		return result;
	}

	private void applyEvent(final Event nextEvent) {
		notifyEvents(Collections.singletonList(nextEvent));
		currentTime = EventTimes.parseEventTime(nextEvent.getTime());
		LOGGER.debug("Set current time to \"{}\".", currentTime);
		eventCount++;
	}

	private String getPrecedingEventTime() {
		final String result;
		final List<Event> appliedEvents = getAppliedEvents();
		if (appliedEvents.size() < 2) {
			result = initialTime.toString();
		} else {
			result = appliedEvents.listIterator(appliedEvents.size() - 1).previous().getTime();
		}
		return result;
	}

	private void notifyEvents(final List<Event> events) {
		for (final Event event : events) {
			final String eventName = event.getName();
			final GameManagementEvent gameEventType = GameManagementEvent.getEventType(eventName);
			if (gameEventType == null) {
				LOGGER.info("Ignoring non-game event \"{}\" received at \"{}\".", eventName, event.getTime());
			} else {
				final String submittingPlayerId = event.getString(GameManagementEvent.Attribute.PLAYER_ID.toString());
				switch (gameEventType) {
				case COMPLETED_TURN_REQUEST: {
					LOGGER.debug("Received game event reporting a completed turn, submitted by \"{}\".",
							submittingPlayerId);
					final Move move = (Move) event.get(GameManagementEvent.Attribute.MOVE.toString());
					controller.notifyTurnComplete(submittingPlayerId, move);
					break;
				}
				case GAME_READY_RESPONSE: {
					LOGGER.debug("Ignoring received game event type \"{}\".", gameEventType);
					break;
				}
				case NEXT_TURN_REQUEST: {
					LOGGER.debug("Received game event reporting the submission of a new move, submitted by \"{}\".",
							submittingPlayerId);
					final Move move = (Move) event.get(GameManagementEvent.Attribute.MOVE.toString());
					controller.notifyNextMove(submittingPlayerId, move);
					break;
				}
				case PLAYER_JOIN_RESPONSE: {
					final String joinTime = event.getString(GameManagementEvent.Attribute.TIMESTAMP.toString());
					LOGGER.debug("Received game event reporting that \"{}\" has joined the game at {}.",
							submittingPlayerId, joinTime);
					if (controller == null) {
						LOGGER.debug("Game controller not yet set; Not notifying controller of joined player.");
					} else {
						controller.notifyPlayerJoined(submittingPlayerId, Timestamp.valueOf(joinTime).getTime());
					}
					break;
				}
				case SELECTION_REJECTION: {
					LOGGER.debug("Received game event reporting that \"{}\" rejected a selection.", submittingPlayerId);
					final Selection selection = (Selection) event
							.get(GameManagementEvent.Attribute.SELECTION.toString());
					controller.notifySelectionRejected(submittingPlayerId, selection);
					break;
				}
				case SELECTION_REQUEST: {
					LOGGER.debug("Received game event reporting selection info for \"{}\".", submittingPlayerId);
					final Selection selection = (Selection) event
							.get(GameManagementEvent.Attribute.SELECTION.toString());
					controller.notifyPlayerSelection(submittingPlayerId, selection);
					break;
				}
				default: {
					throw new IllegalArgumentException(
							String.format("Cannot create an undo version of event type \"%s\".", gameEventType));
				}
				}
			}

		}
	}

	private void undoEvent(final Event eventToUndo, final String precedingEventTime) {
		final Event undoEvent = createUndoEvent(eventToUndo, precedingEventTime);
		notifyEvents(Collections.singletonList(undoEvent));
		currentTime = EventTimes.parseEventTime(undoEvent.getTime());
		LOGGER.debug("Set current time to \"{}\".", currentTime);
		eventCount--;
	}

}
