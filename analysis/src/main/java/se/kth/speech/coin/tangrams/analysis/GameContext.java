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
package se.kth.speech.coin.tangrams.analysis;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.Collections;
import java.util.EnumMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.Optional;
import java.util.OptionalInt;
import java.util.function.Predicate;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Lists;

import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;
import se.kth.speech.coin.tangrams.game.GameStateDescription;
import se.kth.speech.coin.tangrams.game.Move;
import se.kth.speech.coin.tangrams.iristk.EventTypeMatcher;
import se.kth.speech.coin.tangrams.iristk.GameEvent;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;

public final class GameContext {

	private static final Map<GameManagementEvent, EventTypeMatcher> EVENT_TYPE_MATCHERS = createEventTypeMatcherMap();

	private static final Logger LOGGER = LoggerFactory.getLogger(GameContext.class);

	private static Map<GameManagementEvent, EventTypeMatcher> createEventTypeMatcherMap() {
		final Map<GameManagementEvent, EventTypeMatcher> result = new EnumMap<>(GameManagementEvent.class);
		Arrays.stream(GameManagementEvent.values())
				.forEach(eventType -> result.put(eventType, new EventTypeMatcher(eventType)));
		return result;
	}

	private static <E> OptionalInt findFirstMatchingDistance(final Stream<E> elems,
			final Predicate<? super E> matcher) {
		OptionalInt result = OptionalInt.empty();
		int currentDist = 0;
		for (final Iterator<E> elemIter = elems.iterator(); elemIter.hasNext();) {
			final E elem = elemIter.next();
			if (matcher.test(elem)) {
				LOGGER.debug("Found matching element: {}", elem);
				result = OptionalInt.of(currentDist);
				break;
			}
			currentDist++;
		}
		return result;
	}

	private static Stream<GameEvent> getEventsDescendingOrder(final NavigableMap<?, ? extends List<GameEvent>> map) {
		return map.descendingMap().values().stream().map(Lists::reverse).flatMap(List::stream);
	}

	private final GameHistory history;

	private final LocalDateTime time;

	GameContext(final GameHistory history, final LocalDateTime time) {
		this.history = history;
		this.time = time;
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
		if (!(obj instanceof GameContext)) {
			return false;
		}
		final GameContext other = (GameContext) obj;
		if (history == null) {
			if (other.history != null) {
				return false;
			}
		} else if (!history.equals(other.history)) {
			return false;
		}
		if (time == null) {
			if (other.time != null) {
				return false;
			}
		} else if (!time.equals(other.time)) {
			return false;
		}
		return true;
	}

	/**
	 * Finds the last {@link GameEvent} representing a given
	 * {@link GameManagementEvent} in chronological order, i.e.&nbsp;the
	 * &ldquo;last&rdquo; matching event preceding the time in the game
	 * represented by this {@link GameContext} instance.
	 *
	 * @param eventType
	 *            The {@link GameManagementEvent} to match.
	 * @return An {@link Optional} containing the last matching event, if any.
	 */
	public Optional<GameEvent> findLastEvent(final GameManagementEvent eventType) {
		final EventTypeMatcher matcher = EVENT_TYPE_MATCHERS.get(eventType);
		return findLastEvent(matcher);
	}

	/**
	 * Finds the last {@link GameEvent} matching a given {@link Predicate} in
	 * chronological order, i.e.&nbsp;the &ldquo;last&rdquo; matching event
	 * preceding the time in the game represented by this {@link GameContext}
	 * instance.
	 *
	 * @param matcher
	 *            A {@link Predicate} matching the event to find.
	 * @return An {@link Optional} containing the last matching event, if any.
	 */
	public Optional<GameEvent> findLastEvent(final Predicate<? super GameEvent> matcher) {
		final NavigableMap<LocalDateTime, List<GameEvent>> timedEvents = getPrecedingEvents();
		// Look for the last matching event (iterating
		// backwards)
		final Stream<GameEvent> eventsDescTime = getEventsDescendingOrder(timedEvents);
		return eventsDescTime.filter(matcher).findFirst();
	}

	/**
	 * Finds the last {@link GameEvent} representing a given
	 * {@link GameManagementEvent} in chronological order, i.e.&nbsp;the
	 * &ldquo;last&rdquo; matching event preceding the time in the game
	 * represented by this {@link GameContext} instance.
	 *
	 * @param eventType
	 *            The {@link GameManagementEvent} to match.
	 * @return An {@link Optional} containing the last matching event, if any.
	 */
	public OptionalInt findLastEventDistance(final GameManagementEvent eventType) {
		final EventTypeMatcher matcher = EVENT_TYPE_MATCHERS.get(eventType);
		return findLastEventDistance(matcher);
	}

	/**
	 * Finds the distance to the last {@link GameEvent} matching a given
	 * {@link Predicate} in chronological order, i.e.&nbsp;the
	 * &ldquo;last&rdquo; matching event preceding the time in the game
	 * represented by this {@link GameContext} instance; The distance is
	 * represented as the number of events between the last event and the
	 * matching event, i.e.&nbsp;<code>0</code> means that the last event
	 * submitted matches.
	 *
	 * @param matcher
	 *            A {@link Predicate} matching the event to find.
	 * @return An {@link OptionalInt} containing the distance to the last
	 *         matching event, if any.
	 */
	public OptionalInt findLastEventDistance(final Predicate<? super GameEvent> matcher) {
		final NavigableMap<LocalDateTime, List<GameEvent>> timedEvents = getPrecedingEvents();
		// Look for the last time the event was seen (iterating
		// backwards)
		final Stream<GameEvent> eventsDescTime = getEventsDescendingOrder(timedEvents);
		return findFirstMatchingDistance(eventsDescTime, matcher);
	}

	public OptionalInt findLastSelectedEntityId() {
		final GameManagementEvent.Attribute moveAttr = GameManagementEvent.Attribute.MOVE;
		// NOTE: This finds turn completion as well as next-turn submission
		// events
		final Optional<GameEvent> lastSelectionEvent = findLastEvent(
				event -> event.getGameAttrs().containsKey(moveAttr));
		final Optional<Move> lastSelectionEventMove = lastSelectionEvent
				.map(event -> (Move) event.getGameAttrs().get(moveAttr));
		return lastSelectionEventMove.isPresent() ? OptionalInt.of(lastSelectionEventMove.get().getPieceId())
				: OptionalInt.empty();
	}

	public List<ImageVisualizationInfo.Datum> getEntityVisualizationInfo() {
		final GameStateDescription initialState = history.getInitialState();
		return Collections.unmodifiableList(initialState.getImageVisualizationInfo().getData());
	}

	/**
	 * @return the history
	 */
	public GameHistory getHistory() {
		return history;
	}

	public Duration getOffset() {
		final LocalDateTime gameStartTime = history.getStartTime();
		return Duration.between(gameStartTime, time);
	}

	public NavigableMap<LocalDateTime, List<GameEvent>> getPrecedingEvents() {
		return history.getEvents().headMap(time, true);
	}

	public Stream<GameEvent> getPrecedingEventsDescendingOrder() {
		return getEventsDescendingOrder(getPrecedingEvents());
	}

	/**
	 * @return the time
	 */
	public LocalDateTime getTime() {
		return time;
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
		result = prime * result + (history == null ? 0 : history.hashCode());
		result = prime * result + (time == null ? 0 : time.hashCode());
		return result;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder(64 * history.getEvents().size() + 1);
		builder.append("GameContext [events=");
		builder.append(history);
		builder.append(", time=");
		builder.append(time);
		builder.append(']');
		return builder.toString();
	}
	
	public int getEntityCount() {
		return history.getEntityCount();
	}

}