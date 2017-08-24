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
package se.kth.speech.coin.tangrams.analysis;

import java.time.LocalDateTime;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NavigableMap;
import java.util.Set;
import java.util.TreeMap;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Lists;

import iristk.system.Event;
import se.kth.speech.coin.tangrams.iristk.events.GameStateDescription;

public final class GameHistory {

	private static final Logger LOGGER = LoggerFactory.getLogger(GameHistory.class);

	private static Stream<Event> getEventsDescendingOrder(final NavigableMap<?, ? extends List<Event>> map) {
		return map.descendingMap().values().stream().map(Lists::reverse).flatMap(List::stream);
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
	static Entry<String, GameHistory> ensureSingleGame(final Map<String, GameHistory> gameHistories) {
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

	private final NavigableMap<LocalDateTime, List<Event>> events = new TreeMap<>();

	private final GameStateDescription initialState;

	private final LocalDateTime startTime;

	GameHistory(final GameStateDescription initialState, final LocalDateTime startTime) {
		this.initialState = initialState;
		this.startTime = startTime;
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
		if (!(obj instanceof GameHistory)) {
			return false;
		}
		final GameHistory other = (GameHistory) obj;
		if (events == null) {
			if (other.events != null) {
				return false;
			}
		} else if (!events.equals(other.events)) {
			return false;
		}
		if (initialState == null) {
			if (other.initialState != null) {
				return false;
			}
		} else if (!initialState.equals(other.initialState)) {
			return false;
		}
		if (startTime == null) {
			if (other.startTime != null) {
				return false;
			}
		} else if (!startTime.equals(other.startTime)) {
			return false;
		}
		return true;
	}

	public int getEntityCount() {
		return initialState.getImageVisualizationInfoDescription().getData().size();
	}

	/**
	 * @return the events
	 */
	public NavigableMap<LocalDateTime, List<Event>> getEvents() {
		return Collections.unmodifiableNavigableMap(events);
	}

	/**
	 *
	 * @return A {@link Stream} of all {@link Event events}, in the sequence
	 *         they occurred in the game.
	 */
	public Stream<Event> getEventSequence() {
		return getEvents().values().stream().flatMap(List::stream);
	}

	/**
	 *
	 * @return A {@link Stream} of all {@link Event events}, in descending order
	 *         according to the time they occurred, i.e.&nbsp; The last event
	 *         appears first in the returned {@code Stream}.
	 */
	public Stream<Event> getEventSequenceDescendingOrder() {
		return getEventsDescendingOrder(getEvents());
	}

	/**
	 * @return the initialState
	 */
	public GameStateDescription getInitialState() {
		return initialState;
	}

	public Set<String> getPlayerIds() {
		return Collections.unmodifiableSet(initialState.getPlayerRoles().values());
	}

	/**
	 * @return the startTime
	 */
	public LocalDateTime getStartTime() {
		return startTime;
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
		result = prime * result + (events == null ? 0 : events.hashCode());
		result = prime * result + (initialState == null ? 0 : initialState.hashCode());
		result = prime * result + (startTime == null ? 0 : startTime.hashCode());
		return result;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder(64 * events.size() + 1);
		builder.append("GameHistory [startTime=");
		builder.append(startTime);
		builder.append(", initialState=");
		builder.append(initialState);
		builder.append(", events=");
		builder.append(events);
		builder.append(']');
		return builder.toString();
	}

	/**
	 * @return the events
	 */
	NavigableMap<LocalDateTime, List<Event>> getEventsMutable() {
		return events;
	}
}