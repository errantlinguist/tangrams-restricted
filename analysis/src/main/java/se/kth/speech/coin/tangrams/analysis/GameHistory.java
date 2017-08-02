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
import java.util.NavigableMap;
import java.util.Set;
import java.util.TreeMap;
import java.util.stream.Stream;

import iristk.system.Event;
import se.kth.speech.coin.tangrams.iristk.events.GameStateDescription;

public final class GameHistory {

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