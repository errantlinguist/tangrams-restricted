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

import java.sql.Timestamp;
import java.util.List;
import java.util.NavigableMap;
import java.util.Optional;
import java.util.function.Predicate;
import java.util.stream.Stream;

import com.google.common.collect.Lists;

import iristk.system.Event;

public final class GameContext {

	private static <V> Stream<V> getValuesDescendingOrder(final NavigableMap<?, ? extends List<? extends V>> map) {
		return map.descendingMap().values().stream().map(Lists::reverse).flatMap(List::stream);
	}

	private final GameHistory history;

	private final String playerId;

	private final Timestamp time;

	GameContext(final GameHistory history, final Timestamp time, final String playerId) {
		this.history = history;
		this.time = time;
		this.playerId = playerId;
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
		if (playerId == null) {
			if (other.playerId != null) {
				return false;
			}
		} else if (!playerId.equals(other.playerId)) {
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

	public Optional<Event> findLastEvent(final Predicate<? super Event> matcher) {
		final NavigableMap<Timestamp, List<Event>> timedEventsBeforeUtt = getPrecedingEvents();
		// Look for the last matching event (iterating
		// backwards)
		final Stream<Event> eventsDescTime = getValuesDescendingOrder(timedEventsBeforeUtt);
		return eventsDescTime.filter(matcher).findFirst();
	}

	/**
	 * @return the history
	 */
	public GameHistory getHistory() {
		return history;
	}

	/**
	 * @return the playerId
	 */
	public String getPlayerId() {
		return playerId;
	}

	public NavigableMap<Timestamp, List<Event>> getPrecedingEvents() {
		return history.getEvents().headMap(time, true);
	}

	/**
	 * @return the time
	 */
	public Timestamp getTime() {
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
		result = prime * result + (playerId == null ? 0 : playerId.hashCode());
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
		final StringBuilder builder = new StringBuilder();
		builder.append("GameContext [events=");
		builder.append(history);
		builder.append(", time=");
		builder.append(time);
		builder.append(", playerId=");
		builder.append(playerId);
		builder.append(']');
		return builder.toString();
	}
}