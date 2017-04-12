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

import java.sql.Timestamp;
import java.util.Collections;
import java.util.List;
import java.util.NavigableMap;
import java.util.Set;
import java.util.TreeMap;

import iristk.system.Event;
import se.kth.speech.coin.tangrams.iristk.events.GameStateDescription;

public final class GameStateChangeData {

	private final NavigableMap<Timestamp, List<Event>> events = new TreeMap<>();

	private final GameStateDescription initialState;

	private final Timestamp startTime;

	GameStateChangeData(final GameStateDescription initialState, final Timestamp startTime) {
		this.initialState = initialState;
		this.startTime = startTime;
	}

	/**
	 * @return the events
	 */
	public NavigableMap<Timestamp, List<Event>> getEvents() {
		return events;
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
	public Timestamp getStartTime() {
		return startTime;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder();
		builder.append("GameStateChangeData [startTime=");
		builder.append(startTime);
		builder.append(", initialState=");
		builder.append(initialState);
		builder.append(", events=");
		builder.append(events);
		builder.append("]");
		return builder.toString();
	}
}