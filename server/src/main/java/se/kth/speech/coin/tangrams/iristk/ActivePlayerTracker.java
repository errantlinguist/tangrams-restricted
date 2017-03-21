/*
 *  This file is part of tangrams.
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
package se.kth.speech.coin.tangrams.iristk;

import java.util.Collections;
import java.util.Comparator;
import java.util.NavigableSet;
import java.util.TreeSet;

import se.kth.speech.CyclingIterator;
import se.kth.speech.coin.tangrams.game.PlayerJoinTime;

final class SelectingPlayerTracker {

	private static final Comparator<PlayerJoinTime> TIMESTAMP_COMPARATOR = Comparator
			.comparing(PlayerJoinTime::getJoinTime).thenComparing(PlayerJoinTime::getPlayerId);

	private String activePlayerId;

	private final NavigableSet<PlayerJoinTime> joinedPlayers;

	private final CyclingIterator<PlayerJoinTime> nextSelectingPlayerIter;

	public SelectingPlayerTracker() {
		this(new TreeSet<>(TIMESTAMP_COMPARATOR));
	}

	private SelectingPlayerTracker(final NavigableSet<PlayerJoinTime> joinedPlayers) {
		this.joinedPlayers = joinedPlayers;
		nextSelectingPlayerIter = new CyclingIterator<>(joinedPlayers);
	}

	public boolean addPlayer(final PlayerJoinTime joinedPlayer) {
		final boolean result;
		synchronized (nextSelectingPlayerIter) {
			// Add through the iterator in order to ensure that it is in the
			// right
			// position
			result = nextSelectingPlayerIter.add(joinedPlayer);
		}
		return result;
	}

	public String cycleSelectingPlayer() {
		final String result;
		synchronized (nextSelectingPlayerIter) {
			activePlayerId = nextSelectingPlayerIter.next().getPlayerId();
			result = activePlayerId;
		}
		return result;
	}

	public String getSelectingPlayerId() {
		return activePlayerId;
	}

	/**
	 * @return the joinedPlayers
	 */
	public NavigableSet<PlayerJoinTime> getJoinedPlayers() {
		return Collections.unmodifiableNavigableSet(joinedPlayers);
	}

	public int getPlayerCount() {
		return joinedPlayers.size();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder();
		builder.append("SelectingPlayerTracker [activePlayerId=");
		builder.append(activePlayerId);
		builder.append(", joinedPlayers=");
		builder.append(joinedPlayers);
		builder.append("]");
		return builder.toString();
	}

}