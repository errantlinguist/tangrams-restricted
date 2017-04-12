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
package se.kth.speech.coin.tangrams.iristk;

import java.util.function.Predicate;

import iristk.system.Event;

public final class EventSubmittingPlayerMatcher implements Predicate<Event> {

	private final String playerId;

	public EventSubmittingPlayerMatcher(final String playerId) {
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
		if (!(obj instanceof EventSubmittingPlayerMatcher)) {
			return false;
		}
		final EventSubmittingPlayerMatcher other = (EventSubmittingPlayerMatcher) obj;
		if (playerId == null) {
			if (other.playerId != null) {
				return false;
			}
		} else if (!playerId.equals(other.playerId)) {
			return false;
		}
		return true;
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
		result = prime * result + (playerId == null ? 0 : playerId.hashCode());
		return result;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Predicate#test(java.lang.Object)
	 */
	@Override
	public boolean test(final Event event) {
		final String submittingPlayerId = event.getString(GameManagementEvent.Attribute.PLAYER_ID.toString());
		return playerId.equals(submittingPlayerId);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder();
		builder.append("EventSubmittingPlayerMatcher [playerId=");
		builder.append(playerId);
		builder.append("]");
		return builder.toString();
	}
}