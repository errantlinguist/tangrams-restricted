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
package se.kth.speech.coin.tangrams.game;

import java.io.Serializable;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 8 Dec 2016
 *
 */
public final class PlayerJoinTime implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 8542712145015317921L;

	private final long joinTime;

	private final String playerId;

	/**
	 *
	 */
	public PlayerJoinTime(final String playerId, final long joinTime) {
		this.playerId = playerId;
		this.joinTime = joinTime;
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
		if (!(obj instanceof PlayerJoinTime)) {
			return false;
		}
		final PlayerJoinTime other = (PlayerJoinTime) obj;
		if (joinTime != other.joinTime) {
			return false;
		}
		if (playerId == null) {
			if (other.playerId != null) {
				return false;
			}
		} else if (!playerId.equals(other.playerId)) {
			return false;
		}
		return true;
	}

	/**
	 * @return the joinTime
	 */
	public long getJoinTime() {
		return joinTime;
	}

	/**
	 * @return the playerId
	 */
	public String getPlayerId() {
		return playerId;
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
		result = prime * result + (int) (joinTime ^ joinTime >>> 32);
		result = prime * result + (playerId == null ? 0 : playerId.hashCode());
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
		builder.append("PlayerJoinTime [playerId=");
		builder.append(playerId);
		builder.append(", joinTime=");
		builder.append(joinTime);
		builder.append("]");
		return builder.toString();
	}

}
