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
package se.kth.speech.coin.tangrams.iristk.events;

import iristk.util.Record;
import se.kth.speech.coin.tangrams.game.PlayerRole;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 12 Jan 2017
 *
 */
public final class PlayerRoleChange extends Record {

	private String playerId;

	@RecordField(name = "role")
	private String role;

	public PlayerRoleChange() {
		// Default constructor is required for JSON (un-)marshalling
	}

	/**
	 * @param playerIdsToUndo
	 */
	public PlayerRoleChange(final PlayerRoleChange copyee) {
		super(copyee);
	}

	public PlayerRoleChange(final String playerId,final PlayerRole role) {
		setPlayerId(playerId);
		setRole(role);
	}

	/**
	 * @return the playerId
	 */
	@RecordField(name = "playerId")
	public String getPlayerId() {
		return playerId;
	}

	/**
	 * @return the role
	 */
	public PlayerRole getRole() {
		return PlayerRole.valueOf(role);
	}

	/**
	 * @param playerId
	 *            the playerId to set
	 */
	@RecordField(name = "playerId")
	public void setPlayerId(final String playerId) {
		this.playerId = playerId;
	}

	/**
	 * @param role
	 *            the role to set
	 */
	public void setRole(final PlayerRole role) {
		this.role = role.toString();
	}

}
