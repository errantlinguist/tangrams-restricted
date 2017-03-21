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

import java.util.Random;

import com.google.common.collect.BiMap;

import se.kth.speech.coin.tangrams.game.PlayerRole;
import se.kth.speech.coin.tangrams.game.RemoteController;

public final class GameState {

	private final boolean allowFailedPlacements;

	private final LocalController localController;

	private final double occupiedGridArea;

	private final BiMap<PlayerRole, String> playerRoles;

	private final RemoteController<Integer> remoteController;

	private final Random rnd;

	GameState(final LocalController localController, final RemoteController<Integer> remoteController,
			final BiMap<PlayerRole, String> playerRoles, final Random rnd, final double occupiedGridArea,
			final boolean allowFailedPlacements) {
		this.localController = localController;
		this.remoteController = remoteController;
		this.playerRoles = playerRoles;
		this.rnd = rnd;
		this.occupiedGridArea = occupiedGridArea;
		this.allowFailedPlacements = allowFailedPlacements;
	}

	/**
	 * @return the allowFailedPlacements
	 */
	public boolean allowFailedPlacements() {
		return allowFailedPlacements;
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
		if (!(obj instanceof GameState)) {
			return false;
		}
		final GameState other = (GameState) obj;
		if (allowFailedPlacements != other.allowFailedPlacements) {
			return false;
		}
		if (localController == null) {
			if (other.localController != null) {
				return false;
			}
		} else if (!localController.equals(other.localController)) {
			return false;
		}
		if (Double.doubleToLongBits(occupiedGridArea) != Double.doubleToLongBits(other.occupiedGridArea)) {
			return false;
		}
		if (playerRoles == null) {
			if (other.playerRoles != null) {
				return false;
			}
		} else if (!playerRoles.equals(other.playerRoles)) {
			return false;
		}
		if (remoteController == null) {
			if (other.remoteController != null) {
				return false;
			}
		} else if (!remoteController.equals(other.remoteController)) {
			return false;
		}
		if (rnd == null) {
			if (other.rnd != null) {
				return false;
			}
		} else if (!rnd.equals(other.rnd)) {
			return false;
		}
		return true;
	}

	/**
	 * @return the localController
	 */
	public LocalController getLocalController() {
		return localController;
	}

	/**
	 * @return the occupiedGridArea
	 */
	public double getOccupiedGridArea() {
		return occupiedGridArea;
	}

	/**
	 * @return the playerRoles
	 */
	public BiMap<PlayerRole, String> getPlayerRoles() {
		return playerRoles;
	}

	/**
	 * @return the remoteController
	 */
	public RemoteController<Integer> getRemoteController() {
		return remoteController;
	}

	/**
	 * @return the rnd
	 */
	public Random getRnd() {
		return rnd;
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
		result = prime * result + (allowFailedPlacements ? 1231 : 1237);
		result = prime * result + (localController == null ? 0 : localController.hashCode());
		long temp;
		temp = Double.doubleToLongBits(occupiedGridArea);
		result = prime * result + (int) (temp ^ temp >>> 32);
		result = prime * result + (playerRoles == null ? 0 : playerRoles.hashCode());
		result = prime * result + (remoteController == null ? 0 : remoteController.hashCode());
		result = prime * result + (rnd == null ? 0 : rnd.hashCode());
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
		builder.append("GameState [allowFailedPlacements=");
		builder.append(allowFailedPlacements);
		builder.append(", localController=");
		builder.append(localController);
		builder.append(", occupiedGridArea=");
		builder.append(occupiedGridArea);
		builder.append(", playerRoles=");
		builder.append(playerRoles);
		builder.append(", remoteController=");
		builder.append(remoteController);
		builder.append(", rnd=");
		builder.append(rnd);
		builder.append("]");
		return builder.toString();
	}

}