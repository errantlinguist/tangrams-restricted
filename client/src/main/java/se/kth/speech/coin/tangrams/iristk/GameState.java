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

import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;
import se.kth.speech.coin.tangrams.game.Controller;
import se.kth.speech.coin.tangrams.game.PlayerRole;

public final class GameState {

	private final boolean allowFailedPlacements;

	private final Controller controller;

	private final ImageVisualizationInfo imgVizInfo;

	private final double occupiedGridArea;

	private final BiMap<PlayerRole, String> playerRoles;

	private final Random rnd;

	GameState(final Controller controller, final ImageVisualizationInfo imgVizInfo,
			final BiMap<PlayerRole, String> playerRoles, final Random rnd, final double occupiedGridArea,
			final boolean allowFailedPlacements) {
		this.controller = controller;
		this.imgVizInfo = imgVizInfo;
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
		if (controller == null) {
			if (other.controller != null) {
				return false;
			}
		} else if (!controller.equals(other.controller)) {
			return false;
		}
		if (imgVizInfo == null) {
			if (other.imgVizInfo != null) {
				return false;
			}
		} else if (!imgVizInfo.equals(other.imgVizInfo)) {
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
	 * @return the controller
	 */
	public Controller getController() {
		return controller;
	}

	/**
	 * @return the imgVizInfo
	 */
	public ImageVisualizationInfo getImageVisualizationInfo() {
		return imgVizInfo;
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
		result = prime * result + (controller == null ? 0 : controller.hashCode());
		result = prime * result + (imgVizInfo == null ? 0 : imgVizInfo.hashCode());
		long temp;
		temp = Double.doubleToLongBits(occupiedGridArea);
		result = prime * result + (int) (temp ^ temp >>> 32);
		result = prime * result + (playerRoles == null ? 0 : playerRoles.hashCode());
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
		builder.append(", controller=");
		builder.append(controller);
		builder.append(", occupiedGridArea=");
		builder.append(occupiedGridArea);
		builder.append(", playerRoles=");
		builder.append(playerRoles);
		builder.append(", rnd=");
		builder.append(rnd);
		builder.append(", imgVizInfo=");
		builder.append(imgVizInfo);
		builder.append("]");
		return builder.toString();
	}

}