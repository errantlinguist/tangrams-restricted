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

import com.google.common.collect.BiMap;

import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;
import se.kth.speech.coin.tangrams.game.PlayerRole;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 19 Oct 2017
 *
 */
public final class HashableGameStateDescription {

	private final boolean allowFailedPlacements;

	private final ImageVisualizationInfo imgVizInfo;

	private final HashableModelDescription modelDescription;

	private final double occupiedGridArea;

	private final BiMap<PlayerRole, String> playerRoles;

	private final long seed;

	public HashableGameStateDescription(final HashableModelDescription modelDescription,
			final ImageVisualizationInfo imgVizInfo, final BiMap<PlayerRole, String> playerRoles,
			final double occupiedGridArea, final long seed, final boolean allowFailedPlacements) {
		this.modelDescription = modelDescription;
		this.imgVizInfo = imgVizInfo;
		this.playerRoles = playerRoles;
		this.occupiedGridArea = occupiedGridArea;
		this.seed = seed;
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
		if (!(obj instanceof HashableGameStateDescription)) {
			return false;
		}
		final HashableGameStateDescription other = (HashableGameStateDescription) obj;
		if (allowFailedPlacements != other.allowFailedPlacements) {
			return false;
		}
		if (imgVizInfo == null) {
			if (other.imgVizInfo != null) {
				return false;
			}
		} else if (!imgVizInfo.equals(other.imgVizInfo)) {
			return false;
		}
		if (modelDescription == null) {
			if (other.modelDescription != null) {
				return false;
			}
		} else if (!modelDescription.equals(other.modelDescription)) {
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
		if (seed != other.seed) {
			return false;
		}
		return true;
	}

	/**
	 * @return the imgVisualizationInfo
	 */
	public ImageVisualizationInfo getImageVisualizationInfo() {
		return imgVizInfo;
	}

	/**
	 * @return the modelDescription
	 */
	public HashableModelDescription getModelDescription() {
		return modelDescription;
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
	 * @return the seed
	 */
	public long getSeed() {
		return seed;
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
		result = prime * result + (imgVizInfo == null ? 0 : imgVizInfo.hashCode());
		result = prime * result + (modelDescription == null ? 0 : modelDescription.hashCode());
		long temp;
		temp = Double.doubleToLongBits(occupiedGridArea);
		result = prime * result + (int) (temp ^ temp >>> 32);
		result = prime * result + (playerRoles == null ? 0 : playerRoles.hashCode());
		result = prime * result + (int) (seed ^ seed >>> 32);
		return result;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder(512);
		builder.append("HashableGameStateDescription [allowFailedPlacements=");
		builder.append(allowFailedPlacements);
		builder.append(", imgVizInfo=");
		builder.append(imgVizInfo);
		builder.append(", modelDescription=");
		builder.append(modelDescription);
		builder.append(", occupiedGridArea=");
		builder.append(occupiedGridArea);
		builder.append(", playerRoles=");
		builder.append(playerRoles);
		builder.append(", seed=");
		builder.append(seed);
		builder.append("]");
		return builder.toString();
	}

}
