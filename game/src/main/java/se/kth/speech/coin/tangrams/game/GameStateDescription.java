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

import com.google.common.collect.BiMap;

import se.kth.speech.SpatialMatrix;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 19 Oct 2017
 *
 */
public final class GameStateDescription {

	private final ImageVisualizationInfo imgVizInfo;

	private final SpatialMatrix<Integer> model;

	private final BiMap<PlayerRole, String> playerRoles;

	public GameStateDescription(final SpatialMatrix<Integer> model, final ImageVisualizationInfo imgVizInfo,
			final BiMap<PlayerRole, String> playerRoles) {
		this.model = model;
		this.imgVizInfo = imgVizInfo;
		this.playerRoles = playerRoles;
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
		if (!(obj instanceof GameStateDescription)) {
			return false;
		}
		final GameStateDescription other = (GameStateDescription) obj;
		if (imgVizInfo == null) {
			if (other.imgVizInfo != null) {
				return false;
			}
		} else if (!imgVizInfo.equals(other.imgVizInfo)) {
			return false;
		}
		if (model == null) {
			if (other.model != null) {
				return false;
			}
		} else if (!model.equals(other.model)) {
			return false;
		}
		if (playerRoles == null) {
			if (other.playerRoles != null) {
				return false;
			}
		} else if (!playerRoles.equals(other.playerRoles)) {
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
	 * @return the model
	 */
	public SpatialMatrix<Integer> getModel() {
		return model;
	}

	/**
	 * @return the playerRoles
	 */
	public BiMap<PlayerRole, String> getPlayerRoles() {
		return playerRoles;
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
		result = prime * result + (imgVizInfo == null ? 0 : imgVizInfo.hashCode());
		result = prime * result + (model == null ? 0 : model.hashCode());
		result = prime * result + (playerRoles == null ? 0 : playerRoles.hashCode());
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
		builder.append("GameStateDescription [imgVizInfo=");
		builder.append(imgVizInfo);
		builder.append(", model=");
		builder.append(model);
		builder.append(", playerRoles=");
		builder.append(playerRoles);
		builder.append("]");
		return builder.toString();
	}

}
