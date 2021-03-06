/*
 *  This file is part of Tangrams-restricted.
 *
 *  Tangrams-restricted is free software: you can redistribute it and/or modify
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

import java.util.Map;

import se.kth.speech.SpatialMatrix;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;

public final class Game<T> {

	private final ImageVisualizationInfo imgVisualizationInfo;

	private final SpatialMatrix<T> model;

	private final Map<PlayerRole, String> playerRoles;

	private final long seed;

	Game(final long seed, final SpatialMatrix<T> model, final ImageVisualizationInfo imgVisualizationInfo,
			final Map<PlayerRole, String> playerRoles) {
		this.seed = seed;
		this.model = model;
		this.imgVisualizationInfo = imgVisualizationInfo;
		this.playerRoles = playerRoles;
	}

	/**
	 * @return the imgVisualizationInfo
	 */
	public ImageVisualizationInfo getImgVisualizationInfo() {
		return imgVisualizationInfo;
	}

	/**
	 * @return the model
	 */
	public SpatialMatrix<T> getModel() {
		return model;
	}

	/**
	 * @return the playerRoles
	 */
	public Map<PlayerRole, String> getPlayerRoles() {
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
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder(256);
		builder.append("Game [imgVisualizationInfo=");
		builder.append(imgVisualizationInfo);
		builder.append(", model=");
		builder.append(model);
		builder.append(", playerRoles=");
		builder.append(playerRoles);
		builder.append(", seed=");
		builder.append(seed);
		builder.append(']');
		return builder.toString();
	}

}