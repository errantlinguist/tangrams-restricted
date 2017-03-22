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
package se.kth.speech.coin.tangrams;

import com.google.common.collect.BiMap;

import se.kth.speech.SpatialMatrix;
import se.kth.speech.coin.tangrams.game.PlayerRole;

public final class Game<T> {

	private final long seed;

	private final SpatialMatrix<T> model;

	private final BiMap<PlayerRole, String> playerRoles;

	Game(final long seed, final SpatialMatrix<T> model, final BiMap<PlayerRole, String> playerRoles) {
		this.seed = seed;
		this.model = model;
		this.playerRoles = playerRoles;
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
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder();
		builder.append("Game [seed=");
		builder.append(seed);
		builder.append(", model=");
		builder.append(model);
		builder.append(", playerRoles=");
		builder.append(playerRoles);
		builder.append(']');
		return builder.toString();
	}

}