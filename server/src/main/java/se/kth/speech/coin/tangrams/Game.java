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

import java.util.Arrays;
import java.util.Objects;

import se.kth.speech.coin.tangrams.game.Model;
import se.kth.speech.coin.tangrams.game.RemoteController;

public final class Game<T> {

	private final RemoteController<T> remoteController;

	private final long seed;

	private final Model<T> winningModel;

	Game(final RemoteController<T> remoteController, final Model<T> winningModel, final long seed) {
		if (!Arrays.equals(remoteController.getModel().getCoordinateDimensions(),
				winningModel.getCoordinateDimensions())) {
			throw new IllegalArgumentException("The dimensions of the model and the winning model are not equal.");
		}
		this.remoteController = remoteController;
		this.winningModel = winningModel;
		this.seed = seed;
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
		if (!(obj instanceof Game)) {
			return false;
		}
		final Game<?> other = (Game<?>) obj;
		if (remoteController == null) {
			if (other.remoteController != null) {
				return false;
			}
		} else if (!remoteController.equals(other.remoteController)) {
			return false;
		}
		if (winningModel == null) {
			if (other.winningModel != null) {
				return false;
			}
		} else if (!winningModel.equals(other.winningModel)) {
			return false;
		}
		return true;
	}

	/**
	 * @return the remoteController
	 */
	public RemoteController<T> getRemoteController() {
		return remoteController;
	}

	/**
	 * @return the seed
	 */
	public long getSeed() {
		return seed;
	}

	/**
	 * @return the winningModel
	 */
	public Model<T> getWinningModel() {
		return winningModel;
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
		result = prime * result + (remoteController == null ? 0 : remoteController.hashCode());
		result = prime * result + (winningModel == null ? 0 : winningModel.hashCode());
		return result;
	}

	public boolean isWon() {
		return Objects.equals(remoteController.getModel(), winningModel);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder();
		builder.append("Game [remoteController=");
		builder.append(remoteController);
		builder.append(", winningModel=");
		builder.append(winningModel);
		builder.append("]");
		return builder.toString();
	}

}