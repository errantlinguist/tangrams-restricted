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

import java.util.List;

import se.kth.speech.coin.tangrams.game.LocalController;
import se.kth.speech.coin.tangrams.game.Model;
import se.kth.speech.coin.tangrams.game.RemoteController;

public final class GameState {

	private final LocalController<Integer> localController;

	private final List<String> playerIds;

	private final RemoteController<Integer> remoteController;

	private final long seed;

	private final Model<Integer> winningModel;

	GameState(final LocalController<Integer> localController, final RemoteController<Integer> remoteController,
			final Model<Integer> winningModel, final List<String> playerIds, final long seed) {
		this.localController = localController;
		this.remoteController = remoteController;
		this.winningModel = winningModel;
		this.playerIds = playerIds;
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
		if (!(obj instanceof GameState)) {
			return false;
		}
		final GameState other = (GameState) obj;
		if (localController == null) {
			if (other.localController != null) {
				return false;
			}
		} else if (!localController.equals(other.localController)) {
			return false;
		}
		if (playerIds == null) {
			if (other.playerIds != null) {
				return false;
			}
		} else if (!playerIds.equals(other.playerIds)) {
			return false;
		}
		if (remoteController == null) {
			if (other.remoteController != null) {
				return false;
			}
		} else if (!remoteController.equals(other.remoteController)) {
			return false;
		}
		if (seed != other.seed) {
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
	 * @return the localController
	 */
	public LocalController<Integer> getLocalController() {
		return localController;
	}

	/**
	 * @return the playerIds
	 */
	public List<String> getPlayerIds() {
		return playerIds;
	}

	/**
	 * @return the remoteController
	 */
	public RemoteController<Integer> getRemoteController() {
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
	public Model<Integer> getWinningModel() {
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
		result = prime * result + (localController == null ? 0 : localController.hashCode());
		result = prime * result + (playerIds == null ? 0 : playerIds.hashCode());
		result = prime * result + (remoteController == null ? 0 : remoteController.hashCode());
		result = prime * result + (int) (seed ^ seed >>> 32);
		result = prime * result + (winningModel == null ? 0 : winningModel.hashCode());
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
		builder.append("GameState [localController=");
		builder.append(localController);
		builder.append(", remoteController=");
		builder.append(remoteController);
		builder.append(", winningModel=");
		builder.append(winningModel);
		builder.append(", playerIds=");
		builder.append(playerIds);
		builder.append(", seed=");
		builder.append(seed);
		builder.append("]");
		return builder.toString();
	}

}