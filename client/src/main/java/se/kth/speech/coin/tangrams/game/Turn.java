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

import java.util.Map.Entry;

import se.kth.speech.SpatialRegion;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 12 Jan 2017
 *
 */
public final class Turn {

	private final Entry<SpatialRegion, SpatialRegion> move;

	private final String playerId;

	private final int sequenceOrdinality;

	Turn(final String playerId, final Entry<SpatialRegion, SpatialRegion> move, final int sequenceOrdinality) {
		this.playerId = playerId;
		this.move = move;
		this.sequenceOrdinality = sequenceOrdinality;
	}

	/**
	 * @return the move
	 */
	public Entry<SpatialRegion, SpatialRegion> getMove() {
		return move;
	}

	/**
	 * @return the playerId
	 */
	public String getPlayerId() {
		return playerId;
	}

	/**
	 * @return the sequenceOrdinality
	 */
	public int getSequenceOrdinality() {
		return sequenceOrdinality;
	}

}
