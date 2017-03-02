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
import java.util.Random;
import java.util.function.Function;

import se.kth.speech.IntArrays;
import se.kth.speech.Matrix;
import se.kth.speech.ObjectArrays;
import se.kth.speech.coin.tangrams.game.Model;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 3 Jan 2017
 *
 */
final class RandomModelFactory<T> implements Function<Random, Model<T>> {

	/**
	 * The dimensions of the model (i.e.&nbsp;row and column count) of the board
	 * model to create.
	 */
	private final int[] coordDims;

	/**
	 * An array of all values which can occupy a coordinate on the board.
	 */
	private final T[] possibleCoordOccupants;

	/**
	 *
	 * @param coordDims
	 *            The dimensions of the model (i.e.&nbsp;row and column count)
	 *            of the board model to create.
	 * @param possibleCoordOccupants
	 *            An array of all values which can occupy a coordinate on the
	 *            board.
	 */
	RandomModelFactory(final int[] coordDims, final T[] possibleCoordOccupants) {
		if (coordDims.length != 2) {
			throw new IllegalArgumentException("Only 2D models are supported.");
		}
		this.coordDims = coordDims;
		this.possibleCoordOccupants = possibleCoordOccupants;
	}

	@Override
	public Model<T> apply(final Random rnd) {
		final T[] possibleCoordOccupants = createCoordPointArray();
		ObjectArrays.shuffle(possibleCoordOccupants, rnd);
		return new Model<>(new Matrix<>(possibleCoordOccupants, coordDims[1]));
	}

	private T[] createCoordPointArray() {
		final int cellCount = IntArrays.product(coordDims);
		final int emptySpaces = cellCount - possibleCoordOccupants.length;
		if (emptySpaces < 1) {
			throw new IllegalStateException(
					String.format("The board is not big enough (at size %d) to fit %d piece(s).", cellCount,
							possibleCoordOccupants.length));
		}
		final T[] result = Arrays.copyOf(possibleCoordOccupants, cellCount);
		final int firstEmptySpaceIdx = result.length - emptySpaces;
		for (int emptySpaceIdx = firstEmptySpaceIdx; emptySpaceIdx < result.length; ++emptySpaceIdx) {
			result[emptySpaceIdx] = null;
		}
		return result;
	}

}
