/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.game.
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
package se.kth.speech;

import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.function.Function;

import com.google.common.collect.Maps;

/**
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since Mar 18, 2017
 *
 */
final class MatrixSpatialMapFactory<E> implements Function<Matrix<? extends E>, SpatialMap<E>> {

	private static <E> Map<E, int[]> createElementMatrixIndexMap(final Matrix<? extends E> posMatrix) {
		// TODO: better estimate number of elements in the positional matrix
		final Map<E, int[]> result = Maps.newHashMapWithExpectedSize(posMatrix.getValues().size() / 4);
		for (final ListIterator<? extends List<? extends E>> rowIter = posMatrix.rowIterator(); rowIter.hasNext();) {
			final int rowStartIdx = rowIter.nextIndex();
			final List<? extends E> row = rowIter.next();
			for (final ListIterator<? extends E> rowCellIter = row.listIterator(); rowCellIter.hasNext();) {
				final int colStartIdx = rowCellIter.nextIndex();
				final E elemId = rowCellIter.next();
				if (elemId != null) {
					result.compute(elemId, (k, oldVal) -> {
						int[] newVal;
						final int rowEndIdx = rowStartIdx + 1;
						final int colEndIdx = colStartIdx + 1;
						if (oldVal == null) {
							// The element ID was seen for the first time
							newVal = new int[] { rowStartIdx, rowEndIdx, colStartIdx, colEndIdx };
						} else {
							// Update the upper bounds to the row and col on
							// which
							// this ID was now seen
							oldVal[1] = rowEndIdx;
							oldVal[3] = Math.max(oldVal[3], colEndIdx);
							newVal = oldVal;
						}
						return newVal;
					});
				}
			}
		}
		return result;
	}

	private static SpatialRegion createRegion(final int[] matrixIdxs) {
		return new SpatialRegion(matrixIdxs[0], matrixIdxs[1], matrixIdxs[2], matrixIdxs[3]);
	}

	@Override
	public SpatialMap<E> apply(final Matrix<? extends E> posMatrix) {
		final Map<E, int[]> elemIdxs = createElementMatrixIndexMap(posMatrix);
		final SpatialMap<E> result = SpatialMap.Factory.STABLE_ITER_ORDER.create(elemIdxs.size());
		elemIdxs.forEach((elem, idxs) -> {
			final SpatialRegion region = createRegion(idxs);
			result.put(elem, region);
		});
		return result;
	}

}
