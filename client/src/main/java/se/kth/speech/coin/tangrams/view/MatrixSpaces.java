/*
 *  This file is part of client.
 *
 *  client is free software: you can redistribute it and/or modify
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
package se.kth.speech.coin.tangrams.view;

import java.util.List;
import java.util.ListIterator;

import se.kth.speech.Matrix;
import se.kth.speech.SpatialMap;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 10 Mar 2017
 *
 */
final class MatrixSpaces {

	static <T> void setMatrixPositionValues(final Matrix<T> posMatrix, final SpatialMap.Region occupiedRegion,
			final T pieceId) {
		final ListIterator<List<T>> rowIter = posMatrix.rowIterator(occupiedRegion.getXLowerBound());
		for (int rowIdx = rowIter.nextIndex(); rowIdx < occupiedRegion.getXUpperBound(); rowIdx++) {
			final List<T> occupiedRow = rowIter.next();
			final ListIterator<T> rowCellIter = occupiedRow.listIterator(occupiedRegion.getYLowerBound());
			for (int colIdx = rowCellIter.nextIndex(); colIdx < occupiedRegion.getYUpperBound(); colIdx++) {
				final T nextOldPieceId = rowCellIter.next();
				if (nextOldPieceId == null) {
					rowCellIter.set(pieceId);
				} else {
					throw new IllegalArgumentException(
							String.format("Previous value at %d*%d not null.", rowIdx, colIdx));
				}
			}
		}
	}

	private MatrixSpaces() {

	}

}
