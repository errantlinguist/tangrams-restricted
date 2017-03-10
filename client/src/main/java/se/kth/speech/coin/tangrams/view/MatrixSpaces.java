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
import java.util.Random;
import java.util.stream.IntStream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.Matrix;
import se.kth.speech.SpatialMap;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 10 Mar 2017
 *
 */
final class MatrixSpaces {

	private static final Logger LOGGER = LoggerFactory.getLogger(MatrixSpaces.class);

	private static SpatialMap.Region createSpatialRegion(final int[] startMatrixIdx, final int[] endMatrixIdx) {
		return new SpatialMap.Region(startMatrixIdx[0], endMatrixIdx[0], startMatrixIdx[1], endMatrixIdx[1]);
	}

	static int[] createPosMatrixBoundsArray(final ImageViewInfo viewInfo) {
		final ImageViewInfo.RasterizationInfo rasterizationInfo = viewInfo.getRasterization();
		// NOTE: "rows" in the matrix go top-bottom and "cols" go left-right
		// The number of rows this image takes up in the
		// position matrix
		final int occupiedPosMatrixRowCount = rasterizationInfo.getHeight() / rasterizationInfo.getGcd();
		// The number of columns this image takes up in the
		// position matrix
		final int occupiedPosMatrixColCount = rasterizationInfo.getWidth() / rasterizationInfo.getGcd();
		LOGGER.debug("Calculated position grid size {}*{} for \"{}\".", new Object[] {
				viewInfo.getVisualization().getResourceLoc(), occupiedPosMatrixRowCount, occupiedPosMatrixColCount });

		return new int[] { occupiedPosMatrixRowCount, occupiedPosMatrixColCount };
	}

	static SpatialMap.Region createRandomSpatialRegion(final int[] piecePosMatrixSize, final int[] matrixDims,
			final Random rnd) {
		final IntStream maxPossibleMatrixIdxs = IntStream.range(0, matrixDims.length)
				.map(i -> matrixDims[i] - piecePosMatrixSize[i] + 1);
		// Randomly pick a space in the matrix
		final int[] startMatrixIdx = maxPossibleMatrixIdxs.map(rnd::nextInt).toArray();
		final int[] endMatrixIdx = IntStream.range(0, startMatrixIdx.length)
				.map(i -> startMatrixIdx[i] + piecePosMatrixSize[i]).toArray();
		return createSpatialRegion(startMatrixIdx, endMatrixIdx);
	}

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
