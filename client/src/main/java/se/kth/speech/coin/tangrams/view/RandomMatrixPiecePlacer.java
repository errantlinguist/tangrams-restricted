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

import java.awt.Image;
import java.util.Map.Entry;
import java.util.Random;

import se.kth.speech.Matrix;
import se.kth.speech.MutablePair;
import se.kth.speech.SpatialMap;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 10 Mar 2017
 *
 */
final class RandomMatrixPiecePlacer<T> {

	private final Matrix<T> posMatrix;
	
	private final Random rnd;
	
	private final SpatialMap<Entry<? extends Image, ImageViewInfo>> occupiedPositions;

	RandomMatrixPiecePlacer(final Matrix<T> posMatrix, final Random rnd,
			final SpatialMap<Entry<? extends Image, ImageViewInfo>> occupiedPositions) {
		this.posMatrix = posMatrix;
		this.rnd = rnd;
		this.occupiedPositions = occupiedPositions;
	}

	public Entry<SpatialMap.Region, Boolean> apply(final Entry<? extends Image, ImageViewInfo> imgViewInfoDatum,
			final T pieceId) {
		final int[] posDims = posMatrix.getDimensions();
		final ImageViewInfo viewInfo = imgViewInfoDatum.getValue();
		// The number of rows and columns this image takes up in the
		// position matrix
		final int[] piecePosMatrixSize = MatrixSpaces.createPosMatrixBoundsArray(viewInfo);
		// Randomly pick a space in the matrix
		final SpatialMap.Region piecePosition = MatrixSpaces.createRandomSpatialRegion(piecePosMatrixSize, posDims,
				rnd);
		final boolean success;
		if (occupiedPositions.isOccupied(piecePosition)) {
			success = false;
		} else {
			MatrixSpaces.setMatrixPositionValues(posMatrix, piecePosition, pieceId);
			occupiedPositions.put(piecePosition, imgViewInfoDatum);
			success = true;
		}
		return new MutablePair<>(piecePosition, success);
	}
}
