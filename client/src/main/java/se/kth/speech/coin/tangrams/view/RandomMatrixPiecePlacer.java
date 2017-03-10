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
import java.util.function.BiFunction;

import se.kth.speech.Matrix;
import se.kth.speech.MutablePair;
import se.kth.speech.SpatialMap;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 10 Mar 2017
 *
 */
final class RandomMatrixPiecePlacer<I>
		implements BiFunction<Entry<? extends Image, ImageViewInfo>, I, Entry<SpatialMap.Region, Boolean>> {

	private final SpatialMap<Entry<? extends Image, ImageViewInfo>> occupiedPositions;

	private final Matrix<I> posMatrix;

	private final RandomImageMatrixSpatialRegionFactory regionFactory;
	
	private final Random rnd;

	RandomMatrixPiecePlacer(final Matrix<I> posMatrix, final Random rnd,
			final SpatialMap<Entry<? extends Image, ImageViewInfo>> occupiedPositions) {
		this.posMatrix = posMatrix;
		this.rnd = rnd;
		this.occupiedPositions = occupiedPositions;
	
		regionFactory = new RandomImageMatrixSpatialRegionFactory(posMatrix.getDimensions());
	}

	@Override
	public Entry<SpatialMap.Region, Boolean> apply(final Entry<? extends Image, ImageViewInfo> imgViewInfoDatum,
			final I pieceId) {
		final ImageViewInfo viewInfo = imgViewInfoDatum.getValue();
		// Randomly pick a space in the matrix
		final SpatialMap.Region piecePosition = regionFactory.apply(viewInfo, rnd);
		final boolean success;
		if (occupiedPositions.isOccupied(piecePosition)) {
			success = false;
		} else {
			MatrixSpaces.setMatrixPositionValues(posMatrix, piecePosition, pieceId);
			occupiedPositions.put(imgViewInfoDatum, piecePosition);
			success = true;
		}
		return new MutablePair<>(piecePosition, success);
	}
}
