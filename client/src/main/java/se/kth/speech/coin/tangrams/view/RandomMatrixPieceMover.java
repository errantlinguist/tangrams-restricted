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

import java.util.Collection;
import java.util.Map.Entry;
import java.util.Random;
import java.util.function.Function;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.Matrix;
import se.kth.speech.RandomCollections;
import se.kth.speech.SpatialMap;
import se.kth.speech.SpatialMap.Region;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 10 Mar 2017
 *
 */
final class RandomMatrixPieceMover<I> {

	private static final Logger LOGGER = LoggerFactory.getLogger(RandomMatrixPieceMover.class);

	private final Matrix<? super I> posMatrix;

	private final SpatialMap<ImageViewInfo> piecePlacements;

	private final Function<? super ImageViewInfo, ? extends I> pieceIdGetter;

	RandomMatrixPieceMover(final Matrix<? super I> posMatrix, final SpatialMap<ImageViewInfo> piecePlacements,
			final Function<? super ImageViewInfo, ? extends I> pieceIdGetter) {
		this.posMatrix = posMatrix;
		this.piecePlacements = piecePlacements;
		this.pieceIdGetter = pieceIdGetter;
	}

	public Entry<SpatialMap.Region, Boolean> apply(final Random rnd) {
		final RandomMatrixPiecePlacer<I> piecePlacer = new RandomMatrixPiecePlacer<>(posMatrix, rnd, piecePlacements);
		// TODO: Change probability of a piece being selected for moving based
		// on if it was moved before: E.g. cannot move a given piece more than
		// twice in a row
		final Region occupiedRegion = RandomCollections.getRandomElement(piecePlacements.getMinimalRegions(), rnd);
		final Collection<ImageViewInfo> pieces = piecePlacements.getMinimalRegionElements().get(occupiedRegion);
		Entry<SpatialMap.Region, Boolean> lastSuccessfulPlacementResult = null;
		do {
			// NOTE: The collection should only have one element here
			regionPieceMovement: for (final ImageViewInfo piece : pieces) {
				final I pieceId = pieceIdGetter.apply(piece);
				LOGGER.info("Trying to move piece \"{}\" to a random location.", pieceId);
				final Entry<SpatialMap.Region, Boolean> placementResult = piecePlacer.apply(piece, pieceId);
				if (placementResult.getValue()) {
					lastSuccessfulPlacementResult = placementResult;
				} else {
					lastSuccessfulPlacementResult = null;
					break regionPieceMovement;
				}
			}
		} while (lastSuccessfulPlacementResult == null);

		return lastSuccessfulPlacementResult;
	}
}
