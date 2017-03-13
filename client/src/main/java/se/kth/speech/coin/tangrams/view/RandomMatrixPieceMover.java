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

import java.util.Iterator;
import java.util.Map.Entry;
import java.util.Random;
import java.util.function.Function;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.MutablePair;
import se.kth.speech.RandomCollections;
import se.kth.speech.SpatialMap;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 10 Mar 2017
 *
 */
final class RandomMatrixPieceMover<I> {

	private static final Logger LOGGER = LoggerFactory.getLogger(RandomMatrixPieceMover.class);

	private final SpatialMatrix<? super I> posMatrix;

	private final SpatialMap<ImageViewInfo> piecePlacements;

	private final Function<? super ImageViewInfo, ? extends I> pieceIdGetter;

	RandomMatrixPieceMover(final SpatialMatrix<? super I> posMatrix, final SpatialMap<ImageViewInfo> piecePlacements,
			final Function<? super ImageViewInfo, ? extends I> pieceIdGetter) {
		this.posMatrix = posMatrix;
		this.piecePlacements = piecePlacements;
		this.pieceIdGetter = pieceIdGetter;
	}

	public Entry<SpatialMap.Region, SpatialMap.Region> apply(final Random rnd) {
		final RandomMatrixPiecePlacer<I> piecePlacer = new RandomMatrixPiecePlacer<>(posMatrix, rnd, piecePlacements);
		// TODO: Change probability of a piece being selected for moving based
		// on if it was moved before: E.g. cannot move a given piece more than
		// twice in a row
		final SpatialMap.Region occupiedRegion = RandomCollections.getRandomElement(piecePlacements.getMinimalRegions(),
				rnd);
		Entry<SpatialMap.Region, Boolean> lastSuccessfulPlacementResult = null;
		do {
			final Iterator<ImageViewInfo> pieceIter = piecePlacements.getMinimalRegionElements().get(occupiedRegion)
					.iterator();
			// NOTE: The iterator should only have one element here
			regionPieceMovement: while (pieceIter.hasNext()) {
				final ImageViewInfo piece = pieceIter.next();
				final I pieceId = pieceIdGetter.apply(piece);
				LOGGER.info("Trying to move piece \"{}\" to a random location.", pieceId);
				final Entry<SpatialMap.Region, Boolean> placementResult = piecePlacer.apply(piece, pieceId);
				if (placementResult.getValue()) {
					lastSuccessfulPlacementResult = placementResult;
					LOGGER.debug(
							"Successfully placed piece \"{}\"; Setting its previous region to point to a null element.",
							pieceId);
					posMatrix.setPositionValues(occupiedRegion, null);
					pieceIter.remove();
				} else {
					lastSuccessfulPlacementResult = null;
					LOGGER.info("Couldn't place piece \"{}\".", pieceId);
					break regionPieceMovement;
				}
			}
		} while (lastSuccessfulPlacementResult == null);

		return new MutablePair<>(occupiedRegion, lastSuccessfulPlacementResult.getKey());
	}
}
