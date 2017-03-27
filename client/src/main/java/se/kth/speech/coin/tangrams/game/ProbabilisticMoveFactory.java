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
package se.kth.speech.coin.tangrams.game;

import java.util.Collection;
import java.util.List;
import java.util.Random;
import java.util.Set;
import java.util.function.Supplier;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Sets;

import se.kth.speech.MapEntryRemapping;
import se.kth.speech.RandomCollections;
import se.kth.speech.SpatialMatrix;
import se.kth.speech.SpatialRegion;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 23 Mar 2017
 *
 */
public final class ProbabilisticMoveFactory implements Supplier<MapEntryRemapping<Integer, SpatialRegion>> {

	private static final Logger LOGGER = LoggerFactory.getLogger(ProbabilisticMoveFactory.class);

	private final Controller.History history;

	private final SpatialMatrix<Integer> posMatrix;

	private final Random rnd;

	private final int turnCount = 0;

	public ProbabilisticMoveFactory(final Random rnd, final SpatialMatrix<Integer> posMatrix,
			final Controller.History history) {
		this.rnd = rnd;
		this.posMatrix = posMatrix;
		this.history = history;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Supplier#get()
	 */
	@Override
	public MapEntryRemapping<Integer, SpatialRegion> get() {
		LOGGER.debug("Generating random move.");
		final List<SpatialRegion> regionsToTry = posMatrix.getElementPlacements().getMinimalRegions();
		// TODO: estimate number of failed tries better
		final Set<SpatialRegion> failedRegions = Sets.newHashSetWithExpectedSize(Math.min(regionsToTry.size(), 16));

		MapEntryRemapping<Integer, SpatialRegion> result = null;
		do {
			final SpatialRegion occupiedRegion = RandomCollections.getRandomElement(regionsToTry, rnd);
			final Set<SpatialRegion> regionValidMoves = posMatrix.createValidMoveSet(occupiedRegion);
			if (regionValidMoves.isEmpty()) {
				// The pieces at the region cannot be moved anywhere else; Give
				// up
				LOGGER.debug("Not valid moves for piece at occupied {}.", occupiedRegion);
				failedRegions.add(occupiedRegion);
			} else {
				final Collection<Integer> pieceIds = posMatrix.getElementPlacements().getMinimalRegionElements()
						.get(occupiedRegion);
				switch (pieceIds.size()) {
				case 0: {
					throw new AssertionError("Piece ID set for extant region should not be empty but is.");
				}
				case 1: {
					final int pieceId = pieceIds.iterator().next();
					final SpatialRegion moveTarget = RandomCollections.getRandomElement(regionValidMoves, rnd);
					assert !posMatrix.isOccupied(moveTarget);
					if (shouldMove(occupiedRegion, moveTarget, pieceId)) {
						result = new MapEntryRemapping<>(pieceId, occupiedRegion, moveTarget);
					} else {
						LOGGER.debug("Piece ID \"{}\" was probabilistically ruled out.");
					}
					break;
				}
				default: {
					throw new UnsupportedOperationException("Cannot move multiple pieces in one turn.");
				}
				}
			}

		} while (result == null && failedRegions.size() < regionsToTry.size());

		return result;
	}

	private double calculateMoveProb(final int pieceId) {
		final int lastPieceMoveTurn = history.getLastPieceMoveTurn(pieceId);
		final double currentTurn = turnCount;
		// Add 1 to the denominator to avoid the last moved piece from being
		// picked again, and add to the denominator in order to avoid dividing
		// by zero
		final double quotient = lastPieceMoveTurn + 1 / currentTurn + 1;
		// TODO: Convert this into integer arithmetic in the case you have to
		// sum probs later
		return 1.0 - quotient;
	}

	private int calculateRndMaxVal(final int pieceId) {
		final double prob = calculateMoveProb(pieceId);
		// Subtract one because the Random.nextInt(bound) returns ints in the
		// range of 0 to bound-1
		return (int) (prob * 100) - 1;
	}

	private boolean shouldMove(final SpatialRegion region, final SpatialRegion target, final int pieceId) {
		// TODO: implement probability of moving from a given region and/or
		// moving a given piece from a given region (all possible combinations?)
		// see: Bayes' theorem
		return shouldMovePiece(pieceId);
	}

	private boolean shouldMovePiece(final int pieceId) {
		final int rndMaxVal = calculateRndMaxVal(pieceId);
		final int rndVal = rnd.nextInt(100);
		return rndVal <= rndMaxVal;
	}

}
