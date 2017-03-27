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

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Deque;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Random;
import java.util.Set;
import java.util.function.Predicate;
import java.util.function.Supplier;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Maps;
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
public final class PatternMoveFactory implements Supplier<MapEntryRemapping<Integer, SpatialRegion>> {

	private enum PieceChoice {
		HISTORY_LONG_TERM, HISTORY_SHORT_TERM, NEW_PIECE;
	}

	private static final Logger LOGGER = LoggerFactory.getLogger(PatternMoveFactory.class);

	private final Set<Integer> longTermHistory;

	private final PieceChoice nextChoiceType;

	private final SpatialMatrix<Integer> posMatrix;

	private final Random rnd;

	private final Set<Integer> shortTermHistory;
	
	
	private final Set<Integer> history;
	
	public PatternMoveFactory(final Random rnd, final SpatialMatrix<Integer> posMatrix) {
		this.rnd = rnd;
		this.posMatrix = posMatrix;

		nextChoiceType = PieceChoice.NEW_PIECE;
		this.history = new HashSet<>();
		shortTermHistory = new HashSet<>();
		longTermHistory = new HashSet<>();
	}
	
	private MapEntryRemapping<Integer, SpatialRegion> pick(){
		MapEntryRemapping<Integer, SpatialRegion> result;
		if (history.size() < 4 || history.size() % 2 == 0){
			// The first four pieces should be picked random as well as every other piece
			Collection<Integer> pieceIds = posMatrix.getElementPlacements().getAllElements();
			result = createRandomMove(pieceIds, pieceId -> !history.contains(pieceId));
		} else if (history.size() % 2 == 0) {
			// Every other 
		}

		Integer pieceId = result.getKey();
		history.add(pieceId);
		return result;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Supplier#get()
	 */
	@Override
	public MapEntryRemapping<Integer, SpatialRegion> get() {
		MapEntryRemapping<Integer, SpatialRegion> result = null;
		switch (nextChoiceType) {
		case HISTORY_LONG_TERM: {
//			MapEntryRemapping<Integer, SpatialRegion> move = createRandomMove(longTermHistory);
			break;
		}
		case HISTORY_SHORT_TERM: {
//			MapEntryRemapping<Integer, SpatialRegion> move = createRandomMove(shortTermHistory);
			break;
		}
		case NEW_PIECE: {
			result = findNewRandomMove();
			break;
		}
		default: {
			throw new AssertionError("No logic for handling " + nextChoiceType + ".");
		}

		}

		return result;
	}

	private MapEntryRemapping<Integer, SpatialRegion> createRandomMove(final Collection<Integer> pieceIds, Predicate<? super Integer> pieceIdFilter) {
		final Integer pieceId = RandomCollections.getRandomElement(pieceIds, rnd);
		final SpatialRegion sourceRegion = posMatrix.getElementPlacements().getElementMinimalRegions().get(pieceId);
		final Set<SpatialRegion> possibleTargetRegions = posMatrix.createValidMoveSet(sourceRegion);
		final SpatialRegion targetRegion = RandomCollections.getRandomElement(possibleTargetRegions, rnd);
		return new MapEntryRemapping<>(pieceId, sourceRegion, targetRegion);
	}

	private MapEntryRemapping<Integer, SpatialRegion> findNewRandomMove() {
		LOGGER.debug("Creating random move.");
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
					if (wasPieceAlreadyMoved(pieceId)) {
						LOGGER.debug("Piece ID \"{}\" was already moved at least once.");
					} else {
						result = new MapEntryRemapping<>(pieceId, occupiedRegion, moveTarget);
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

	private boolean wasPieceAlreadyMoved(final int pieceId) {
		return shortTermHistory.contains(pieceId) || longTermHistory.contains(pieceId);
	}

}
