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
import java.util.LinkedList;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Random;
import java.util.Set;
import java.util.function.Predicate;
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
public final class PatternMoveFactory implements Supplier<MapEntryRemapping<Integer, SpatialRegion>> {

	private static final Logger LOGGER = LoggerFactory.getLogger(PatternMoveFactory.class);

	private final SpatialMatrix<Integer> posMatrix;

	private final Random rnd;

	private final List<Integer> history;

	private final int initialRndOnsetLength;

	private final int nthElemRandom;

	public PatternMoveFactory(final Random rnd, final SpatialMatrix<Integer> posMatrix, final int initialRndOnsetLength,
			final int nthElemRandom) {
		if (nthElemRandom == 0) {
			throw new IllegalArgumentException("n-th random element parameter cannot be zero.");
		}
		this.rnd = rnd;
		this.posMatrix = posMatrix;
		this.initialRndOnsetLength = initialRndOnsetLength;
		this.nthElemRandom = nthElemRandom;
		history = new LinkedList<>();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Supplier#get()
	 */
	@Override
	public MapEntryRemapping<Integer, SpatialRegion> get() throws NoSuchElementException {
		MapEntryRemapping<Integer, SpatialRegion> result;
		if (history.size() < initialRndOnsetLength) {
			LOGGER.info("Still creating initial random onset: {}", history);
			// The first n pieces should be picked randomly as well as every
			// m-th piece
			final Collection<Integer> pieceIds = posMatrix.getElementPlacements().getAllElements();
			result = createRandomMove(pieceIds, pieceId -> !history.contains(pieceId));
		} else {
			final int turnCount = history.size() + 1;
			if (turnCount % nthElemRandom == 0) {
				LOGGER.info("Found turn count {} % {} == 0: {}; Picking unseen piece.",
						new Object[] { turnCount, nthElemRandom, history });
				// The first n pieces should be picked randomly as well as every
				// m-th piece
				final Collection<Integer> pieceIds = posMatrix.getElementPlacements().getAllElements();
				result = createRandomMove(pieceIds, pieceId -> !history.contains(pieceId));
			} else {
				LOGGER.info("Picking already-seen piece from history: {}", history);
				// Pick a random element from the turns before the last one
				final Collection<Integer> pieceIds = history.subList(0, history.size() - 1);
				result = createRandomMove(pieceIds, pieceId -> true);
				// "pop" the result from its current position in the history
				history.remove(result.getKey());
			}
		}
		if (result == null) {
			throw new NoSuchElementException("Could not find any valid moves.");
		} else {
			// (Re-)add the result to the end of the history
			history.add(result.getKey());
			return result;
		}
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder();
		builder.append("PatternMoveFactory [posMatrix=");
		builder.append(posMatrix);
		builder.append(", rnd=");
		builder.append(rnd);
		builder.append(", history=");
		builder.append(history);
		builder.append(", initialRndOnsetLength=");
		builder.append(initialRndOnsetLength);
		builder.append(", nthElemRandom=");
		builder.append(nthElemRandom);
		builder.append("]");
		return builder.toString();
	}

	private MapEntryRemapping<Integer, SpatialRegion> createRandomMove(final Collection<Integer> pieceIds,
			final Predicate<? super Integer> pieceIdFilter) {
		LOGGER.debug("Creating random move.");
		// TODO: estimate number of failed tries better
		final Set<Integer> failedIds = Sets.newHashSetWithExpectedSize(Math.min(pieceIds.size(), 16));
		MapEntryRemapping<Integer, SpatialRegion> result = null;
		do {
			final Integer pieceId = RandomCollections.getRandomElement(pieceIds, rnd);
			if (pieceIdFilter.test(pieceId)) {
				result = createRandomMove(pieceId);
				if (result != null) {
					break;
				}
			} else {
				LOGGER.debug("Piece ID \"{}\" failed filter test; Cannot move again (yet).");
				failedIds.add(pieceId);
			}

		} while (result == null && failedIds.size() < pieceIds.size());
		return result;
	}

	private MapEntryRemapping<Integer, SpatialRegion> createRandomMove(final Integer pieceId) {
		final MapEntryRemapping<Integer, SpatialRegion> result;
		final SpatialRegion sourceRegion = posMatrix.getElementPlacements().getElementMinimalRegions().get(pieceId);
		final Set<SpatialRegion> possibleTargetRegions = posMatrix.createValidMoveSet(sourceRegion);
		if (possibleTargetRegions.isEmpty()) {
			LOGGER.debug("No valid moves for piece \"{}\", at {}.", pieceId, sourceRegion);
			result = null;
		} else {
			final SpatialRegion targetRegion = RandomCollections.getRandomElement(possibleTargetRegions, rnd);
			result = new MapEntryRemapping<>(pieceId, sourceRegion, targetRegion);
		}
		return result;
	}

}
