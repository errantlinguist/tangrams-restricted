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
package se.kth.speech.coin.tangrams;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map.Entry;
import java.util.Random;
import java.util.Set;

import se.kth.speech.MutablePair;
import se.kth.speech.RandomCollections;
import se.kth.speech.SpatialMap;
import se.kth.speech.SpatialMatrix;
import se.kth.speech.SpatialRegion;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 24 Mar 2017
 *
 */
public final class SpatialMatrixTests {

	public static Entry<SpatialRegion, Entry<Integer, SpatialRegion>> createRandomValidMove(
			final SpatialMatrix<Integer> model, final Random rnd) {
		final SpatialMap<Integer> elemPlacements = model.getElementPlacements();
		final List<SpatialRegion> occupiedRegions = elemPlacements.getMinimalRegions();
		SpatialRegion sourceRegion = null;
		Set<SpatialRegion> regionValidMoves = Collections.emptySet();
		do {
			sourceRegion = RandomCollections.getRandomElement(occupiedRegions, rnd);
			regionValidMoves = model.createValidMoveSet(sourceRegion);
		} while (regionValidMoves.isEmpty());
		final Collection<Integer> pieceIds = elemPlacements.getMinimalRegionElements().get(sourceRegion);
		final Integer pieceId = RandomCollections.getRandomElement(pieceIds, rnd);
		final SpatialRegion targetRegion = RandomCollections.getRandomElement(regionValidMoves, rnd);
		return new MutablePair<>(sourceRegion, new MutablePair<>(pieceId, targetRegion));
	}

	public static Entry<SpatialRegion, Integer> findRandomMovableElement(final SpatialMatrix<Integer> model,
			final Random rnd) {
		final SpatialMap<Integer> elemPlacements = model.getElementPlacements();
		final List<SpatialRegion> occupiedRegions = elemPlacements.getMinimalRegions();
		SpatialRegion sourceRegion = null;
		Set<SpatialRegion> regionValidMoves = Collections.emptySet();
		do {
			sourceRegion = RandomCollections.getRandomElement(occupiedRegions, rnd);
			regionValidMoves = model.createValidMoveSet(sourceRegion);
		} while (regionValidMoves.isEmpty());
		final Collection<Integer> pieceIds = elemPlacements.getMinimalRegionElements().get(sourceRegion);
		final Integer pieceId = RandomCollections.getRandomElement(pieceIds, rnd);
		return new MutablePair<>(sourceRegion, pieceId);
	}

	private SpatialMatrixTests() {

	}

}
