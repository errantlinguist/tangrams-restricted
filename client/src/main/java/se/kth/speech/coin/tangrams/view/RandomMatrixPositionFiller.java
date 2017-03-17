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

import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Random;
import java.util.Set;
import java.util.function.Function;
import java.util.function.IntFunction;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Sets;
import com.google.common.collect.Table;

import se.kth.speech.RandomCollections;
import se.kth.speech.SpatialRegion;
import se.kth.speech.SpatialMatrix;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 9 Mar 2017
 *
 */
final class RandomMatrixPositionFiller<I, E> implements Function<Collection<? extends E>, Set<I>> {

	private static final Logger LOGGER = LoggerFactory.getLogger(RandomMatrixPositionFiller.class);

	/**
	 * <strong>NOTE:</strong> This creates a {@link LinkedHashSet} in order to
	 * ensure that iteration order is stable across invokations.
	 */
	private static final IntFunction<LinkedHashSet<SpatialRegion>> SUB_REGION_SET_FACTORY = LinkedHashSet::new;

	private final Function<? super E, I> pieceIdGetter;

	private final Function<? super E, int[]> piecePosMatrixSizeFactory;

	private final SpatialMatrix<? super I, E> posMatrix;

	private final Random rnd;

	RandomMatrixPositionFiller(final SpatialMatrix<? super I, E> posMatrix, final Function<? super E, I> pieceIdGetter,
			final Random rnd, final Function<? super E, int[]> piecePosMatrixSizeFactory) {
		this.posMatrix = posMatrix;
		this.pieceIdGetter = pieceIdGetter;
		this.rnd = rnd;
		this.piecePosMatrixSizeFactory = piecePosMatrixSizeFactory;
	}

	@Override
	public Set<I> apply(final Collection<? extends E> pieces) {
		LOGGER.info("Trying to place {} pieces.", pieces.size());
		final Table<Integer, Integer, LinkedHashSet<SpatialRegion>> subRegionsToTry = posMatrix
				.createSizeIndexedRegionPowerSet(SUB_REGION_SET_FACTORY);
		final Set<I> result = Sets.newHashSetWithExpectedSize(pieces.size());
		{
			// Randomly place each image in the position matrix
			final Iterator<? extends E> pieceIter = pieces.iterator();
			while (pieceIter.hasNext()) {
				final E piece = pieceIter.next();
				LOGGER.debug("Adding {}.", piece);
				final I pieceId = pieceIdGetter.apply(piece);
				final SpatialRegion placementResult = placePieceRandomly(piece, pieceId, subRegionsToTry);
				LOGGER.debug("Added {} (with ID \"{}\") to {}.", new Object[] { piece, pieceId, placementResult });
				result.add(pieceId);
			}
		}
		LOGGER.info("Added piece IDs {}.", result);
		return result;
	}

	private SpatialRegion placePieceRandomly(final E piece, final I pieceId,
			final Table<? super Integer, ? super Integer, ? extends Collection<SpatialRegion>> subRegionsToTry) {
		// The number of rows and columns this image takes up in the
		// position matrix
		final int[] piecePosMatrixSize = piecePosMatrixSizeFactory.apply(piece);
		final Collection<SpatialRegion> allFittingSubRegions = subRegionsToTry.get(piecePosMatrixSize[0],
				piecePosMatrixSize[1]);

		SpatialRegion result = null;
		do {
			// Randomly pick a space in the matrix
			result = RandomCollections.getRandomElement(allFittingSubRegions, rnd);
			LOGGER.debug("Result size: {}", result.getDimensions());
			if (posMatrix.isOccupied(result)) {
				LOGGER.debug("Region {} is already occupied.", result);
				allFittingSubRegions.remove(result);
				result = null;
			} else {
				posMatrix.placeElement(piece, result);
				assert posMatrix.isOccupied(result);
			}
		} while (result == null && !allFittingSubRegions.isEmpty());

		if (result == null) {
			throw new IllegalStateException(String.format(
					"Could not place piece \"%s\" (with ID \"%s\") because all regions of size %s are already occupied.",
					piece, pieceId, Arrays.toString(piecePosMatrixSize)));
		} else {
			return result;
		}
	}

}
