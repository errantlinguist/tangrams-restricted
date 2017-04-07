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
package se.kth.speech;

import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.Map.Entry;
import java.util.Random;
import java.util.Set;
import java.util.function.Function;
import java.util.function.IntFunction;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Sets;
import com.google.common.collect.Table;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 9 Mar 2017
 *
 */
public final class RandomMatrixPositionFiller<I, E>
		implements Function<Collection<? extends Entry<? extends E, ? extends I>>, Set<I>> {

	private static final Logger LOGGER = LoggerFactory.getLogger(RandomMatrixPositionFiller.class);

	/**
	 * <strong>NOTE:</strong> This creates a {@link LinkedHashSet} in order to
	 * ensure that iteration order is stable across instances.
	 */
	private static final IntFunction<LinkedHashSet<SpatialRegion>> SUB_REGION_SET_FACTORY = Sets::newLinkedHashSetWithExpectedSize;

	private final boolean allowFailedPlacements;

	private final Function<? super E, int[]> elementPosMatrixSizeFactory;

	private final SpatialMatrix<? super I> posMatrix;

	private final Random rnd;

	public RandomMatrixPositionFiller(final SpatialMatrix<? super I> posMatrix, final Random rnd,
			final Function<? super E, int[]> elementPosMatrixSizeFactory, final boolean allowFailedPlacements) {
		this.posMatrix = posMatrix;
		this.rnd = rnd;
		this.elementPosMatrixSizeFactory = elementPosMatrixSizeFactory;
		this.allowFailedPlacements = allowFailedPlacements;
	}

	@Override
	public Set<I> apply(final Collection<? extends Entry<? extends E, ? extends I>> elementIds) {
		LOGGER.info("Trying to place {} elements.", elementIds.size());
		// FIXME: Create the element position sizes here and then only get the
		// sub-regions of appropriate size in order to avoid a combinatorial
		// explosion
		final Table<Integer, Integer, LinkedHashSet<SpatialRegion>> subRegionsToTry = posMatrix
				.createSizeIndexedRegionTable(SUB_REGION_SET_FACTORY);
		final Set<I> result = Sets.newHashSetWithExpectedSize(elementIds.size());
		{
			// Randomly place each element in the position matrix
			for (final Entry<? extends E, ? extends I> elementId : elementIds) {
				final E element = elementId.getKey();
				LOGGER.debug("Adding {}.", element);
				final I id = elementId.getValue();
				final SpatialRegion placementResult = placePieceRandomly(element, id, subRegionsToTry);
				if (LOGGER.isDebugEnabled()) {
					LOGGER.debug("Added {} (with ID \"{}\") to {}.", new Object[] { element, id, placementResult });
				}
				result.add(id);
			}
		}
		LOGGER.info("Added element IDs {}.", result);
		return result;
	}

	private SpatialRegion placePieceRandomly(final E element, final I id,
			final Table<? super Integer, ? super Integer, ? extends Collection<SpatialRegion>> subRegionsToTry) {
		// The number of rows and columns this element takes up in the
		// position matrix
		final int[] elementPosMatrixSize = elementPosMatrixSizeFactory.apply(element);
		final Collection<SpatialRegion> allFittingSubRegions = subRegionsToTry.get(elementPosMatrixSize[0],
				elementPosMatrixSize[1]);
		if (allFittingSubRegions == null) {
			throw new IllegalArgumentException(String.format(
					"Found no subregions of matrix of dimensions %s of sufficient size to hold element of dimenions %s.",
					Arrays.toString(posMatrix.getDimensions()), Arrays.toString(elementPosMatrixSize)));
		} else {
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
					posMatrix.placeElement(id, result);
					assert posMatrix.isOccupied(result);
				}
			} while (result == null && !allFittingSubRegions.isEmpty());

			if (result == null) {
				final String msg = String.format(
						"Could not place element \"%s\" (with ID \"%s\") because all regions of size %s are already occupied.",
						element, id, Arrays.toString(elementPosMatrixSize));
				if (allowFailedPlacements) {
					LOGGER.warn(msg);
				} else {
					throw new IllegalArgumentException(msg);
				}
			}
			return result;
		}
	}

}
