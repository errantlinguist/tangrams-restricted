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
import se.kth.speech.SpatialMatrix;
import se.kth.speech.SpatialRegion;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 9 Mar 2017
 *
 */
public final class RandomMatrixPositionFiller<I, E> implements Function<Collection<? extends E>, Set<I>> {

	private static final Logger LOGGER = LoggerFactory.getLogger(RandomMatrixPositionFiller.class);

	/**
	 * <strong>NOTE:</strong> This creates a {@link LinkedHashSet} in order to
	 * ensure that iteration order is stable across invocations.
	 */
	private static final IntFunction<LinkedHashSet<SpatialRegion>> SUB_REGION_SET_FACTORY = Sets::newLinkedHashSetWithExpectedSize;

	private final Function<? super E, I> elementIdGetter;

	private final Function<? super E, int[]> elementPosMatrixSizeFactory;

	private final SpatialMatrix<? super I> posMatrix;

	private final Random rnd;

	private final boolean allowFailedPlacements;

	public RandomMatrixPositionFiller(final SpatialMatrix<? super I> posMatrix, final Function<? super E, I> elementIdGetter,
			final Random rnd, final Function<? super E, int[]> elementPosMatrixSizeFactory,
			final boolean allowFailedPlacements) {
		this.posMatrix = posMatrix;
		this.elementIdGetter = elementIdGetter;
		this.rnd = rnd;
		this.elementPosMatrixSizeFactory = elementPosMatrixSizeFactory;
		this.allowFailedPlacements = allowFailedPlacements;
	}

	@Override
	public Set<I> apply(final Collection<? extends E> elements) {
		LOGGER.info("Trying to place {} elements.", elements.size());
		final Table<Integer, Integer, LinkedHashSet<SpatialRegion>> subRegionsToTry = posMatrix
				.createSizeIndexedRegionPowerSet(SUB_REGION_SET_FACTORY);
		final Set<I> result = Sets.newHashSetWithExpectedSize(elements.size());
		{
			// Randomly place each element in the position matrix
			final Iterator<? extends E> elementIter = elements.iterator();
			while (elementIter.hasNext()) {
				final E element = elementIter.next();
				LOGGER.debug("Adding {}.", element);
				final I elementId = elementIdGetter.apply(element);
				final SpatialRegion placementResult = placePieceRandomly(element, elementId, subRegionsToTry);
				LOGGER.debug("Added {} (with ID \"{}\") to {}.", new Object[] { element, elementId, placementResult });
				result.add(elementId);
			}
		}
		LOGGER.info("Added element IDs {}.", result);
		return result;
	}

	private SpatialRegion placePieceRandomly(final E element, final I elementId,
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
					posMatrix.placeElement(elementId, result);
					assert posMatrix.isOccupied(result);
				}
			} while (result == null && !allFittingSubRegions.isEmpty());

			if (result == null) {
				final String msg = String.format(
						"Could not place element \"%s\" (with ID \"%s\") because all regions of size %s are already occupied.",
						element, elementId, Arrays.toString(elementPosMatrixSize));
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
