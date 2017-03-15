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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;
import java.util.function.IntFunction;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.ArrayTable;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;
import com.google.common.collect.Table;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 10 Mar 2017
 *
 */
public final class SpatialMatrix<I, E> {

	private static final Logger LOGGER = LoggerFactory.getLogger(SpatialMatrix.class);

	private final Function<? super E, ? extends I> elementIdGetter;

	private final SpatialMap<E> elementPlacements;

	private transient final Function<SpatialMap.Region, Set<SpatialMap.Region>> newRegionPossibleMoveSetFactory;

	private final Matrix<I> positionMatrix;

	public SpatialMatrix(final Matrix<I> posMatrix, final Function<? super E, ? extends I> elementIdGetter,
			final Function<? super SpatialMatrix<I, E>, ? extends SpatialMap<E>> elementPosMapFactory) {
		this.positionMatrix = posMatrix;
		this.elementIdGetter = elementIdGetter;
		this.elementPlacements = elementPosMapFactory.apply(this);
		newRegionPossibleMoveSetFactory = region -> {
			final int occupiedRegionArea = region.getLengthX() * region.getLengthY();
			return Sets.newHashSetWithExpectedSize(posMatrix.getValues().size() / occupiedRegionArea);
		};
	}

	public void addRegionPowerSet(final Collection<? super SpatialMap.Region> regions) {
		final int dims[] = positionMatrix.getDimensions();
		final int x = dims[0];
		final int y = dims[1];
		for (int xLowerBound = 0; xLowerBound <= x; ++xLowerBound) {
			for (int xUpperBound = xLowerBound; xUpperBound <= x; ++xUpperBound) {
				for (int yLowerBound = 0; yLowerBound < y; ++yLowerBound) {
					for (int yUpperBound = yLowerBound; yUpperBound <= y; ++yUpperBound) {
						final SpatialMap.Region region = getRegion(xLowerBound, xUpperBound, yLowerBound, yUpperBound);
						regions.add(region);
					}
				}
			}
		}
	}

	public int calculateRegionPowerSetSize() {
		final int dims[] = positionMatrix.getDimensions();
		// m(m+1)n(n+1)/4 http://stackoverflow.com/a/17927830/1391325
		return Arrays.stream(dims).map(dim -> dim * (dim + 1)).reduce(1, (a, b) -> a * b) / dims.length * 2;
	}

	public void clearRegion(final SpatialMap.Region occupiedRegion) {
		final Collection<?> elements = elementPlacements.getMinimalRegionElements().get(occupiedRegion);
		// NOTE: Iterator.remove() for the instance returned by the
		// multimap's collection iterator throws a
		// ConcurrentModificationException
		elements.clear();
		setPositionValues(occupiedRegion, null);
	}

	public Set<SpatialMap.Region> createRegionPowerSet() {
		return createRegionPowerSet(Sets::newHashSetWithExpectedSize);
	}

	public <C extends Collection<? super SpatialMap.Region>> C createRegionPowerSet(
			final IntFunction<? extends C> collFactory) {
		final int subRegionCount = calculateRegionPowerSetSize();
		final C result = collFactory.apply(subRegionCount);
		addRegionPowerSet(result);
		return result;
	}

	public Table<Integer, Integer, SpatialMap.Region> createSizeIndexedRegionPowerSet() {
		final int dims[] = positionMatrix.getDimensions();
		final int x = dims[0];
		final int y = dims[1];
		final Table<Integer, Integer, SpatialMap.Region> result = ArrayTable.create(
				IntStream.range(1, x + 1).boxed().collect(Collectors.toCollection(() -> new ArrayList<>(x))),
				IntStream.range(1, y + 1).boxed().collect(Collectors.toCollection(() -> new ArrayList<>(y))));
		for (int xLowerBound = 0; xLowerBound <= x; ++xLowerBound) {
			for (int xUpperBound = xLowerBound; xUpperBound <= x; ++xUpperBound) {
				final int xLength = xUpperBound - xLowerBound;
				for (int yLowerBound = 0; yLowerBound < y; ++yLowerBound) {
					for (int yUpperBound = yLowerBound; yUpperBound <= y; ++yUpperBound) {
						final int yLength = yUpperBound - yLowerBound;
						final SpatialMap.Region region = getRegion(xLowerBound, xUpperBound, yLowerBound, yUpperBound);
						result.put(xLength, yLength, region);
					}
				}
			}
		}
		return result;
	}

	public Map<SpatialMap.Region, Set<SpatialMap.Region>> createValidMoveMap() {
		final Set<SpatialMap.Region> regionElements = elementPlacements.getMinimalRegionElements().keySet();
		final Map<SpatialMap.Region, Set<SpatialMap.Region>> result = Maps
				.newHashMapWithExpectedSize(regionElements.size());
		final int[] matrixDims = getDimensions();

		for (final SpatialMap.Region occupiedRegion : regionElements) {
			final Set<SpatialMap.Region> possibleMoveRegions = result.computeIfAbsent(occupiedRegion,
					newRegionPossibleMoveSetFactory);
			final int maxXLowerBound = matrixDims[0] - occupiedRegion.getLengthX();
			final int maxYLowerBound = matrixDims[1] - occupiedRegion.getLengthY();
			for (int xLowerBound = 0; xLowerBound < maxXLowerBound; xLowerBound++) {
				final int xUpperBound = xLowerBound + occupiedRegion.getLengthX();
				for (int yLowerBound = 0; yLowerBound < maxYLowerBound; yLowerBound++) {
					final int yUpperBound = yLowerBound + occupiedRegion.getLengthY();
					if (testCells(xLowerBound, xUpperBound, yLowerBound, yUpperBound, Objects::isNull)) {
						final SpatialMap.Region possibleMoveRegion = getRegion(xLowerBound, xUpperBound, yLowerBound,
								yUpperBound);
						possibleMoveRegions.add(possibleMoveRegion);
					} else {
						assert elementPlacements.isOccupied(occupiedRegion);
						if (LOGGER.isDebugEnabled()) {
							LOGGER.debug("Found occupied space at {}.",
									Arrays.toString(new int[] { xLowerBound, xUpperBound, yLowerBound, yUpperBound }));
						}
					}
				}

			}
		}
		return result;
	}

	public Stream<I> getCells(final SpatialMap.Region region) {
		return positionMatrix.getValues(region.getXLowerBound(), region.getXUpperBound(), region.getYLowerBound(),
				region.getYUpperBound());
	}

	/**
	 * @return
	 */
	public int[] getDimensions() {
		return positionMatrix.getDimensions();
	}

	/**
	 * @return the elementPlacements
	 */
	public SpatialMap<E> getElementPlacements() {
		return elementPlacements;
	}

	/**
	 * @return the position matrix
	 */
	public Matrix<I> getPositionMatrix() {
		return positionMatrix;
	}

	public SpatialMap.Region getRegion(final int xLowerBound, final int xUpperBound, final int yLowerBound,
			final int yUpperBound) {
		// TODO Implement caching, i.e. use a flyweight pattern?
		return new SpatialMap.Region(xLowerBound, xUpperBound, yLowerBound, yUpperBound);
	}

	public void placeElements(final Map<E, SpatialMap.Region> elementMoveTargets) {
		for (final Entry<E, SpatialMap.Region> elementMoveTarget : elementMoveTargets.entrySet()) {
			final E element = elementMoveTarget.getKey();
			final I elementId = elementIdGetter.apply(element);
			final SpatialMap.Region moveTarget = elementMoveTarget.getValue();
			LOGGER.debug("Moving element \"{}\" to {}.", elementId, moveTarget);
			assert !elementPlacements.isOccupied(moveTarget);
			setPositionValues(moveTarget, elementId);
			elementPlacements.put(element, moveTarget);
		}
	}

	public void setPositionValues(final SpatialMap.Region region, final I elementId) {
		LOGGER.debug("Setting {} to value \"{}\".", region, elementId);
		final ListIterator<List<I>> rowIter = positionMatrix.rowIterator(region.getXLowerBound());
		for (int rowIdx = rowIter.nextIndex(); rowIdx < region.getXUpperBound(); rowIdx++) {
			final List<I> row = rowIter.next();
			final ListIterator<I> rowCellIter = row.listIterator(region.getYLowerBound());
			for (int colIdx = rowCellIter.nextIndex(); colIdx < region.getYUpperBound(); colIdx++) {
				rowCellIter.next();
				rowCellIter.set(elementId);
			}
		}
	}

	public boolean testCells(final int xLowerBound, final int xUpperBound, final int yLowerBound, final int yUpperBound,
			final Predicate<? super I> cellPredicate) {
		boolean result = true;
		final ListIterator<List<I>> rowIter = positionMatrix.rowIterator(xLowerBound);
		for (int rowIdx = rowIter.nextIndex(); rowIdx < xUpperBound; rowIdx++) {
			final List<I> row = rowIter.next();
			final ListIterator<I> rowCellIter = row.listIterator(yLowerBound);
			for (int colIdx = rowCellIter.nextIndex(); colIdx < yUpperBound; colIdx++) {
				final I cellValue = rowCellIter.next();
				if (!cellPredicate.test(cellValue)) {
					result = false;
					break;
				}
			}
		}
		return result;
	}

	public boolean testCells(final SpatialMap.Region region, final Predicate<? super I> cellPredicate) {
		LOGGER.debug("Checking {}.", region);
		return testCells(region.getXLowerBound(), region.getXUpperBound(), region.getYLowerBound(),
				region.getYUpperBound(), cellPredicate);
	}

}
