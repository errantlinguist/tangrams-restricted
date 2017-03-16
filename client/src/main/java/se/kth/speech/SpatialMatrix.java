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
import java.util.LinkedHashSet;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
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

	private transient final Function<? super E, ? extends I> elementIdGetter;

	private final SpatialMap<E> elementPlacements;

	private transient final Function<SpatialMap.Region, Set<SpatialMap.Region>> newRegionPossibleMoveSetFactory;

	private final Matrix<I> positionMatrix;

	public SpatialMatrix(final Matrix<I> posMatrix, final Function<? super E, ? extends I> elementIdGetter,
			final SpatialMap<E> elementPlacements) {
		this.positionMatrix = posMatrix;
		this.elementIdGetter = elementIdGetter;
		this.elementPlacements = elementPlacements;
		newRegionPossibleMoveSetFactory = region -> {
			final int occupiedRegionArea = region.getLengthX() * region.getLengthY();
			return Sets.newHashSetWithExpectedSize(posMatrix.getValues().size() / occupiedRegionArea);
		};
	}

	public void clearRegion(final SpatialMap.Region occupiedRegion) {
		final Collection<?> elements = elementPlacements.getMinimalRegionElements().get(occupiedRegion);
		// NOTE: Iterator.remove() for the instance returned by the
		// multimap's collection iterator throws a
		// ConcurrentModificationException
		elements.clear();
		setPositionValues(occupiedRegion, null);
	}

	public Table<Integer, Integer, Set<SpatialMap.Region>> createSizeIndexedRegionPowerSet() {
		final int dims[] = positionMatrix.getDimensions();
		LOGGER.debug("Dims: {}", dims);
		final int x = dims[0];
		final int y = dims[1];
		final Table<Integer, Integer, Set<SpatialMap.Region>> result = ArrayTable.create(
				IntStream.rangeClosed(1, x).boxed().collect(Collectors.toCollection(() -> new ArrayList<>(x))),
				IntStream.rangeClosed(1, y).boxed().collect(Collectors.toCollection(() -> new ArrayList<>(y))));
		for (int xLowerBound = 0; xLowerBound < x; ++xLowerBound) {
			for (int xUpperBound = xLowerBound; xUpperBound < x; ++xUpperBound) {
				final int xLength = xUpperBound - xLowerBound + 1;
				for (int yLowerBound = 0; yLowerBound < y; ++yLowerBound) {
					for (int yUpperBound = yLowerBound; yUpperBound < y; ++yUpperBound) {
						final int yLength = yUpperBound - yLowerBound + 1;
						Set<SpatialMap.Region> regions = result.get(xLength, yLength);
						if (regions == null) {
							// TODO: set capacity
							// Linked in order to preserve iteration order
							regions = new LinkedHashSet<>();
							LOGGER.debug("putting {}*{}", xLength, yLength);
							result.put(xLength, yLength, regions);
						}
						final SpatialMap.Region region = getRegion(xLowerBound, xUpperBound, yLowerBound, yUpperBound);
						regions.add(region);
					}
				}
			}
		}
		return result;
	}

	// public void addRegionPowerSet(final Collection<? super SpatialMap.Region>
	// regions) {
	// final int dims[] = positionMatrix.getDimensions();
	// final int x = dims[0];
	// final int y = dims[1];
	// for (int xLowerBound = 0; xLowerBound <= x; ++xLowerBound) {
	// for (int xUpperBound = xLowerBound; xUpperBound <= x; ++xUpperBound) {
	// for (int yLowerBound = 0; yLowerBound <= y; ++yLowerBound) {
	// for (int yUpperBound = yLowerBound; yUpperBound <= y; ++yUpperBound) {
	// final SpatialMap.Region region = getRegion(xLowerBound, xUpperBound,
	// yLowerBound, yUpperBound);
	// regions.add(region);
	// }
	// }
	// }
	// }
	// }
	//
	// public int calculateRegionPowerSetSize() {
	// final int dims[] = positionMatrix.getDimensions();
	// // m(m+1)n(n+1)/4 http://stackoverflow.com/a/17927830/1391325
	// final int m = dims[0];
	// final int n = dims[1];
	// return m * (m + 1) * n * (n + 1) / 4;
	// }

	public Map<SpatialMap.Region, Set<SpatialMap.Region>> createValidMoveMap() {
		final Set<SpatialMap.Region> regionElements = elementPlacements.getMinimalRegionElements().keySet();
		final Map<SpatialMap.Region, Set<SpatialMap.Region>> result = Maps
				.newHashMapWithExpectedSize(regionElements.size());
		final int[] matrixDims = getDimensions();
		final int x = matrixDims[0];
		final int y = matrixDims[1];

		for (final SpatialMap.Region movablePieceRegion : regionElements) {
			final Set<SpatialMap.Region> possibleMoveRegions = result.computeIfAbsent(movablePieceRegion,
					newRegionPossibleMoveSetFactory);
			final int maxXLowerBound = x - movablePieceRegion.getLengthX();
			final int maxYLowerBound = y - movablePieceRegion.getLengthY();
			for (int xLowerBound = 0; xLowerBound < maxXLowerBound; xLowerBound++) {
				final int xUpperBound = xLowerBound + movablePieceRegion.getLengthX();
				for (int yLowerBound = 0; yLowerBound < maxYLowerBound; yLowerBound++) {
					final int yUpperBound = yLowerBound + movablePieceRegion.getLengthY();
					final SpatialMap.Region possibleMoveRegion = getRegion(xLowerBound, xUpperBound, yLowerBound,
							yUpperBound);
					if (elementPlacements.isOccupied(possibleMoveRegion)) {
						if (LOGGER.isDebugEnabled()) {
							LOGGER.debug("Found occupied space at {}.",
									Arrays.toString(new int[] { xLowerBound, xUpperBound, yLowerBound, yUpperBound }));
						}
					} else {
						possibleMoveRegions.add(possibleMoveRegion);
					}
				}

			}
		}
		return result;
	}

	// public Set<SpatialMap.Region> createRegionPowerSet() {
	// return createRegionPowerSet(Sets::newHashSetWithExpectedSize);
	// }
	//
	// public <C extends Collection<? super SpatialMap.Region>> C
	// createRegionPowerSet(
	// final IntFunction<? extends C> collFactory) {
	// final int subRegionCount = calculateRegionPowerSetSize();
	// final C result = collFactory.apply(subRegionCount);
	// addRegionPowerSet(result);
	// return result;
	// }

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(final Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (!(obj instanceof SpatialMatrix)) {
			return false;
		}
		final SpatialMatrix<?, ?> other = (SpatialMatrix<?, ?>) obj;
		if (elementPlacements == null) {
			if (other.elementPlacements != null) {
				return false;
			}
		} else if (!elementPlacements.equals(other.elementPlacements)) {
			return false;
		}
		if (positionMatrix == null) {
			if (other.positionMatrix != null) {
				return false;
			}
		} else if (!positionMatrix.equals(other.positionMatrix)) {
			return false;
		}
		return true;
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

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (elementPlacements == null ? 0 : elementPlacements.hashCode());
		result = prime * result + (positionMatrix == null ? 0 : positionMatrix.hashCode());
		return result;
	}

	/**
	 * @param result
	 * @return
	 */
	public boolean isOccupied(final SpatialMap.Region result) {
		return elementPlacements.isOccupied(result);
	}

	public void placeElement(final E element, final SpatialMap.Region target) {
		final I elementId = elementIdGetter.apply(element);
		LOGGER.debug("Placing element \"{}\" at {}.", elementId, target);
		assert !elementPlacements.isOccupied(target);
		setPositionValues(target, elementId);
		elementPlacements.put(element, target);
	}

	private void setPositionValues(final SpatialMap.Region region, final I elementId) {
		LOGGER.debug("Setting {} to value \"{}\".", region, elementId);
		final ListIterator<List<I>> rowIter = positionMatrix.rowIterator(region.getXLowerBound());
		for (int rowIdx = rowIter.nextIndex(); rowIdx <= region.getXUpperBound(); rowIdx++) {
			final List<I> row = rowIter.next();
			final ListIterator<I> rowCellIter = row.listIterator(region.getYLowerBound());
			for (int colIdx = rowCellIter.nextIndex(); colIdx <= region.getYUpperBound(); colIdx++) {
				rowCellIter.next();
				rowCellIter.set(elementId);
			}
		}
	}

}
