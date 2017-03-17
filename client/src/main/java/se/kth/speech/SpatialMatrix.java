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
import java.util.Set;
import java.util.function.Function;
import java.util.function.IntFunction;
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

	private static int calculateSubRegionCount(final int x, final int xLength, final int y, final int yLength) {
		return (x - xLength + 1) * (y - yLength + 1);
	}

	private transient final Function<? super E, ? extends I> elementIdGetter;

	private final SpatialMap<E> elementPlacements;

	private transient final Function<SpatialRegion, Set<SpatialRegion>> newRegionPossibleMoveSetFactory;

	private final Matrix<I> positionMatrix;

	public SpatialMatrix(final Matrix<I> posMatrix, final Function<? super E, ? extends I> elementIdGetter,
			final SpatialMap<E> elementPlacements) {
		this.positionMatrix = posMatrix;
		this.elementIdGetter = elementIdGetter;
		this.elementPlacements = elementPlacements;
		newRegionPossibleMoveSetFactory = region -> {
			final int count = calculateSubRegionCount(region) - 1;
			return Sets.newHashSetWithExpectedSize(count);
		};
	}

	public int calculateSubRegionCount(final SpatialRegion region) {
		final int[] dims = positionMatrix.getDimensions();
		return calculateSubRegionCount(dims[0], region.getLengthX(), dims[1], region.getLengthY());
	}

	public void clearRegion(final SpatialRegion occupiedRegion) {
		final Collection<?> elements = elementPlacements.getMinimalRegionElements().get(occupiedRegion);
		// NOTE: Iterator.remove() for the instance returned by the
		// multimap's collection iterator throws a
		// ConcurrentModificationException
		elements.clear();
		setPositionValues(occupiedRegion, null);
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

	public <C extends Collection<? super SpatialRegion>> Table<Integer, Integer, C> createSizeIndexedRegionPowerSet(
			final IntFunction<? extends C> setFactory) {
		final int dims[] = positionMatrix.getDimensions();
		LOGGER.debug("Dims: {}", dims);
		final int x = dims[0];
		final int y = dims[1];
		final Table<Integer, Integer, C> result = ArrayTable.create(
				IntStream.rangeClosed(1, x).boxed().collect(Collectors.toCollection(() -> new ArrayList<>(x))),
				IntStream.rangeClosed(1, y).boxed().collect(Collectors.toCollection(() -> new ArrayList<>(y))));
		for (int xLowerBound = 0; xLowerBound < x; ++xLowerBound) {
			for (int xUpperBound = xLowerBound + 1; xUpperBound <= x; ++xUpperBound) {
				final int xLength = SpatialRegion.getLength(xLowerBound, xUpperBound);
				for (int yLowerBound = 0; yLowerBound < y; ++yLowerBound) {
					for (int yUpperBound = yLowerBound + 1; yUpperBound <= y; ++yUpperBound) {
						final int yLength = SpatialRegion.getLength(yLowerBound, yUpperBound);
						C regions = result.get(xLength, yLength);
						if (regions == null) {
							final int setSize = calculateSubRegionCount(xLength, yLength);
							regions = setFactory.apply(setSize);
							LOGGER.debug("Adding new set for size {}*{}", xLength, yLength);
							result.put(xLength, yLength, regions);
						}
						final SpatialRegion region = getRegion(xLowerBound, xUpperBound, yLowerBound, yUpperBound);
						regions.add(region);
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

	public Map<SpatialRegion, Set<SpatialRegion>> createValidMoveMap() {
		final Set<SpatialRegion> regionElements = elementPlacements.getMinimalRegionElements().keySet();
		final Map<SpatialRegion, Set<SpatialRegion>> result = Maps.newHashMapWithExpectedSize(regionElements.size());
		final int[] matrixDims = getDimensions();
//		System.out.println("matrixDims = " + Arrays.toString(matrixDims));
		final int x = matrixDims[0];
		final int y = matrixDims[1];

		for (final SpatialRegion movablePieceRegion : regionElements) {
			final Set<SpatialRegion> possibleMoveRegions = result.computeIfAbsent(movablePieceRegion,
					newRegionPossibleMoveSetFactory);
			final int maxStartingRowIdx = x - movablePieceRegion.getLengthX() + 1;
//			System.out.println("maxStartingRowIdx = " + maxStartingRowIdx);
			final int maxStartingColIdx = y - movablePieceRegion.getLengthY() + 1;
//			System.out.println("maxStartingColIdx = " + maxStartingColIdx);
			for (int startingRowIdx = 0; startingRowIdx < maxStartingRowIdx; startingRowIdx++) {
				final int endingRowIdx = startingRowIdx + movablePieceRegion.getLengthX();
				for (int startingColIdx = 0; startingColIdx < maxStartingColIdx; startingColIdx++) {
					final int endingColIdx = startingColIdx + movablePieceRegion.getLengthY();
					final SpatialRegion possibleMoveRegion = getRegion(startingRowIdx, endingRowIdx, startingColIdx,
							endingColIdx);
//					System.out.println(String.format("coords : %d %d %d %d", startingRowIdx, endingRowIdx, startingColIdx, endingColIdx));
					if (elementPlacements.isOccupied(possibleMoveRegion)) {
						LOGGER.debug("Found occupied space at {}.", Arrays
								.toString(new int[] { startingRowIdx, endingRowIdx, startingColIdx, startingColIdx }));
					} else {
						possibleMoveRegions.add(possibleMoveRegion);
					}
				}

			}
		}
		return result;
	}

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

	public Stream<I> getCells() {
		return positionMatrix.getValues().stream();
	}

	public Stream<I> getCells(final SpatialRegion region) {
		return positionMatrix.getValues(region.getRowStartIdx(), region.getRowEndIdx(), region.getColumnStartIdx(),
				region.getColumnEndIdx());
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

	public SpatialRegion getRegion(final int xLowerBound, final int xUpperBound, final int yLowerBound,
			final int yUpperBound) {
		// TODO Implement caching, i.e. use a flyweight pattern?
		return new SpatialRegion(xLowerBound, xUpperBound, yLowerBound, yUpperBound);
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
	public boolean isOccupied(final SpatialRegion result) {
		return elementPlacements.isOccupied(result);
	}

	public void placeElement(final E element, final SpatialRegion target) {
		final I elementId = elementIdGetter.apply(element);
		LOGGER.debug("Placing element \"{}\" at {}.", elementId, target);
		assert !elementPlacements.isOccupied(target);
		setPositionValues(target, elementId);
		elementPlacements.put(element, target);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder();
		builder.append("SpatialMatrix [positionMatrix=");
		builder.append(positionMatrix);
		builder.append("]");
		return builder.toString();
	}

	private void setPositionValues(final SpatialRegion region, final I elementId) {
		LOGGER.debug("Setting {} to value \"{}\".", region, elementId);
		final ListIterator<List<I>> rowIter = positionMatrix.rowIterator(region.getRowStartIdx());
		for (int rowIdx = rowIter.nextIndex(); rowIdx < region.getRowEndIdx(); rowIdx++) {
			final List<I> row = rowIter.next();
			final ListIterator<I> rowCellIter = row.listIterator(region.getColumnStartIdx());
			for (int colIdx = rowCellIter.nextIndex(); colIdx < region.getColumnEndIdx(); colIdx++) {
				rowCellIter.next();
				rowCellIter.set(elementId);
			}
		}
	}

	int calculateSubRegionCount(final int xLength, final int yLength) {
		final int[] dims = positionMatrix.getDimensions();
		return calculateSubRegionCount(dims[0], xLength, dims[1], yLength);
	}

}
