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

import java.util.List;
import java.util.ListIterator;
import java.util.function.Predicate;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.SpatialMap.Region;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 10 Mar 2017
 *
 */
public final class SpatialMatrix<T> {

	private static final Logger LOGGER = LoggerFactory.getLogger(SpatialMatrix.class);

	private final Matrix<T> posMatrix;

	public SpatialMatrix(final Matrix<T> posMatrix) {
		this.posMatrix = posMatrix;
	}

	public Stream<T> getCells(final SpatialMap.Region region) {
		return posMatrix.getValues(region.getXLowerBound(), region.getXUpperBound(), region.getYLowerBound(),
				region.getYUpperBound());
	}

	/**
	 * @return
	 */
	public int[] getDimensions() {
		return posMatrix.getDimensions();
	}

	/**
	 * @return the posMatrix
	 */
	public Matrix<T> getPosMatrix() {
		return posMatrix;
	}

	public Region getRegion(final int xLowerBound, final int xUpperBound, final int yLowerBound,
			final int yUpperBound) {
		// TODO Implement caching, i.e. use a flyweight pattern?
		return new SpatialMap.Region(xLowerBound, xUpperBound, yLowerBound, yUpperBound);
	}

	public void setPositionValues(final SpatialMap.Region region, final T pieceId) {
		LOGGER.debug("Setting {} to value \"{}\".", region, pieceId);
		final ListIterator<List<T>> rowIter = posMatrix.rowIterator(region.getXLowerBound());
		for (int rowIdx = rowIter.nextIndex(); rowIdx < region.getXUpperBound(); rowIdx++) {
			final List<T> row = rowIter.next();
			final ListIterator<T> rowCellIter = row.listIterator(region.getYLowerBound());
			for (int colIdx = rowCellIter.nextIndex(); colIdx < region.getYUpperBound(); colIdx++) {
				rowCellIter.next();
				rowCellIter.set(pieceId);
			}
		}
	}

	public boolean testCells(final int xLowerBound, final int xUpperBound, final int yLowerBound, final int yUpperBound,
			final Predicate<? super T> cellPredicate) {
		boolean result = true;
		final ListIterator<List<T>> rowIter = posMatrix.rowIterator(xLowerBound);
		for (int rowIdx = rowIter.nextIndex(); rowIdx < xUpperBound; rowIdx++) {
			final List<T> row = rowIter.next();
			final ListIterator<T> rowCellIter = row.listIterator(yLowerBound);
			for (int colIdx = rowCellIter.nextIndex(); colIdx < yUpperBound; colIdx++) {
				final T cellValue = rowCellIter.next();
				if (!cellPredicate.test(cellValue)) {
					result = false;
					break;
				}
			}
		}
		return result;
	}

	public boolean testCells(final SpatialMap.Region region, final Predicate<? super T> cellPredicate) {
		LOGGER.debug("Checking {}.", region);
		return testCells(region.getXLowerBound(), region.getXUpperBound(), region.getYLowerBound(),
				region.getYUpperBound(), cellPredicate);
	}

}
