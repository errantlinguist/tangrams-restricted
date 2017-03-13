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

import java.util.List;
import java.util.ListIterator;
import java.util.function.Predicate;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.Matrix;
import se.kth.speech.SpatialMap;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 10 Mar 2017
 *
 */
final class SpatialMatrix<T> {

	private static final Logger LOGGER = LoggerFactory.getLogger(SpatialMatrix.class);

	private final Matrix<T> posMatrix;

	SpatialMatrix(final Matrix<T> posMatrix) {
		this.posMatrix = posMatrix;
	}

	/**
	 * @return
	 */
	public int[] getDimensions() {
		return posMatrix.getDimensions();
	}

	Stream<T> getCells(final SpatialMap.Region region) {
		final Stream.Builder<T> resultBuilder = Stream.builder();
		final ListIterator<List<T>> rowIter = posMatrix.rowIterator(region.getXLowerBound());
		for (int rowIdx = rowIter.nextIndex(); rowIdx < region.getXUpperBound(); rowIdx++) {
			final List<T> row = rowIter.next();
			final ListIterator<T> rowCellIter = row.listIterator(region.getYLowerBound());
			for (int colIdx = rowCellIter.nextIndex(); colIdx < region.getYUpperBound(); colIdx++) {
				final T cellValue = rowCellIter.next();
				resultBuilder.accept(cellValue);
			}
		}
		return resultBuilder.build();
	}
	
	// TODO: Finish
//	void checkNext(final SpatialMap.Region region){
//		final ListIterator<List<T>> rowIter = posMatrix.rowIterator();
//		final int contiguousRowCount = region.getXUpperBound();
//		final int contiguousColCount = region.getYUpperBound();
//		rowSearch: while (rowIter.hasNext()){
//			int rowIdx = rowIter.nextIndex();
//			final List<T> row = rowIter.next();
//			final ListIterator<T> rowCellIter = row.listIterator(region.getYLowerBound());
//			colSearch: while (rowCellIter.hasNext()) {
//				final int colIdx = rowCellIter.nextIndex();
//				final T cellValue = rowCellIter.next();
//				if (cellValue != null){
//					// Stop
//					break colSearch;
//				}
//			}
//			
//		}
//		for (int rowIdx = rowIter.nextIndex(); rowIdx < ; rowIdx++) {
//			
//			final ListIterator<T> rowCellIter = row.listIterator(region.getYLowerBound());
//			for (int colIdx = rowCellIter.nextIndex(); colIdx < region.getYUpperBound(); colIdx++) {
//				final T cellValue = rowCellIter.next();
//			}
//		}		
//	}

	/**
	 * @return the posMatrix
	 */
	Matrix<T> getPosMatrix() {
		return posMatrix;
	}

	void setPositionValues(final SpatialMap.Region region, final T pieceId) {
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

	boolean testCells(final SpatialMap.Region region, final Predicate<? super T> cellPredicate) {
		boolean result = true;
		LOGGER.debug("Checking {}.", region);
		final ListIterator<List<T>> rowIter = posMatrix.rowIterator(region.getXLowerBound());
		for (int rowIdx = rowIter.nextIndex(); rowIdx < region.getXUpperBound(); rowIdx++) {
			final List<T> row = rowIter.next();
			final ListIterator<T> rowCellIter = row.listIterator(region.getYLowerBound());
			for (int colIdx = rowCellIter.nextIndex(); colIdx < region.getYUpperBound(); colIdx++) {
				final T cellValue = rowCellIter.next();
				if (!cellPredicate.test(cellValue)) {
					result = false;
					break;
				}
			}
		}
		return result;
	}

}
