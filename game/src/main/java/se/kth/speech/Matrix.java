/*
 *  This file is part of tangrams.
 *
 *  tangrams is free software: you can redistribute it and/or modify
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
import java.util.List;
import java.util.ListIterator;
import java.util.NoSuchElementException;
import java.util.stream.Stream;

/**
 * <strong>NOTE:</strong> This class is generic in order to emphasize the fact
 * that the value of the cells themselves is irrelevant; Only their positions in
 * relation to one another are.
 *
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 13 Jan 2017
 * @param <T>
 *            The type used to represent matrix cell data.
 */
public final class Matrix<T> {

	private class RowIterator implements ListIterator<List<T>> {

		private int nextRowIdx;

		private int rowValArrStartIdx;

		public RowIterator() {
			this(0);
		}

		/**
		 * @param rowIdx
		 *            The index of the row to start at.
		 */
		public RowIterator(final int rowIdx) {
			this.nextRowIdx = rowIdx;
			this.rowValArrStartIdx = getRowValueArrayStartIdx(rowIdx);
		}

		@Override
		public void add(final List<T> e) {
			throw new UnsupportedOperationException();
		}

		@Override
		public boolean hasNext() {
			return rowValArrStartIdx < values.size();
		}

		@Override
		public boolean hasPrevious() {
			return rowValArrStartIdx > 0;
		}

		@Override
		public List<T> next() {
			if (!hasNext()) {
				throw new NoSuchElementException();
			}
			final int cols = getColCount();
			final int rowValArrEndIdx = rowValArrStartIdx + cols;
			final List<T> result = values.subList(rowValArrStartIdx, rowValArrEndIdx);
			rowValArrStartIdx = rowValArrEndIdx;
			nextRowIdx++;
			return result;
		}

		@Override
		public int nextIndex() {
			return nextRowIdx;
		}

		@Override
		public List<T> previous() {
			if (!hasPrevious()) {
				throw new NoSuchElementException();
			}
			final int cols = getColCount();
			final int rowValArrEndIdx = rowValArrStartIdx;
			rowValArrStartIdx -= cols;
			final List<T> result = values.subList(rowValArrStartIdx, rowValArrEndIdx);
			nextRowIdx--;
			return result;
		}

		@Override
		public int previousIndex() {
			return nextRowIdx - 1;
		}

		@Override
		public void remove() {
			throw new UnsupportedOperationException();
		}

		@Override
		public void set(final List<T> e) {
			final int cols = getColCount();
			if (e.size() != cols) {
				throw new IllegalArgumentException(String.format(
						"Supplied array length is %d but does not match matrix column count of %d.", e.size(), cols));
			}
			int valArrIdx = rowValArrStartIdx;
			for (final T val : e) {
				values.set(valArrIdx++, val);
			}
		}
	}

	private final int colCount;

	private final List<T> values;

	public Matrix(final List<T> values, final int colCount) {
		if (colCount < 1) {
			throw new IllegalArgumentException(String.format("Column count is %d but must be positive.", colCount));
		}
		if (values.size() < colCount) {
			throw new IllegalArgumentException(
					String.format("Column count (%d) is greater than the coordinate point vector length.", colCount));
		}
		this.colCount = colCount;
		{
			final int lengthRem = values.size() % colCount;
			if (lengthRem > 0) {
				throw new IllegalArgumentException(String.format(
						"The coordinate point vector length is invalid: It has %d too few elements in order to form a proper 2D matrix.",
						lengthRem));
			}
		}
		this.values = values;
	}

	public Matrix(final Matrix<T> copyee) {
		this(copyee.getValues(), copyee.getColCount());
	}

	public Matrix(final T[] values, final int colCount) {
		this(Arrays.asList(values), colCount);
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
		if (!(obj instanceof Matrix)) {
			return false;
		}
		final Matrix<?> other = (Matrix<?>) obj;
		if (colCount != other.colCount) {
			return false;
		}
		if (values == null) {
			if (other.values != null) {
				return false;
			}
		} else if (!values.equals(other.values)) {
			return false;
		}
		return true;
	}

	public List<T> getColumn(final int colIdx) {
		final List<T> result = new ArrayList<>(getDimensions()[0]);
		for (final ListIterator<List<T>> rowIter = rowIterator(); rowIter.hasNext();) {
			final List<T> row = rowIter.next();
			final T rowColValue = row.get(colIdx);
			result.add(rowColValue);
		}
		return result;
	}

	/**
	 * @return a copy of the dims
	 */
	public int[] getDimensions() {
		final int cellCount = values.size();
		return new int[] { cellCount / colCount, colCount };
	}

	public int[] getMatrixIndices(final int valueArrayIdx) {
		final int row = getRowIdx(valueArrayIdx);
		final int col = getColIdx(valueArrayIdx);
		return new int[] { row, col };
	}

	public List<T> getRow(final int rowIdx) {
		final int rowStartIdx = getRowValueArrayStartIdx(rowIdx);
		return values.subList(rowStartIdx, rowStartIdx + getColCount());
	}

	public T getValue(final int[] coords) {
		return values.get(getValueArrayIdx(coords));
	}

	public List<T> getValues() {
		return values;
	}

	public Stream<T> getValues(final int xLowerBound, final int xUpperBound, final int yLowerBound,
			final int yUpperBound) {
		final Stream.Builder<T> resultBuilder = Stream.builder();
		final ListIterator<List<T>> rowIter = rowIterator(xLowerBound);
		for (int rowIdx = rowIter.nextIndex(); rowIdx < xUpperBound; rowIdx++) {
			final List<T> row = rowIter.next();
			final ListIterator<T> rowCellIter = row.listIterator(yLowerBound);
			for (int colIdx = rowCellIter.nextIndex(); colIdx < yUpperBound; colIdx++) {
				final T cellValue = rowCellIter.next();
				resultBuilder.accept(cellValue);
			}
		}
		return resultBuilder.build();
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
		result = prime * result + colCount;
		result = prime * result + (values == null ? 0 : values.hashCode());
		return result;
	}

	public ListIterator<List<T>> rowIterator() {
		return new RowIterator();
	}

	public ListIterator<List<T>> rowIterator(final int rowIdx) {
		return new RowIterator(rowIdx);
	}

	public T setValue(final int[] coords, final T value) {
		final List<T> values = getValues();
		final int valueArrayIdx = getValueArrayIdx(coords);
		final T result = values.set(valueArrayIdx, value);
		return result;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder();
		builder.append("Matrix [getDimensions()=");
		builder.append(Arrays.toString(getDimensions()));
		builder.append(", values=");
		builder.append(values);
		builder.append("]");
		return builder.toString();
	}

	private int getColCount() {
		return colCount;
	}

	private int getColIdx(final int valueArrayIdx) {
		final int cols = getColCount();
		return valueArrayIdx % cols;
	}

	private int getRowIdx(final int valueArrayIdx) {
		final int cols = getColCount();
		return valueArrayIdx / cols;
	}

	private int getRowValueArrayStartIdx(final int rowIdx) {
		return rowIdx * getColCount();
	}

	private int getValueArrayIdx(final int[] coords) {
		final int rowStartIdx = getRowValueArrayStartIdx(coords[0]);
		return rowStartIdx + coords[1];
	}

}
