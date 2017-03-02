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

import java.util.Arrays;
import java.util.ListIterator;
import java.util.NoSuchElementException;

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

	private class RowIterator implements ListIterator<T[]> {

		private final int maxRowValArrStartIdx = getRowValueArrayStartIdxForValueArrayIdx(values.length);

		private int nextRowIdx = 0;

		private int rowValArrStartIdx = 0;

		@Override
		public void add(final T[] e) {
			throw new UnsupportedOperationException();
		}

		@Override
		public boolean hasNext() {
			return rowValArrStartIdx <= maxRowValArrStartIdx;
		}

		@Override
		public boolean hasPrevious() {
			return rowValArrStartIdx > 0;
		}

		@Override
		public T[] next() {
			if (!hasNext()) {
				throw new NoSuchElementException();
			}
			final int cols = getColCount();
			final int rowValArrEndIdx = rowValArrStartIdx + cols;
			final T[] result = Arrays.copyOfRange(values, rowValArrStartIdx, rowValArrEndIdx);
			rowValArrStartIdx = rowValArrEndIdx;
			nextRowIdx++;
			return result;
		}

		@Override
		public int nextIndex() {
			return nextRowIdx;
		}

		@Override
		public T[] previous() {
			if (!hasPrevious()) {
				throw new NoSuchElementException();
			}
			final int cols = getColCount();
			final int rowValArrEndIdx = rowValArrStartIdx;
			rowValArrStartIdx -= cols;
			final T[] result = Arrays.copyOfRange(values, rowValArrStartIdx, rowValArrEndIdx);
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
		public void set(final T[] e) {
			final int cols = getColCount();
			if (e.length != cols) {
				throw new IllegalArgumentException(String.format(
						"Supplied array length is %d but does not match matrix column count of %d.", e.length, cols));
			}
			int valArrIdx = rowValArrStartIdx;
			for (final T val : e) {
				values[valArrIdx++] = val;
			}
		}
	}

	private static <T> T[] copy(final T[] arr) {
		return Arrays.copyOf(arr, arr.length);
	}

	private final int[] dims;

	private final T[] values;

	public Matrix(final Matrix<? extends T> copyee) {
		this(copy(copyee.getValues()), copyee.getDimensions()[1]);
	}

	public Matrix(final T[] values, final int colCount) {
		if (colCount < 1) {
			throw new IllegalArgumentException(String.format("Column count is %d but must be positive.", colCount));
		}
		if (values.length < colCount) {
			throw new IllegalArgumentException(
					String.format("Column count (%d) is greater than the coordinate point vector length.", colCount));
		}
		{
			final int lengthRem = values.length % colCount;
			if (lengthRem > 0) {
				throw new IllegalArgumentException(String.format(
						"The coordinate point vector length is invalid: It has %d too few elements in order to form a proper 2D matrix.",
						lengthRem));
			}
		}
		this.values = values;
		this.dims = new int[] { values.length / colCount, colCount };
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
		if (!Arrays.equals(dims, other.dims)) {
			return false;
		}
		if (!Arrays.equals(values, other.values)) {
			return false;
		}
		return true;
	}

	/**
	 * @return a copy of the dims
	 */
	public int[] getDimensions() {
		return Arrays.copyOf(dims, dims.length);
	}

	public int[] getMatrixIndices(final int valueArrayIdx) {
		final int row = getRowIdx(valueArrayIdx);
		final int col = getColIdx(valueArrayIdx);
		return new int[] { row, col };
	}

	public T getValue(final int[] coords) {
		return values[getValueArrayIdx(coords)];
	}

	public T[] getValues() {
		return values;
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
		result = prime * result + Arrays.hashCode(dims);
		result = prime * result + Arrays.hashCode(values);
		return result;
	}

	public ListIterator<T[]> rowIterator() {
		return new RowIterator();
	}

	public T setValue(final int[] coords, final T value) {
		final T[] values = getValues();
		final int valueArrayIdx = getValueArrayIdx(coords);
		final T result = values[valueArrayIdx];
		values[valueArrayIdx] = value;
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
		builder.append("Matrix [dims=");
		builder.append(Arrays.toString(dims));
		builder.append(", values=");
		builder.append(Arrays.toString(values));
		builder.append("]");
		return builder.toString();
	}

	private int getColCount() {
		return getDimensions()[1];
	}

	private int getColIdx(final int valueArrayIdx) {
		final int cols = getColCount();
		return valueArrayIdx % cols;
	}

	private int getRowIdx(final int valueArrayIdx) {
		final int cols = getColCount();
		return valueArrayIdx / cols;
	}

	private int getRowValueArrayStartIdxForValueArrayIdx(final int valueArrayIdx) {
		final int col = getColIdx(valueArrayIdx);
		return valueArrayIdx - col;
	}

	private int getValueArrayIdx(final int[] coords) {
		final int rowStartIdx = coords[0] * getColCount();
		return rowStartIdx + coords[1];
	}

}
