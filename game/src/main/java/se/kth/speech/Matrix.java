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
	
	private class RowListIterator extends AbstractRowIterator<List<T>> {
		
		/* (non-Javadoc)
		 * @see se.kth.speech.Matrix.AbstractRowIterator#getRow(int, int)
		 */
		@Override
		protected List<T> getRow(int rowValArrStartIdx, int rowValArrEndIdx) {
			return values.subList(rowValArrStartIdx, rowValArrEndIdx);
		}

		/* (non-Javadoc)
		 * @see se.kth.speech.Matrix.AbstractRowIterator#getRowLength(java.lang.Iterable)
		 */
		@Override
		protected int getRowLength(List<T> row) {
			return row.size();
		}
		
	}

	private abstract class AbstractRowIterator<R extends Iterable<T>> implements ListIterator<R> {
		
		protected abstract R getRow(int rowValArrStartIdx, final int rowValArrEndIdx);
		
		protected abstract int getRowLength(R row);
		
		private final int maxRowValArrStartIdx = getRowValueArrayStartIdxForValueArrayIdx(values.size());

		private int nextRowIdx = 0;

		private int rowValArrStartIdx = 0;

		@Override
		public void add(final R e) {
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
		public R next() {
			if (!hasNext()) {
				throw new NoSuchElementException();
			}
			final int cols = getColCount();
			final int rowValArrEndIdx = rowValArrStartIdx + cols;
//			final T[] result = Arrays.copyOfRange(values, rowValArrStartIdx, rowValArrEndIdx);
			R result = getRow(rowValArrEndIdx, rowValArrEndIdx);
			rowValArrStartIdx = rowValArrEndIdx;
			nextRowIdx++;
			return result;
		}

		@Override
		public int nextIndex() {
			return nextRowIdx;
		}

		@Override
		public R previous() {
			if (!hasPrevious()) {
				throw new NoSuchElementException();
			}
			final int cols = getColCount();
			final int rowValArrEndIdx = rowValArrStartIdx;
			rowValArrStartIdx -= cols;
//			final T[] result = Arrays.copyOfRange(values, rowValArrStartIdx, rowValArrEndIdx);
			R result = getRow(rowValArrEndIdx, rowValArrEndIdx);
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
		public void set(final R e) {
			final int cols = getColCount();
			int rowLength = getRowLength(e);
			if (getRowLength(e) != cols) {
				throw new IllegalArgumentException(String.format(
						"Supplied array length is %d but does not match matrix column count of %d.", rowLength, cols));
			}
			int valArrIdx = rowValArrStartIdx;
			for (final T val : e) {
				values.set(valArrIdx++, val);
			}
		}
	}

	private final int colCount;

	private final List<T> values;

	public Matrix(final Matrix<T> copyee) {
		this(copyee.getValues(), copyee.getDimensions()[1]);
	}
	
	public Matrix(final T[] values, final int colCount) {
		this(Arrays.asList(values), colCount);
	}

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

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof Matrix))
			return false;
		Matrix other = (Matrix) obj;
		if (colCount != other.colCount)
			return false;
		if (values == null) {
			if (other.values != null)
				return false;
		} else if (!values.equals(other.values))
			return false;
		return true;
	}

	/**
	 * @return a copy of the dims
	 */
	public int[] getDimensions() {
		return new int[] { values.size() / colCount, colCount };
	}

	public int[] getMatrixIndices(final int valueArrayIdx) {
		final int row = getRowIdx(valueArrayIdx);
		final int col = getColIdx(valueArrayIdx);
		return new int[] { row, col };
	}

	public T getValue(final int[] coords) {
		return values.get(getValueArrayIdx(coords));
	}

//	public T getColValues(final int colIdx) {
//		// Get the index of the last row in the matrix
//		int rowCount = getRowIdx(values.size());
//		List<T> result = new ArrayList<>(rowCount);
//		for (RowListIterator rowIter = new RowListIterator(); rowIter.hasNext();) {
//			List<T> nextRow = rowIter.next();
//		}
//	}

	public List<T> getValues() {
		return values;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + colCount;
		result = prime * result + ((values == null) ? 0 : values.hashCode());
		return result;
	}

	public ListIterator<List<T>> rowIterator() {
		return new RowListIterator();
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

	private int getRowValueArrayStartIdxForValueArrayIdx(final int valueArrayIdx) {
		final int col = getColIdx(valueArrayIdx);
		return valueArrayIdx - col;
	}

	private int getValueArrayIdx(final int[] coords) {
		final int rowStartIdx = coords[0] * getColCount();
		return rowStartIdx + coords[1];
	}

}
