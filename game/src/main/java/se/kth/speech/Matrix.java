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
import java.util.List;

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
		this(copyee.getValues(), copyee.getDimensions()[1]);
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
