/*
 *  This file is part of Tangrams-restricted.
 *
 *  Tangrams-restricted is free software: you can redistribute it and/or modify
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

import java.util.Iterator;
import java.util.List;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

/**
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since 17 Mar 2017
 *
 */
public final class MatrixStringReprFactory implements Function<Matrix<?>, String> {

	private static final Collector<CharSequence, ?, String> TABLE_ROW_CELL_JOINER;

	private static final String TABLE_STRING_REPR_COL_DELIMITER;

	private static final String TABLE_STRING_REPR_ROW_DELIMITER = System.lineSeparator();

	static {
		TABLE_STRING_REPR_COL_DELIMITER = "\t";
		TABLE_ROW_CELL_JOINER = Collectors.joining(TABLE_STRING_REPR_COL_DELIMITER);
	}

	private static void appendRowTableRepr(final int rowIdx, final Iterator<?> rowCellIter, final StringBuilder sb) {
		sb.append(rowIdx);
		final String nullValRepr = "-";
		while (rowCellIter.hasNext()) {
			sb.append(TABLE_STRING_REPR_COL_DELIMITER);
			final Object next = rowCellIter.next();
			final String nextRepr = Objects.toString(next, nullValRepr);
			sb.append(nextRepr);
		}
	}

	@Override
	public String apply(final Matrix<?> matrix) {
		final Stream<String> colNames = Stream.of("ROW_IDX", "COL_IDXS...");
		final String header = colNames.collect(TABLE_ROW_CELL_JOINER);

		final int[] dims = matrix.getDimensions();
		final Stream.Builder<String> subheaderBuilder = Stream.builder();
		subheaderBuilder.accept("");
		IntStream.range(0, dims[1]).mapToObj(Integer::toString).forEachOrdered(subheaderBuilder);
		final String subHeader = TABLE_STRING_REPR_ROW_DELIMITER
				+ subheaderBuilder.build().collect(TABLE_ROW_CELL_JOINER);
		final int cellCount = matrix.getValues().size();
		final StringBuilder sb = new StringBuilder(header.length() + subHeader.length() + cellCount * 16);
		sb.append(header);
		sb.append(subHeader);
		final Iterator<? extends List<?>> rowIter = matrix.rowIterator();
		int rowIdx = 0;
		if (rowIter.hasNext()) {
			do {
				sb.append(TABLE_STRING_REPR_ROW_DELIMITER);
				final List<?> row = rowIter.next();
				appendRowTableRepr(rowIdx++, row.iterator(), sb);
			} while (rowIter.hasNext());
		}
		return sb.toString();
	}

}
