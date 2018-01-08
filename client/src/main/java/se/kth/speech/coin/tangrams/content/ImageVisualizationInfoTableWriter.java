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
package se.kth.speech.coin.tangrams.content;

import java.io.IOException;
import java.io.Writer;
import java.util.Iterator;
import java.util.List;
import java.util.Map.Entry;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since 17 Mar 2017
 *
 */
public final class ImageVisualizationInfoTableWriter {

	private static final Collector<CharSequence, ?, String> TABLE_ROW_CELL_JOINER;

	private static final Collector<CharSequence, ?, String> TABLE_ROW_JOINER;

	private static final String TABLE_STRING_REPR_COL_DELIMITER;

	private static final String TABLE_STRING_REPR_ROW_DELIMITER;

	static {
		TABLE_STRING_REPR_ROW_DELIMITER = System.lineSeparator();
		TABLE_ROW_JOINER = Collectors.joining(TABLE_STRING_REPR_ROW_DELIMITER);

		TABLE_STRING_REPR_COL_DELIMITER = "\t";
		TABLE_ROW_CELL_JOINER = Collectors.joining(TABLE_STRING_REPR_COL_DELIMITER);
	}

	private final ImageVisualizationInfoTableRowCellFactory rowFactory;

	private final Writer writer;

	public ImageVisualizationInfoTableWriter(final Writer writer) {
		this(writer, new ImageVisualizationInfoTableRowCellFactory("-"));
	}

	private ImageVisualizationInfoTableWriter(final Writer writer,
			final ImageVisualizationInfoTableRowCellFactory rowFactory) {
		this.writer = writer;
		this.rowFactory = rowFactory;
	}

	public void write(final Iterator<? extends Entry<?, ImageVisualizationInfo.Datum>> imgVisualizationInfoDataIter)
			throws IOException {
		final List<List<String>> headers = rowFactory.createColumnHeaders();
		final Stream<String> headerStrs = headers.stream().map(List::stream)
				.map(stream -> stream.collect(TABLE_ROW_CELL_JOINER));
		writer.write(headerStrs.collect(TABLE_ROW_JOINER));

		if (imgVisualizationInfoDataIter.hasNext()) {
			final Entry<?, ImageVisualizationInfo.Datum> first = imgVisualizationInfoDataIter.next();
			writer.write(rowFactory.createRowCellValues(first.getKey(), first.getValue()).collect(TABLE_ROW_CELL_JOINER));

			while (imgVisualizationInfoDataIter.hasNext()) {
				final Entry<?, ImageVisualizationInfo.Datum> next = imgVisualizationInfoDataIter.next();
				writer.write(TABLE_STRING_REPR_ROW_DELIMITER);
				writer.write(rowFactory.createRowCellValues(next.getKey(), next.getValue()).collect(TABLE_ROW_JOINER));
			}
		}
	}

}
