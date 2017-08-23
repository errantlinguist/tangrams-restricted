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
package se.kth.speech.coin.tangrams.content;

import java.awt.Color;
import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collector;
import java.util.stream.Collectors;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 17 Mar 2017
 *
 */
public final class ImageVisualizationInfoTableRowWriter {

	private static final String COLOR_COL_NAME = "COLOR";

	public static List<List<String>> createColumnHeaders() {
		final List<String> mainColNames = Collections
				.unmodifiableList(Arrays.asList("ID", "IMAGE", "SIZE", COLOR_COL_NAME));
		final int colorHeaderIdx = mainColNames.indexOf(COLOR_COL_NAME);
		final List<String> subColNames = new ArrayList<>(mainColNames.size());
		// Offset the color sub-header row
		for (int i = 0; i < colorHeaderIdx; ++i) {
			subColNames.add("");
		}
		ColorInfoWriter.getColumnNames().forEach(subColNames::add);
		return Arrays.asList(mainColNames, subColNames);
	}

	private final String colDelimiter;

	private final ColorInfoWriter colorInfoWriter;

	private final String nullValueRepr;

	private final Collector<CharSequence, ?, String> rowCellJoiner;

	private final Collector<CharSequence, ?, String> rowJoiner;

	private final Writer writer;

	public ImageVisualizationInfoTableRowWriter(final Writer writer, final String rowDelimiter,
			final String colDelimiter, final String nullValueRepr) {
		this.writer = writer;
		rowJoiner = Collectors.joining(rowDelimiter);
		this.colDelimiter = colDelimiter;
		rowCellJoiner = Collectors.joining(colDelimiter);
		colorInfoWriter = new ColorInfoWriter(writer, colDelimiter, nullValueRepr);
		this.nullValueRepr = nullValueRepr;
	}

	public void write(final Object rowId, final ImageVisualizationInfo.Datum datum) throws IOException {
		writer.write(Objects.toString(rowId, nullValueRepr));
		writer.write(colDelimiter);

		final String resourceName;
		final String size;
		final Color color;
		if (datum == null) {
			resourceName = nullValueRepr;
			size = nullValueRepr;
			color = null;
		} else {
			resourceName = datum.getResourceName();
			size = datum.getSize().toString();
			color = datum.getColor();
		}
		writer.write(resourceName);
		writer.write(colDelimiter);
		writer.write(size);
		writer.write(colDelimiter);
		colorInfoWriter.accept(color);
	}

	void writeHeader() throws IOException {
		final List<List<String>> headers = createColumnHeaders();
		final String headerStr = headers.stream().map(header -> header.stream().collect(rowCellJoiner))
				.collect(rowJoiner);
		writer.write(headerStr);
	}

}
