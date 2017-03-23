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
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.StringJoiner;
import java.util.TreeSet;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import se.kth.speech.awt.Colors;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 17 Mar 2017
 *
 */
public final class ImageVisualizationInfoTableWriter {

	private static final Map<Integer, Set<String>> COLOR_NAMES = Colors.createColorNameMap(String::toUpperCase,
			TreeSet::new);

	private static final Collector<CharSequence, ?, String> TABLE_ROW_CELL_JOINER;

	private static final String TABLE_STRING_REPR_COL_DELIMITER;

	private static final String TABLE_STRING_REPR_ROW_DELIMITER = System.lineSeparator();

	static {
		TABLE_STRING_REPR_COL_DELIMITER = "\t";
		TABLE_ROW_CELL_JOINER = Collectors.joining(TABLE_STRING_REPR_COL_DELIMITER);
	}

	private final Writer writer;

	public ImageVisualizationInfoTableWriter(final Writer writer) {
		this.writer = writer;
	}

	public void accept(final Iterator<? extends Entry<?, ImageVisualizationInfo>> imgVisualizationInfoDataIter)
			throws IOException {
		final String colorColName = "COLOR";
		final List<String> colNames = Stream.of("ID", "IMAGE", "SIZE", colorColName).collect(Collectors.toList());
		final String header = colNames.stream().collect(TABLE_ROW_CELL_JOINER);
		writer.append(header);
		writer.append(TABLE_STRING_REPR_ROW_DELIMITER);
		final Stream<String> subColNames = Stream.of("RED", "GREEN", "BLUE", "ALPHA", "JAVA_NAME");
		final int colorHeaderIdx = colNames.indexOf(colorColName);
		for (int i = 0; i < colorHeaderIdx; ++i) {
			writer.append(TABLE_STRING_REPR_COL_DELIMITER);
		}
		final String subHeader = subColNames.collect(TABLE_ROW_CELL_JOINER);
		writer.append(subHeader);
		while (imgVisualizationInfoDataIter.hasNext()) {
			writer.append(TABLE_STRING_REPR_ROW_DELIMITER);
			final Entry<?, ImageVisualizationInfo> datumForId = imgVisualizationInfoDataIter.next();
			appendRowTableRepr(datumForId.getKey(), datumForId.getValue());
		}
	}

	private void appendRowTableRepr(final Color color) throws IOException {
		write(color.getRed());
		writer.write(TABLE_STRING_REPR_COL_DELIMITER);
		write(color.getGreen());
		writer.write(TABLE_STRING_REPR_COL_DELIMITER);
		write(color.getBlue());
		writer.write(TABLE_STRING_REPR_COL_DELIMITER);
		write(color.getAlpha());
		writer.write(TABLE_STRING_REPR_COL_DELIMITER);
		{
			final Set<String> colorNames = COLOR_NAMES.get(color.getRGB());
			final String nullVal = "-";
			final String colorNameRepr;
			if (colorNames == null) {
				colorNameRepr = nullVal;
			} else {
				final StringJoiner colorNameJoiner = new StringJoiner(", ");
				colorNameJoiner.setEmptyValue(nullVal);
				colorNames.forEach(colorNameJoiner::add);
				colorNameRepr = colorNameJoiner.toString();
			}
			writer.write(colorNameRepr);
		}
	}

	private void appendRowTableRepr(final Object rowId, final ImageVisualizationInfo datum) throws IOException {
		writer.write(rowId.toString());
		writer.write(TABLE_STRING_REPR_COL_DELIMITER);
		writer.write(datum.getResourceLoc().toString());
		writer.write(TABLE_STRING_REPR_COL_DELIMITER);
		writer.write(datum.getSize().toString());
		writer.write(TABLE_STRING_REPR_COL_DELIMITER);
		appendRowTableRepr(datum.getColor());
	}

	private void write(final int val) throws IOException {
		writer.write(Integer.toString(val));
	}

}
