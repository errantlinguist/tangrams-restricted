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
import java.util.Map;
import java.util.Set;
import java.util.StringJoiner;
import java.util.TreeSet;
import java.util.stream.Collector;
import java.util.stream.Collectors;

import se.kth.speech.awt.Colors;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 17 Mar 2017
 *
 */
public final class ImageVisualizationInfoTableRowWriter {

	private static class ColorInfoWriter {

		private static final List<String> COL_NAMES = Arrays.asList("RED", "GREEN", "BLUE", "ALPHA", "HUE",
				"SATURATION", "BRIGHTNESS", "JAVA_NAME");

		private static final Map<Integer, Set<String>> RGB_COLOR_NAMES = Colors
				.createRGBColorNameMap(String::toUpperCase, TreeSet::new);

		private final Writer writer;

		private ColorInfoWriter(final Writer writer) {
			this.writer = writer;
		}

		public void append(final Color color) throws IOException {
			write(writer, color.getRed());
			writer.write(TABLE_STRING_REPR_COL_DELIMITER);
			write(writer, color.getGreen());
			writer.write(TABLE_STRING_REPR_COL_DELIMITER);
			write(writer, color.getBlue());
			writer.write(TABLE_STRING_REPR_COL_DELIMITER);
			write(writer, color.getAlpha());
			writer.write(TABLE_STRING_REPR_COL_DELIMITER);
			{
				final float[] hsbVals = Color.RGBtoHSB(color.getRed(), color.getGreen(), color.getBlue(), null);
				write(writer, hsbVals[0]);
				writer.write(TABLE_STRING_REPR_COL_DELIMITER);
				write(writer, hsbVals[1]);
				writer.write(TABLE_STRING_REPR_COL_DELIMITER);
				write(writer, hsbVals[2]);
				writer.write(TABLE_STRING_REPR_COL_DELIMITER);
			}
			{
				final Set<String> colorNames = RGB_COLOR_NAMES.get(color.getRGB());
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

	}

	private static final String COLOR_COL_NAME = "COLOR";

	private static final Collector<CharSequence, ?, String> TABLE_ROW_CELL_JOINER;

	private static final Collector<CharSequence, ?, String> TABLE_ROW_JOINER;

	private static final String TABLE_STRING_REPR_COL_DELIMITER;

	private static final String TABLE_STRING_REPR_ROW_DELIMITER;

	static {
		TABLE_STRING_REPR_ROW_DELIMITER = System.lineSeparator();
		TABLE_ROW_JOINER = Collectors.joining(TABLE_STRING_REPR_ROW_DELIMITER);
	}

	static {
		TABLE_STRING_REPR_COL_DELIMITER = "\t";
		TABLE_ROW_CELL_JOINER = Collectors.joining(TABLE_STRING_REPR_COL_DELIMITER);
	}

	public static List<List<String>> createColumnHeaders() {
		final List<String> mainColNames = Collections
				.unmodifiableList(Arrays.asList("ID", "IMAGE", "SIZE", COLOR_COL_NAME));
		final int colorHeaderIdx = mainColNames.indexOf(COLOR_COL_NAME);
		final List<String> subColNames = new ArrayList<>(mainColNames.size());
		// Offset the color sub-header row
		for (int i = 0; i < colorHeaderIdx; ++i) {
			subColNames.add("");
		}
		ColorInfoWriter.COL_NAMES.forEach(subColNames::add);
		return Arrays.asList(mainColNames, subColNames);
	}

	private static void write(final Writer writer, final float val) throws IOException {
		writer.write(Float.toString(val));
	}

	private static void write(final Writer writer, final int val) throws IOException {
		writer.write(Integer.toString(val));
	}

	private final ColorInfoWriter colorInfoWriter;

	private final Writer writer;

	public ImageVisualizationInfoTableRowWriter(final Writer writer) {
		this.writer = writer;
		colorInfoWriter = new ColorInfoWriter(writer);
	}

	public void write(final Object rowId, final ImageVisualizationInfo.Datum datum) throws IOException {
		writer.write(rowId.toString());
		writer.write(TABLE_STRING_REPR_COL_DELIMITER);
		final String resourceName = datum.getResourceName();
		writer.write(resourceName);
		writer.write(TABLE_STRING_REPR_COL_DELIMITER);
		writer.write(datum.getSize().toString());
		writer.write(TABLE_STRING_REPR_COL_DELIMITER);
		colorInfoWriter.append(datum.getColor());
	}

	void writeHeader() throws IOException {
		final List<List<String>> headers = createColumnHeaders();
		final String headerStr = headers.stream().map(header -> header.stream().collect(TABLE_ROW_CELL_JOINER))
				.collect(TABLE_ROW_JOINER);
		writer.write(headerStr);
	}

}
