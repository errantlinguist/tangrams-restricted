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
import java.net.URL;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringJoiner;
import java.util.TreeSet;
import java.util.function.Function;
import java.util.stream.Collector;
import java.util.stream.Collectors;

import se.kth.speech.awt.Colors;
import se.kth.speech.coin.tangrams.iristk.events.ImageVisualizationInfoDescription;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 17 Mar 2017
 *
 */
public final class ImageVisualizationInfoTableRowWriter {

	private class ColorInfoWriter {

		private final List<String> colNames = Arrays.asList("RED", "GREEN", "BLUE", "ALPHA", "HUE", "SATURATION",
				"BRIGHTNESS", "JAVA_NAME");

		private final Map<Integer, Set<String>> rgbColorNames = Colors.createRGBColorNameMap(String::toUpperCase,
				TreeSet::new);

		public void append(final Color color) throws IOException {
			write(color.getRed());
			writer.write(TABLE_STRING_REPR_COL_DELIMITER);
			write(color.getGreen());
			writer.write(TABLE_STRING_REPR_COL_DELIMITER);
			write(color.getBlue());
			writer.write(TABLE_STRING_REPR_COL_DELIMITER);
			write(color.getAlpha());
			writer.write(TABLE_STRING_REPR_COL_DELIMITER);
			{
				final float[] hsbVals = Color.RGBtoHSB(color.getRed(), color.getGreen(), color.getBlue(), null);
				write(hsbVals[0]);
				writer.write(TABLE_STRING_REPR_COL_DELIMITER);
				write(hsbVals[1]);
				writer.write(TABLE_STRING_REPR_COL_DELIMITER);
				write(hsbVals[2]);
				writer.write(TABLE_STRING_REPR_COL_DELIMITER);
			}
			{
				final Set<String> colorNames = rgbColorNames.get(color.getRGB());
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

	private static final Collector<CharSequence, ?, String> TABLE_ROW_CELL_JOINER;

	private static final String TABLE_STRING_REPR_COL_DELIMITER;

	private static final String TABLE_STRING_REPR_ROW_DELIMITER = System.lineSeparator();

	static {
		TABLE_STRING_REPR_COL_DELIMITER = "\t";
		TABLE_ROW_CELL_JOINER = Collectors.joining(TABLE_STRING_REPR_COL_DELIMITER);
	}

	private final ColorInfoWriter colorInfoWriter;

	private final Function<? super URL, String> resourceNameFactory;

	private final Writer writer;

	public ImageVisualizationInfoTableRowWriter(final Writer writer) {
		this(writer, ImageVisualizationInfoDescription.getResourceNameFactory());
	}

	public ImageVisualizationInfoTableRowWriter(final Writer writer,
			final Function<? super URL, String> resourceNameFactory) {
		this.writer = writer;
		this.resourceNameFactory = resourceNameFactory;
		colorInfoWriter = new ColorInfoWriter();
	}

	public void write(final Object rowId, final ImageVisualizationInfo.Datum datum) throws IOException {
		writer.write(rowId.toString());
		writer.write(TABLE_STRING_REPR_COL_DELIMITER);
		final URL resourceLoc = datum.getResourceLoc();
		final String resourceName = resourceNameFactory.apply(resourceLoc);
		writer.write(resourceName);
		writer.write(TABLE_STRING_REPR_COL_DELIMITER);
		writer.write(datum.getSize().toString());
		writer.write(TABLE_STRING_REPR_COL_DELIMITER);
		colorInfoWriter.append(datum.getColor());
	}

	public void writeHeader() throws IOException {
		final String colorColName = "COLOR";
		final List<String> colNames = Arrays.asList("ID", "IMAGE", "SIZE", colorColName);
		final String header = colNames.stream().collect(TABLE_ROW_CELL_JOINER);
		writer.write(header);

		// Write color sub-header
		writer.write(TABLE_STRING_REPR_ROW_DELIMITER);
		final int colorHeaderIdx = colNames.indexOf(colorColName);
		// Offset the color sub-header row
		for (int i = 0; i < colorHeaderIdx; ++i) {
			writer.write(TABLE_STRING_REPR_COL_DELIMITER);
		}
		writer.write(colorInfoWriter.colNames.stream().collect(TABLE_ROW_CELL_JOINER));
	}

	private void write(final float val) throws IOException {
		writer.write(Float.toString(val));
	}

	private void write(final int val) throws IOException {
		writer.write(Integer.toString(val));
	}

}
