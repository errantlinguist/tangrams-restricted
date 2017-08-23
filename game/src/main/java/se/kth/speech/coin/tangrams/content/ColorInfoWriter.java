/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.game.
 *
 *  se.kth.speech.coin.tangrams-restricted.game is free software: you can redistribute it and/or modify
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
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringJoiner;
import java.util.TreeSet;

import se.kth.speech.awt.Colors;

final class ColorInfoWriter {

	private static final List<String> COL_NAMES = Arrays.asList("RED", "GREEN", "BLUE", "ALPHA", "HUE", "SATURATION",
			"BRIGHTNESS", "JAVA_NAME");

	private static final Map<Integer, Set<String>> RGB_COLOR_NAMES = Colors.createRGBColorNameMap(String::toUpperCase,
			TreeSet::new);

	private static void write(final Writer writer, final float val) throws IOException {
		writer.write(Float.toString(val));
	}

	private static void write(final Writer writer, final int val) throws IOException {
		writer.write(Integer.toString(val));
	}

	/**
	 * @return the colNames
	 */
	static List<String> getColumnNames() {
		return COL_NAMES;
	}

	private final String colDelimiter;

	private final String nullValueRepr;

	private final Writer writer;

	ColorInfoWriter(final Writer writer, final String colDelimiter, final String nullValueRepr) {
		this.writer = writer;
		this.colDelimiter = colDelimiter;
		this.nullValueRepr = nullValueRepr;
	}

	private void acceptNonNull(final Color color) throws IOException {
		write(writer, color.getRed());
		writer.write(colDelimiter);
		write(writer, color.getGreen());
		writer.write(colDelimiter);
		write(writer, color.getBlue());
		writer.write(colDelimiter);
		write(writer, color.getAlpha());
		writer.write(colDelimiter);
		{
			final float[] hsbVals = Color.RGBtoHSB(color.getRed(), color.getGreen(), color.getBlue(), null);
			write(writer, hsbVals[0]);
			writer.write(colDelimiter);
			write(writer, hsbVals[1]);
			writer.write(colDelimiter);
			write(writer, hsbVals[2]);
			writer.write(colDelimiter);
		}
		final String colorNameRepr = createColorNameRepr(color);
		writer.write(colorNameRepr);
	}

	private String createColorNameRepr(final Color color) {
		final Set<String> colorNames = RGB_COLOR_NAMES.get(color.getRGB());
		final String result;
		if (colorNames == null) {
			result = nullValueRepr;
		} else {
			final StringJoiner colorNameJoiner = new StringJoiner(", ");
			colorNameJoiner.setEmptyValue(nullValueRepr);
			colorNames.forEach(colorNameJoiner::add);
			result = colorNameJoiner.toString();
		}
		return result;
	}

	void accept(final Color color) throws IOException {
		if (color == null) {
			final Iterator<String> colNameIter = COL_NAMES.iterator();
			if (colNameIter.hasNext()) {
				writer.write(nullValueRepr);
				colNameIter.next();
				while (colNameIter.hasNext()) {
					writer.write(colDelimiter);
					writer.write(nullValueRepr);
					colNameIter.next();
				}
			}
		} else {
			acceptNonNull(color);
		}
	}

}