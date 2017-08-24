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
import java.util.Map.Entry;
import java.util.Set;
import java.util.StringJoiner;

import se.kth.speech.awt.ColorInfo;

final class ColorInfoWriter {

	private static final List<String> COL_NAMES = Arrays
			.asList(Arrays.stream(ColorInfo.values()).map(ColorInfo::toString).toArray(String[]::new));

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
		final Map<ColorInfo, Object> colorInfo = ColorInfo.createInfoMap(color);
		final Iterator<Entry<ColorInfo, Object>> colorInfoDatumIter = colorInfo.entrySet().iterator();
		if (colorInfoDatumIter.hasNext()) {
			final Entry<ColorInfo, Object> first = colorInfoDatumIter.next();
			write(first);
			while (colorInfoDatumIter.hasNext()) {
				final Entry<ColorInfo, Object> next = colorInfoDatumIter.next();
				writer.write(colDelimiter);
				write(next);
			}
		}
	}

	private String createColorNameRepr(final Set<String> colorNames) {
		final String result;
		if (colorNames.isEmpty()) {
			result = nullValueRepr;
		} else {
			final StringJoiner colorNameJoiner = new StringJoiner(", ");
			colorNameJoiner.setEmptyValue(nullValueRepr);
			colorNames.forEach(colorNameJoiner::add);
			result = colorNameJoiner.toString();
		}
		return result;
	}

	private void write(final Entry<ColorInfo, Object> colorInfoDatum) throws IOException {
		switch (colorInfoDatum.getKey()) {
		case JAVA_NAME: {
			@SuppressWarnings("unchecked")
			final Set<String> colorNames = (Set<String>) colorInfoDatum.getValue();
			final String colorNameRepr = createColorNameRepr(colorNames);
			writer.write(colorNameRepr);
			break;
		}
		default: {
			writer.write(colorInfoDatum.getValue().toString());
			break;
		}
		}
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