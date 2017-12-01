/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.client.
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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.StringJoiner;
import java.util.stream.Stream;

import se.kth.speech.awt.ColorInfo;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 17 Mar 2017
 *
 */
public final class ImageVisualizationInfoTableRowCellFactory {

	public enum Attribute {
		COLOR, ENTITY_ID, IMAGE, SIZE;

		private static final List<Attribute> CANONICAL_ORDERING;

		static {
			Collections.unmodifiableList(CANONICAL_ORDERING = Arrays.asList(Attribute.ENTITY_ID, Attribute.IMAGE,
					Attribute.SIZE, Attribute.COLOR));
			assert CANONICAL_ORDERING.size() == Attribute.values().length;
		}

		/**
		 * @return the canonicalOrdering
		 */
		public static List<Attribute> getCanonicalOrdering() {
			return CANONICAL_ORDERING;
		}
	}

	private static final List<String> COLOR_ATTR_COL_NAMES;

	private static final List<ColorInfo> COLOR_INFO_TO_WRITE;

	static {
		COLOR_INFO_TO_WRITE = ColorInfo.getCanonicalOrdering();
		COLOR_ATTR_COL_NAMES = Arrays
				.asList(COLOR_INFO_TO_WRITE.stream().map(ColorInfo::toString).toArray(String[]::new));
	}

	public static List<List<String>> createColumnHeaders(final Collection<Attribute> vizInfoAttrsToWrite) {
		final List<String> mainColNames = Arrays
				.asList(vizInfoAttrsToWrite.stream().map(Attribute::toString).toArray(String[]::new));

		final List<List<String>> result;
		if (vizInfoAttrsToWrite.contains(Attribute.COLOR)) {
			final int colorHeaderIdx = mainColNames.indexOf(Attribute.COLOR.toString());
			final List<String> subColNames = new ArrayList<>(mainColNames.size());
			final String padding = "";
			// Offset the color sub-header row
			for (int i = 0; i < colorHeaderIdx; ++i) {
				subColNames.add(padding);
			}
			COLOR_ATTR_COL_NAMES.forEach(subColNames::add);
			while (subColNames.size() < mainColNames.size()) {
				subColNames.add(padding);
			}
			result = Arrays.asList(mainColNames, subColNames);
		} else {
			result = Arrays.asList(mainColNames);
		}

		return result;
	}

	private final String nullValueRepr;

	private final Collection<Attribute> vizInfoAttrsToWrite;

	ImageVisualizationInfoTableRowCellFactory(final String nullValueRepr) {
		this(nullValueRepr, Attribute.getCanonicalOrdering());
	}

	public ImageVisualizationInfoTableRowCellFactory(final String nullValueRepr,
			final Collection<Attribute> vizInfoAttrsToWrite) {
		this.nullValueRepr = nullValueRepr;
		this.vizInfoAttrsToWrite = vizInfoAttrsToWrite;
	}

	public Stream<String> createRowCellValues(final Object rowId, final ImageVisualizationInfo.Datum datum) {
		return vizInfoAttrsToWrite.stream().flatMap(vizInfo -> getAttributeValues(rowId, datum, vizInfo));
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

	private Stream<String> getAttributeValues(final Object rowId, final ImageVisualizationInfo.Datum datum,
			final Attribute attr) {
		final Stream<String> result;
		switch (attr) {
		case COLOR: {
			if (datum == null) {
				result = COLOR_INFO_TO_WRITE.stream().map(colName -> nullValueRepr);
			} else {
				final Color color = datum.getColor();
				final Map<ColorInfo, Object> colorInfo = ColorInfo.createInfoMap(color);
				result = COLOR_INFO_TO_WRITE.stream().map(infoDatum -> {
					final Object value = colorInfo.get(infoDatum);
					return parseColorInfoValue(infoDatum, value);
				});
			}
			break;
		}
		case ENTITY_ID: {
			final String attrVal = Objects.toString(rowId, nullValueRepr);
			result = Stream.of(attrVal);
			break;
		}
		case IMAGE: {
			final String attrVal = datum == null ? nullValueRepr : datum.getResourceName();
			result = Stream.of(attrVal);
			break;
		}
		case SIZE: {
			final String attrVal = datum == null ? nullValueRepr : datum.getSize().toString();
			result = Stream.of(attrVal);
			break;
		}
		default: {
			throw new AssertionError(String.format("No logic for handling attribute %s.", attr));
		}
		}
		return result;
	}

	private String parseColorInfoValue(final ColorInfo infoDatum, final Object value) {
		final String result;
		switch (infoDatum) {
		case JAVA_NAME: {
			@SuppressWarnings("unchecked")
			final Set<String> colorNames = (Set<String>) value;
			result = createColorNameRepr(colorNames);
			break;
		}
		default: {
			result = value.toString();
			break;
		}
		}
		return result;
	}

	List<List<String>> createColumnHeaders() {
		return createColumnHeaders(vizInfoAttrsToWrite);
	}

}
