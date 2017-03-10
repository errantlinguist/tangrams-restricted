/*
 *  This file is part of se.kth.speech.coin.tangrams.client.
 *
 *  se.kth.speech.coin.tangrams.client is free software: you can redistribute it and/or modify
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
import java.io.UncheckedIOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.IntStream;

import com.github.errantlinguist.ClassProperties;
import com.google.common.collect.Maps;

import se.kth.speech.IntArrays;
import se.kth.speech.Integers;
import se.kth.speech.MutablePair;
import se.kth.speech.PropertyFamilyMapParser;
import se.kth.speech.awt.ColorStringRepresentationParser;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 20 Feb 2017
 *
 */
final class ContentTests {

	static final class TestDescription {

		private final List<Entry<URL, Color>> coloredImgResourceLocators;

		private final Integer[] coords;

		private final int emptyCellCount;

		private final int[] modelDims;

		private final long seed;

		private TestDescription(final int[] modelDims, final int emptyCellCount, final long seed,
				final Integer[] coords, final List<Entry<URL, Color>> coloredImgResourceLocators) {
			this.modelDims = modelDims;
			this.emptyCellCount = emptyCellCount;
			this.seed = seed;
			this.coords = coords;
			this.coloredImgResourceLocators = coloredImgResourceLocators;
		}

		public int calculateOccupiedCellCount() {
			final int cellCount = IntArrays.product(modelDims);
			return cellCount - emptyCellCount;
		}

		public Integer[] createCoordOccupantArray() {
			return IntStream.range(0, calculateOccupiedCellCount()).boxed().toArray(Integer[]::new);
		}

		/**
		 * @return the coloredImgResourceLocators
		 */
		public List<Entry<URL, Color>> getColoredImgResourceLocators() {
			return coloredImgResourceLocators;
		}

		/**
		 * @return the coords
		 */
		public Integer[] getCoords() {
			return coords;
		}

		/**
		 * @return the emptyCellCount
		 */
		public int getEmptyCellCount() {
			return emptyCellCount;
		}

		/**
		 * @return the modelDims
		 */
		public int[] getModelDims() {
			return modelDims;
		}

		/**
		 * @return the seed
		 */
		public long getSeed() {
			return seed;
		}
	}

	private static final Pattern IMAGE_PROP_NAME_PATTERN = Pattern.compile("content\\.coloredicon\\.(\\d+)");

	private static final Pattern MULTIVALUE_PROP_DELIM_PATTERN = Pattern.compile("\\s*,\\s*");

	private static final Map<String, TestDescription> NAMED_TEST_DESC_MAP;

	static {
		try {
			NAMED_TEST_DESC_MAP = readNamedTestDescs();
		} catch (final IOException e) {
			throw new UncheckedIOException(e);
		}
	}

	private static TestDescription createTestDescription(final Map<String, String> params)
			throws MalformedURLException {
		int[] modelDims = null;
		Integer emptyCellCount = null;
		Long seed = null;
		Integer[] coordOccupants = null;
		final SortedMap<Integer, Entry<URL, Color>> coloredImgResourceLocatorMap = new TreeMap<>();
		final Matcher imgPropNameMatcher = IMAGE_PROP_NAME_PATTERN.matcher("");
		final Function<String, Color> colorReprParser = new ColorStringRepresentationParser();
		for (final Entry<String, String> param : params.entrySet()) {
			final String paramName = param.getKey();
			switch (paramName) {
			case "model.coords": {
				final String paramValue = param.getValue();
				coordOccupants = MULTIVALUE_PROP_DELIM_PATTERN.splitAsStream(paramValue).map(Integers::valueOfNullable)
						.toArray(Integer[]::new);
				break;
			}
			case "model.dims": {
				final String paramValue = param.getValue();
				modelDims = MULTIVALUE_PROP_DELIM_PATTERN.splitAsStream(paramValue).mapToInt(Integer::parseInt)
						.toArray();
				break;
			}
			case "model.emptyCells": {
				final String paramValue = param.getValue();
				emptyCellCount = Integer.valueOf(paramValue);
				break;
			}
			case "model.seed": {
				final String paramValue = param.getValue();
				seed = Long.valueOf(paramValue);
				break;
			}
			default: {
				imgPropNameMatcher.reset(paramName);
				if (imgPropNameMatcher.matches()) {
					final Integer imgId = Integer.valueOf(imgPropNameMatcher.group(1));
					final String paramValueStr = param.getValue();
					final int expectedValCount = 2;
					final String[] propValues = MULTIVALUE_PROP_DELIM_PATTERN.split(paramValueStr, expectedValCount);
					if (propValues.length == expectedValCount) {
						final URL imgFileUrl = ImageType.ICON.getFileUrl(propValues[0]);
						final Color color = colorReprParser.apply(propValues[1]);
						coloredImgResourceLocatorMap.put(imgId, new MutablePair<>(imgFileUrl, color));
					} else {
						throw new IllegalArgumentException(String
								.format("Could not parse value for map key \"%s\": %s.", paramName, paramValueStr));
					}
				} else {
					throw new IllegalArgumentException(String.format("Unexpected map key \"%s\".", paramName));
				}
				break;
			}
			}
		}
		if (coloredImgResourceLocatorMap.isEmpty()) {
			throw new IllegalArgumentException("No colored image resources parsed.");
		} else {
			final List<Entry<URL, Color>> coloredImgResourceLocatorList = new ArrayList<>(
					coloredImgResourceLocatorMap.values());
			return new TestDescription(modelDims, emptyCellCount, seed, coordOccupants, coloredImgResourceLocatorList);
		}
	}

	private static Map<String, TestDescription> readNamedTestDescs() throws IOException {
		final Properties props = ClassProperties.load(ContentTests.class);
		final Function<String, HashMap<String, String>> propFamilyMapFactory = propName -> new HashMap<>(3, 1.0f);
		final Map<String, Map<String, String>> gameTestParams = new PropertyFamilyMapParser(propFamilyMapFactory,
				"game").apply(props);
		final Map<String, TestDescription> result = Maps.newHashMapWithExpectedSize(gameTestParams.size());
		for (final Entry<String, Map<String, String>> namedGameTestParams : gameTestParams.entrySet()) {
			result.put(namedGameTestParams.getKey(), createTestDescription(namedGameTestParams.getValue()));
		}
		return result;
	}

	/**
	 * @return the namedTestDescMap
	 */
	static Map<String, TestDescription> getNamedTestDescMap() {
		return NAMED_TEST_DESC_MAP;
	}

	private ContentTests() {

	}

}
