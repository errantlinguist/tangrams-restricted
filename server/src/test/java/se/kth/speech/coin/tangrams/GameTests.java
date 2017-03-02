/*
 *  This file is part of se.kth.speech.coin.tangrams.server.
 *
 *  se.kth.speech.coin.tangrams.server is free software: you can redistribute it and/or modify
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
package se.kth.speech.coin.tangrams;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.function.Function;
import java.util.regex.Pattern;
import java.util.stream.IntStream;

import com.github.errantlinguist.ClassProperties;

import se.kth.speech.IntArrays;
import se.kth.speech.Integers;
import se.kth.speech.PropertyFamilyMapParser;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 20 Feb 2017
 *
 */
final class GameTests {

	static final class TestDescription {

		private final Integer[] coords;

		private final int emptyCellCount;

		private final int[] modelDims;

		private final long seed;

		private TestDescription(final int[] modelDims, final int emptyCellCount, final long seed,
				final Integer[] coords) {
			this.modelDims = modelDims;
			this.emptyCellCount = emptyCellCount;
			this.seed = seed;
			this.coords = coords;
		}

		public int calculateOccupiedCellCount() {
			final int cellCount = IntArrays.product(modelDims);
			return cellCount - emptyCellCount;
		}

		public Integer[] createCoordOccupantArray() {
			return IntStream.range(0, calculateOccupiedCellCount()).boxed().toArray(Integer[]::new);
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

	private static final Pattern MULTIVALUE_PROP_DELIM_PATTERN = Pattern.compile("\\s*,\\s*");

	private static final Map<String, TestDescription> NAMED_TEST_DESC_MAP;

	static {
		try {
			final Properties props = ClassProperties.load(GameTests.class);
			final Function<String, HashMap<String, String>> propFamilyMapFactory = propName -> new HashMap<>(3, 1.0f);
			final Map<String, Map<String, String>> testParams = new PropertyFamilyMapParser(propFamilyMapFactory,
					"game").apply(props);
			NAMED_TEST_DESC_MAP = new HashMap<>(testParams.size() + 1, 1.0f);
			testParams.forEach(
					(testName, params) -> NAMED_TEST_DESC_MAP.put(testName, createTestDescription(params)));
		} catch (final IOException e) {
			throw new UncheckedIOException(e);
		}
	}

	private static TestDescription createTestDescription(final Map<? super String, String> params) {
		final int[] modelDims = MULTIVALUE_PROP_DELIM_PATTERN.splitAsStream(params.get("model.dims"))
				.mapToInt(Integer::parseInt).toArray();
		final int emptyCellCount = Integer.parseInt(params.get("model.emptyCells"));
		final long seed = Long.parseLong(params.get("model.seed"));
		final Integer[] coordOccupants = MULTIVALUE_PROP_DELIM_PATTERN.splitAsStream(params.get("model.coords"))
				.map(Integers::valueOfNullable).toArray(Integer[]::new);
		return new TestDescription(modelDims, emptyCellCount, seed, coordOccupants);
	}

	/**
	 * @return the namedTestDescMap
	 */
	static Map<String, TestDescription> getNamedTestDescMap() {
		return NAMED_TEST_DESC_MAP;
	}

	private GameTests() {

	}

}
