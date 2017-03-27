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
package se.kth.speech.coin.tangrams;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.regex.Pattern;

import com.github.errantlinguist.ClassProperties;

import se.kth.speech.Integers;
import se.kth.speech.NamedTestDescriptionPropertyMapFactory;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 24 Mar 2017
 *
 */
public final class MatrixTests {

	public static final class Description {

		private final int colCount;

		private final Integer[] values;

		private Description(final Integer[] values, final int colCount) {
			this.values = values;
			this.colCount = colCount;
		}

		/**
		 * @return the colCount
		 */
		public int getColCount() {
			return colCount;
		}

		/**
		 * @return the values
		 */
		public Integer[] getValues() {
			return values;
		}

	}

	private static final Pattern MULTIVALUE_PROP_DELIM_PATTERN = Pattern.compile("\\s*,\\s*");

	private static final Map<String, Description> NAMED_TEST_DESC_MAP;

	static {
		try {
			NAMED_TEST_DESC_MAP = readNamedTestDescs();
		} catch (final IOException e) {
			throw new UncheckedIOException(e);
		}
	}

	/**
	 * @return the namedTestDescMap
	 */
	public static Map<String, Description> getNamedTestDescs() {
		return NAMED_TEST_DESC_MAP;
	}

	private static Description createDescription(final Map<String, String> params) {
		Integer[] values = null;
		Integer colCount = null;
		for (final Entry<String, String> param : params.entrySet()) {
			final String paramName = param.getKey();
			switch (paramName) {
			case "colCount": {
				final String paramValue = param.getValue();
				colCount = Integer.valueOf(paramValue);
				break;
			}
			case "values": {
				final String paramValue = param.getValue();
				values = MULTIVALUE_PROP_DELIM_PATTERN.splitAsStream(paramValue).map(Integers::valueOfNullable)
						.toArray(Integer[]::new);
				break;
			}
			default: {
				throw new IllegalArgumentException(String.format("Unexpected map key \"%s\".", paramName));
			}
			}
		}
		return new Description(values, colCount);
	}

	private static Map<String, Description> readNamedTestDescs() throws IOException {
		final Properties props = ClassProperties.load(MatrixTests.class);
		final int testParamCount = 2;
		final NamedTestDescriptionPropertyMapFactory<Description> namedDescMapFactory = new NamedTestDescriptionPropertyMapFactory<>(
				MatrixTests::createDescription, "matrix", testParamCount);
		return namedDescMapFactory.apply(props);
	}

	private MatrixTests() {

	}

}
