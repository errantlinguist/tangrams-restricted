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
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Random;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.hamcrest.collection.IsIterableContainingInOrder;
import org.junit.Assert;
import org.junit.experimental.theories.DataPoints;
import org.junit.experimental.theories.Theories;
import org.junit.experimental.theories.Theory;
import org.junit.runner.RunWith;

import com.github.errantlinguist.ClassProperties;

import se.kth.speech.MutablePair;
import se.kth.speech.PropertyFamilyMapParser;
import se.kth.speech.awt.ColorStringRepresentationParser;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 21 Feb 2017
 *
 */
@RunWith(Theories.class)
public final class RandomColoredImageLocatorFactoryTest {

	private static final Pattern IMAGE_PROP_NAME_PATTERN = Pattern.compile("coloredimg\\.(\\d+)");

	private static final Pattern MULTIVALUE_PROP_DELIM_PATTERN = Pattern.compile("\\s*,\\s*");

	private static final Map<String, Entry<Long, List<Entry<URL, Color>>>> NAMED_TEST_DESC_MAP;

	static {
		try {
			NAMED_TEST_DESC_MAP = readNamedTestDescs();
		} catch (final IOException e) {
			throw new UncheckedIOException(e);
		}
	}

	private static final RandomColoredImageLocatorFactory TEST_FACTORY = new RandomColoredImageLocatorFactory(
			IconImages.getIconImageResources());

	@DataPoints("testDescs")
	public static Collection<Entry<Long, List<Entry<URL, Color>>>> getTestDescs() {
		return NAMED_TEST_DESC_MAP.values();
	}

	private static MutablePair<Long, List<Entry<URL, Color>>> createTestDescription(final Map<String, String> params)
			throws MalformedURLException {
		Long seed = null;
		final SortedMap<Integer, Entry<URL, Color>> coloredImgResourceLocatorMap = new TreeMap<>();
		final Matcher imgPropNameMatcher = IMAGE_PROP_NAME_PATTERN.matcher("");
		final Function<String, Color> colorReprParser = new ColorStringRepresentationParser();
		for (final Entry<String, String> param : params.entrySet()) {
			final String paramName = param.getKey();
			switch (paramName) {
			case "seed": {
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
			return new MutablePair<>(seed, coloredImgResourceLocatorList);
		}
	}

	private static Map<String, Entry<Long, List<Entry<URL, Color>>>> readNamedTestDescs() throws IOException {
		final Properties props = ClassProperties.load(RandomColoredImageLocatorFactoryTest.class);
		final Function<String, HashMap<String, String>> propFamilyMapFactory = propName -> new HashMap<>(3, 1.0f);
		final Map<String, Map<String, String>> testParams = new PropertyFamilyMapParser(propFamilyMapFactory, "test")
				.apply(props);
		final Map<String, Entry<Long, List<Entry<URL, Color>>>> result = new HashMap<>(testParams.size() + 1, 1.0f);
		for (final Entry<String, Map<String, String>> namedTestParams : testParams.entrySet()) {
			result.put(namedTestParams.getKey(), createTestDescription(namedTestParams.getValue()));
		}
		return result;
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.content.RandomColoredImageLocatorFactory#apply(java.util.Random, java.lang.Integer)}.
	 */
	@Theory
	public final void testApply(final Entry<Long, List<Entry<URL, Color>>> testDesc) {
		final Random rnd = new Random(testDesc.getKey());
		final List<Entry<URL, Color>> expectedColoredImgLocators = testDesc.getValue();
		final List<Entry<URL, Color>> actualColoredImgLocators = TEST_FACTORY.apply(rnd).collect(Collectors.toList());
		Assert.assertThat(actualColoredImgLocators,
				IsIterableContainingInOrder.contains(expectedColoredImgLocators.toArray()));
	}

}
