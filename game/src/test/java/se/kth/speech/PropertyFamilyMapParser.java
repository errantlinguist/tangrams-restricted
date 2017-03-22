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
package se.kth.speech;

import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 20 Feb 2017
 *
 */
public final class PropertyFamilyMapParser implements Function<Properties, Map<String, Map<String, String>>> {

	private static final Logger LOGGER = LoggerFactory.getLogger(PropertyFamilyMapParser.class);

	private static final String PROP_FAMILY_NAME_CAPTURING_REGEX = "\\.([^\\.]+)\\.(.*)";

	private final Function<? super String, ? extends Map<String, String>> propFamilyMapFactory;

	private final Pattern propNamePrefixPattern;

	public PropertyFamilyMapParser(final Function<? super String, ? extends Map<String, String>> propFamilyMapFactory,
			final String propNamePrefix) {
		this(propFamilyMapFactory, Pattern.compile(propNamePrefix + PROP_FAMILY_NAME_CAPTURING_REGEX));
	}

	private PropertyFamilyMapParser(final Function<? super String, ? extends Map<String, String>> propFamilyMapFactory,
			final Pattern propNamePrefixPattern) {
		this.propFamilyMapFactory = propFamilyMapFactory;
		this.propNamePrefixPattern = propNamePrefixPattern;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.util.function.Function#apply(java.lang.Object)
	 */
	@Override
	public Map<String, Map<String, String>> apply(final Properties props) {
		final Map<String, Map<String, String>> result = new HashMap<>();
		final Set<String> propNames = props.stringPropertyNames();
		final Matcher gameTestPropMatcher = propNamePrefixPattern.matcher("");
		for (final String propName : propNames) {
			gameTestPropMatcher.reset(propName);
			if (gameTestPropMatcher.matches()) {
				final String testName = gameTestPropMatcher.group(1);
				final Map<String, String> testParams = result.computeIfAbsent(testName, propFamilyMapFactory);
				final String propNameSuffix = gameTestPropMatcher.group(2);
				LOGGER.debug("Reading sub-property \"{}\" for family \"{}\".", propNameSuffix, testName);
				final String propValue = props.getProperty(propName);
				final String oldParam = testParams.put(propNameSuffix, propValue);
				if (oldParam != null) {
					throw new IllegalArgumentException(String.format(
							"More than one sub-property \"%s\" found for family \"%s\".", propNameSuffix, testName));
				}
			}
		}
		return result;
	}

}
