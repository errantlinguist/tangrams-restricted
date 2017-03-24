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
package se.kth.speech;

import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.function.Function;

import com.google.common.collect.Maps;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 24 Mar 2017
 *
 */
public final class NamedTestDescriptionPropertyMapFactory<D> implements Function<Properties, Map<String, D>> {

	private final Function<? super Map<String, String>, D> descFactory;

	private final PropertyFamilyMapParser propFamilyMapParser;

	public NamedTestDescriptionPropertyMapFactory(
			final Function<? super Map<String, String>, D> descFactory,
			final String testDescPropNamePrefix, final int testParamCount) {
		this.descFactory = descFactory;
		propFamilyMapParser = new PropertyFamilyMapParser(propName -> Maps.newHashMapWithExpectedSize(testParamCount),
				testDescPropNamePrefix);
	}

	@Override
	public Map<String, D> apply(final Properties props) {
		final Map<String, Map<String, String>> gameTestParams = propFamilyMapParser.apply(props);
		final Map<String, D> result = Maps.newHashMapWithExpectedSize(gameTestParams.size());
		for (final Entry<String, Map<String, String>> namedGameTestParams : gameTestParams.entrySet()) {
			result.put(namedGameTestParams.getKey(), descFactory.apply(namedGameTestParams.getValue()));
		}
		return result;
	}

}
