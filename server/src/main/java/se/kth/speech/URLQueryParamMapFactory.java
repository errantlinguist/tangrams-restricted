/*
 *  This file is part of server.
 *
 *  server is free software: you can redistribute it and/or modify
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
import java.util.function.Function;

import com.google.common.collect.Maps;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 28 Mar 2017
 * @see <a href="http://stackoverflow.com/a/13592567/1391325">StackOverflow</a>
 *
 */
public final class URLQueryParamMapFactory implements Function<String, Map<String, String>> {

	private static final String PARAM_DELIMITER = "&";

	private final Function<? super String, String> encodedQueryStrTransformer;

	public URLQueryParamMapFactory(final Function<? super String, String> encodedQueryStrTransformer) {
		this.encodedQueryStrTransformer = encodedQueryStrTransformer;
	}

	@Override
	public Map<String, String> apply(final String query) {
		final String[] pairs = query.split(PARAM_DELIMITER);
		final Map<String, String> result = Maps.newHashMapWithExpectedSize(pairs.length);

		for (final String pair : pairs) {
			final int idx = pair.indexOf('=');
			if (idx < 0) {
				result.put(pair, "");
			} else {
				final String paramName = encodedQueryStrTransformer.apply(pair.substring(0, idx));
				final String paramValue = idx < pair.length()
						? encodedQueryStrTransformer.apply(pair.substring(idx + 1)) : "";
				result.put(paramName, paramValue);
			}
		}
		return result;
	}

}
