/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.analysis.
 *
 *  tangrams is free software: you can redistribute it and/or modify
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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since May 26, 2017
 *
 */
public final class WordClasses {

	private static final Pattern CLASS_RELATION_NAME_PATTERN;

	private static final String CLASS_RELATION_PREFIX;

	static {
		CLASS_RELATION_PREFIX = "referent_for_token-";
		CLASS_RELATION_NAME_PATTERN = Pattern.compile(Pattern.quote(CLASS_RELATION_PREFIX) + "(.+)");
	}

	public static String createRelationName(final String className) {
		return CLASS_RELATION_PREFIX + className;
	}

	public static String parseRelationClassName(final String relName) {
		final Matcher classRelNameMatcher = CLASS_RELATION_NAME_PATTERN.matcher(relName);
		if (classRelNameMatcher.matches()) {
			return classRelNameMatcher.group(1);
		} else {
			throw new IllegalArgumentException(
					String.format("Could not parse a class name from relation name \"%s\".", relName));
		}
	}

	private WordClasses() {

	}
}
