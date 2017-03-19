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
package se.kth.speech.awt;

import java.awt.Color;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * <strong>NOTE:</strong> Not thread-safe!
 *
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 21 Feb 2017
 *
 */
public final class ColorStringRepresentationParser implements Function<String, Color> {

	private static final Pattern COLOR_REPR_PATTERN = Pattern
			.compile("java.awt.Color\\[r=(?<r>\\d+),g=(?<g>\\d+),b=(?<b>\\d+)\\]");

	private final Matcher colorReprMatcher;

	public ColorStringRepresentationParser() {
		this(COLOR_REPR_PATTERN.matcher(""));
	}

	private ColorStringRepresentationParser(final Matcher colorReprMatcher) {
		this.colorReprMatcher = colorReprMatcher;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Function#apply(java.lang.Object)
	 */
	@Override
	public Color apply(final String colorReprStr) {
		colorReprMatcher.reset(colorReprStr);
		if (colorReprMatcher.matches()) {
			final int r = Integer.parseInt(colorReprMatcher.group("r"));
			final int g = Integer.parseInt(colorReprMatcher.group("g"));
			final int b = Integer.parseInt(colorReprMatcher.group("b"));
			return new Color(r, g, b);
		} else {
			throw new IllegalArgumentException(
					String.format("Could not parse color representation string \"%s\".", colorReprStr));
		}
	}
}
