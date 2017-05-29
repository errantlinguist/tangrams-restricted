/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.analysis.
 *
 *  se.kth.speech.coin.tangrams-restricted.analysis is free software: you can redistribute it and/or modify
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
package se.kth.speech.nlp;

import java.util.Arrays;
import java.util.List;
import java.util.function.Function;
import java.util.regex.Pattern;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 29 May 2017
 *
 */
public final class PatternTokenizer implements Function<String, List<String>> {

	private static final Pattern DEFAULT_PATTERN = Pattern.compile("\\s+");

	private final Pattern pattern;

	public PatternTokenizer() {
		this(DEFAULT_PATTERN);
	}

	public PatternTokenizer(final Pattern pattern) {
		this.pattern = pattern;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.util.function.Function#apply(java.lang.Object)
	 */
	@Override
	public List<String> apply(final String input) {
		return Arrays.asList(pattern.split(input));
	}

}
