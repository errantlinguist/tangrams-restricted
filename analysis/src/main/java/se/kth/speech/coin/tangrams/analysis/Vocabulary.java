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
package se.kth.speech.coin.tangrams.analysis;

import java.util.Locale;
import java.util.Set;
import java.util.function.Function;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Apr 13, 2017
 *
 */
final class Vocabulary {

	private final Locale locale;

	private final Function<? super String, String> normalizer;

	private final Set<String> words;

	/**
	 * @param words
	 * @param locale
	 * @param normalizer
	 *
	 */
	Vocabulary(final Set<String> words, final Locale locale, final Function<? super String, String> normalizer) {
		this.words = words;
		this.locale = locale;
		this.normalizer = normalizer;
	}

	/**
	 * @return the locale
	 */
	public Locale getLocale() {
		return locale;
	}

	/**
	 * @return the normalizer
	 */
	public Function<? super String, String> getNormalizer() {
		return normalizer;
	}

	/**
	 * @return
	 */
	public int size() {
		return words.size();
	}

}
