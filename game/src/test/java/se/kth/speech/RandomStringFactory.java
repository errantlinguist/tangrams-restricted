/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.client.
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

import java.util.Random;
import java.util.function.Supplier;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 10 Mar 2017
 * @see <a href="http://stackoverflow.com/a/41156/1391325">StackOverflow</a>
 *
 */
public final class RandomStringFactory implements Supplier<String> {

	private static final char[] DEFAULT_SYMBOLS = createDefaultSymbolArray();

	private static final int MIN_LENGTH = 1;

	private static char[] createDefaultSymbolArray() {
		final StringBuilder tmp = new StringBuilder();
		for (char ch = '0'; ch <= '9'; ++ch) {
			tmp.append(ch);
		}
		for (char ch = 'a'; ch <= 'z'; ++ch) {
			tmp.append(ch);
		}
		return tmp.toString().toCharArray();
	}

	private final char[] buf;

	private final Random random;

	public RandomStringFactory(final int length) {
		this(new Random(), length);
	}

	public RandomStringFactory(final Random random, final int length) {
		this.random = random;
		if (length < MIN_LENGTH) {
			throw new IllegalArgumentException(String.format("length < %s: %s", MIN_LENGTH, length));
		}
		buf = new char[length];
	}

	@Override
	public String get() {
		for (int idx = 0; idx < buf.length; ++idx) {
			buf[idx] = DEFAULT_SYMBOLS[random.nextInt(DEFAULT_SYMBOLS.length)];
		}
		return new String(buf);
	}
}
