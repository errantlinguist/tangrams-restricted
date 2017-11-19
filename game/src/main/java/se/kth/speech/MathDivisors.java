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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 8 Mar 2017
 *
 */
public final class MathDivisors {

	public static List<Integer> createCommonDivisorList(final int first, final int... next) {
		final List<Integer> result = createDivisorList(first);
		for (final int value : next) {
			removeNonDivisors(result.iterator(), value);
		}
		return result;
	}

	public static List<Integer> createCommonDivisorList(final Iterator<Integer> values) {
		final Integer first = values.next();
		final List<Integer> result = createDivisorList(first);
		while (values.hasNext()) {
			final Integer next = values.next();
			removeNonDivisors(result.iterator(), next);
		}
		return result;
	}

	/**
	 * @see <a href=
	 *      "http://codereview.stackexchange.com/a/58711">StackExchange</a>
	 * @param value
	 *            The value to compute all divisors of.
	 * @return A new {@link List} of {@link Integer} values which are divisors
	 *         of the given value.
	 */
	public static List<Integer> createDivisorList(final int value) {
		final int maxPossible = value / 2;
		final List<Integer> result = new ArrayList<>(maxPossible);
		for (int i = 1; i <= maxPossible; i++) {
			if (value % i == 0) {
				result.add(i);
			}
		}
		return result;
	}

	/**
	 * @see <a href=
	 *      "http://stackoverflow.com/a/4009247/1391325">StackOverflow</a>
	 * @param first
	 *            The first value.
	 * @param next
	 *            The other value.
	 * @return The greatest common divisor.
	 */
	public static int gcd(final int first, final int next) {
		final int result;
		if (next == 0) {
			result = first;
		} else {
			result = gcd(next, first % next);
		}
		return result;
	}

	/**
	 * @see <a href=
	 *      "http://stackoverflow.com/a/15351278/1391325">StackOverflow</a>
	 * @param first
	 *            The first value.
	 * @param next
	 *            The next values.
	 * @return The greatest common divisor.
	 */
	public static int gcd(final int first, final int... next) {
		int result = first;
		for (final int value : next) {
			result = gcd(result, value);
		}
		return result;
	}

	/**
	 * @see <a href=
	 *      "http://stackoverflow.com/a/15351278/1391325">StackOverflow</a>
	 * @param values
	 *            The values to calculate the greatest common divisor for.
	 * @return The greatest common divisor.
	 */
	public static int gcd(final int[] values) {
		int result = values[0];
		for (int i = 1; i < values.length; ++i) {
			final int value = values[i];
			result = gcd(result, value);
		}
		return result;
	}

	/**
	 * @see <a href=
	 *      "http://stackoverflow.com/a/15351278/1391325">StackOverflow</a>
	 * @param values
	 *            The values to get the GCD of.
	 * @return The greatest common divisor.
	 */
	public static int gcd(final Iterator<Integer> values) {
		int result = values.next();
		while (values.hasNext()) {
			result = gcd(result, values.next());
		}
		return result;
	}

	/**
	 * @see <a href=
	 *      "http://codereview.stackexchange.com/a/58711">StackExchange</a>
	 * @param divisors
	 *            An {@link Iterator} of values to check for their ability to
	 *            divide the provided values.
	 * @param first
	 *            The first value for which all values in <code>divisors</code>
	 *            must be valid divisors.
	 * @param next
	 *            The next values for which all values in <code>divisors</code>
	 *            must be valid divisors.
	 */
	public static void removeNonDivisors(final Iterator<Integer> divisors, final int first, final int... next) {
		while (divisors.hasNext()) {
			final Integer divisor = divisors.next();
			if (first % divisor == 0) {
				for (final int value : next) {
					if (value % divisor != 0) {
						divisors.remove();
					}
				}
			} else {
				divisors.remove();
			}
		}
	}

	/**
	 * @see <a href=
	 *      "http://codereview.stackexchange.com/a/58711">StackExchange</a>
	 * @param divisors
	 *            An {@link Iterator} of values to check for their ability to
	 *            divide the provided values.
	 * @param values
	 *            An {@link Iterable} object containing values for which all
	 *            values in <code>divisors</code> must be valid divisors.
	 */
	public static void removeNonDivisors(final Iterator<Integer> divisors, final Iterable<Integer> values) {
		while (divisors.hasNext()) {
			final Integer divisor = divisors.next();
			final Iterator<Integer> valueIter = values.iterator();
			final int first = valueIter.next();
			if (first % divisor == 0) {
				while (valueIter.hasNext()) {
					final int next = valueIter.next();
					if (next % divisor != 0) {
						divisors.remove();
					}
				}
			} else {
				divisors.remove();
			}
		}
	}

	private MathDivisors() {
	}

}
