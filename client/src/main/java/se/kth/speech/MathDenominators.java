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

import java.util.Iterator;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 8 Mar 2017
 *
 */
public final class MathDenominators {

	/**
	 * @see <a href=
	 *      "http://stackoverflow.com/a/4009247/1391325">StackOverflow</a>
	 * @param first
	 *            The first value.
	 * @param next
	 *            The other value.
	 * @return The greatest common denominator.
	 */
	public static final int gcd(final int first, final int next) {
		if (next == 0) {
			return first;
		}
		return gcd(next, first % next);
	}

	/**
	 * @see <a href=
	 *      "http://stackoverflow.com/a/15351278/1391325">StackOverflow</a>
	 * @param first
	 *            The first value.
	 * @param next
	 *            The next values.
	 * @return The greatest common denominator.
	 */
	public static final int gcd(final int first, final int... next) {
		int result = first;
		for (final int number : next) {
			result = gcd(result, number);
		}
		return result;
	}

	/**
	 * @see <a href=
	 *      "http://stackoverflow.com/a/15351278/1391325">StackOverflow</a>
	 * @param values
	 *            The values to get the GCD of.
	 * @return The greatest common denominator.
	 */
	public static final int gcd(final Iterator<Integer> values) {
		int result = values.next();
		while (values.hasNext()) {
			result = gcd(result, values.next());
		}
		return result;
	}

	/**
	 *
	 */
	private MathDenominators() {
	}

}
