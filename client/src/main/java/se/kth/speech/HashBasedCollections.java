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

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 15 Mar 2017
 *
 */
public final class HashBasedCollections {

	/**
	 * The largest power of two that can be represented as an {@code int}.
	 *
	 * @see <a href="https://github.com/google/guava">Google Core Libraries for
	 *      Java</a>
	 */
	public static final int INT_MAX_POWER_OF_TWO = 1 << Integer.SIZE - 2;

	/**
	 * @see <a href="https://github.com/google/guava">Google Core Libraries for
	 *      Java</a>
	 */
	public static int capacity(final int expectedSize) {
		if (expectedSize < 3) {
			return expectedSize + 1;
		}
		if (expectedSize < INT_MAX_POWER_OF_TWO) {
			return expectedSize + expectedSize / 3;
		}
		return Integer.MAX_VALUE; // any large value
	}

	private HashBasedCollections() {

	}

}
