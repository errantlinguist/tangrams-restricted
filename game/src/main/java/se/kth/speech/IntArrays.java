/*
 *  This file is part of tangrams.
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
package se.kth.speech;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 3 Jan 2017
 *
 */
public final class IntArrays {

	/**
	 * @see <a href="http://stackoverflow.com/a/80503/1391325">Source on SO</a>
	 * @param a
	 * @param b
	 * @return
	 */
	public static int[] concatenate(final int[] a, final int[] b) {
		final int aLen = a.length;
		final int bLen = b.length;
		final int[] c = new int[aLen + bLen];
		System.arraycopy(a, 0, c, 0, aLen);
		System.arraycopy(b, 0, c, aLen, bLen);
		return c;
	}

	public static int product(final int[] array) {
		if (array.length < 1) {
			throw new IllegalArgumentException("Array is empty.");
		}
		int result = array[0];
		for (int i = 1; i < array.length; ++i) {
			result = result * array[i];
		}
		return result;
	}

	private IntArrays() {

	}

}
