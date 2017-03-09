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

import java.lang.reflect.Array;
import java.util.Arrays;
import java.util.Random;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 3 Jan 2017
 *
 */
public final class ObjectArrays {

	/**
	 * @see <a href="http://stackoverflow.com/a/80503/1391325">Source on SO</a>
	 * @param a
	 * @param b
	 * @return
	 */
	public static <T> T[] concatenate(final T[] a, final T[] b) {
		final int aLen = a.length;
		final int bLen = b.length;

		@SuppressWarnings("unchecked")
		final T[] c = (T[]) Array.newInstance(a.getClass().getComponentType(), aLen + bLen);
		System.arraycopy(a, 0, c, 0, aLen);
		System.arraycopy(b, 0, c, aLen, bLen);

		return c;
	}

	public static <T> T[] copy(final T[] arr) {
		return Arrays.copyOf(arr, arr.length);
	}

	/**
	 *
	 * @param array
	 *            The array to shuffle.
	 * @param random
	 *            The {@link Random} instance to use for generating random
	 *            values.
	 * @see <a href="http://stackoverflow.com/a/1520212/1391325">Source on
	 *      SO</a>
	 */
	public static <T> void shuffle(final T[] array, final Random random) {
		for (int i = array.length - 1; i > 0; i--) {
			final int index = random.nextInt(i + 1);
			// Simple swap
			final T a = array[index];
			array[index] = array[i];
			array[i] = a;
		}
	}

	private ObjectArrays() {

	}

}
