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

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Random;
import java.util.RandomAccess;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 7 Mar 2017
 *
 */
public final class RandomCollections {

	/**
	 * <strong>NOTE:</strong> The complexity of this method is linear to the
	 * {@link Collection#size() size} of the given {@link Collection}.
	 *
	 * @param coll
	 *            The {@code Collection} to get a random element from.
	 * @param rnd
	 *            The {@link Random} instance to use for getting a random
	 *            element.
	 * @return A random element.
	 */
	public static <T> T getRandomElement(final Collection<? extends T> coll, final Random rnd) {
		final T result;
		if (coll instanceof RandomAccess && coll instanceof List) {
			final List<? extends T> downcast = (List<? extends T>) coll;
			result = getRandomElement(downcast, rnd);
		} else {
			final Iterator<? extends T> iter = coll.iterator();
			final int idx = rnd.nextInt(coll.size());
			// Start at one because the last-iterated element should be returned
			for (int i = 1; i < idx; ++i) {
				iter.next();
			}
			result = iter.next();
		}
		return result;
	}

	public static int getRandomElement(final int[] array, final Random rnd) {
		return array[rnd.nextInt(array.length)];
	}

	public static <T> T getRandomElement(final List<? extends T> list, final Random rnd) {
		return list.get(rnd.nextInt(list.size()));
	}

	public static <T> T getRandomElement(final T[] array, final Random rnd) {
		return array[rnd.nextInt(array.length)];
	}

	private RandomCollections() {

	}

}
