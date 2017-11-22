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
public final class RandomCollectionElementChooser {

	/**
	 * The {@link Random} instance used for randomization.
	 */
	private final Random rnd;

	/**
	 *
	 * @param rnd
	 *            The {@link Random} instance to use for randomization.
	 */
	public RandomCollectionElementChooser(final Random rnd) {
		this.rnd = rnd;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(final Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (!(obj instanceof RandomCollectionElementChooser)) {
			return false;
		}
		final RandomCollectionElementChooser other = (RandomCollectionElementChooser) obj;
		if (rnd == null) {
			if (other.rnd != null) {
				return false;
			}
		} else if (!rnd.equals(other.rnd)) {
			return false;
		}
		return true;
	}

	/**
	 * <strong>NOTE:</strong> For {@link Collection} instances which do not
	 * implement both {@link List} and {@link RandomAccess}, the complexity of
	 * this method is linear to the {@link Collection#size() size} of the given
	 * {@link Collection}.
	 *
	 * @param <T>
	 *            The type of the element returned.
	 * @param coll
	 *            The {@code Collection} to get a random element from.
	 * @param rnd
	 *            The {@link Random} instance to use for getting a random
	 *            element.
	 * @return A random element.
	 */
	public <T> T getRandomElement(final Collection<? extends T> coll) {
		final T result;
		if (coll instanceof RandomAccess && coll instanceof List) {
			final List<? extends T> downcast = (List<? extends T>) coll;
			result = getRandomElement(downcast);
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

	public int getRandomElement(final int[] array) {
		return array[rnd.nextInt(array.length)];
	}

	public <T> T getRandomElement(final List<? extends T> list) {
		return list.get(rnd.nextInt(list.size()));
	}

	public <T> T getRandomElement(final T[] array) {
		return array[rnd.nextInt(array.length)];
	}

	/**
	 * @return The {@link Random} instance used for randomization.
	 */
	public Random getRnd() {
		return rnd;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (rnd == null ? 0 : rnd.hashCode());
		return result;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder(64);
		builder.append("RandomCollectionElementChooser [rnd=");
		builder.append(rnd);
		builder.append("]");
		return builder.toString();
	}

}
