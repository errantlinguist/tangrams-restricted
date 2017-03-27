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

import java.util.Collection;
import java.util.Iterator;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 3 Jan 2017
 *
 */
public final class CyclingIterator<T> implements Iterator<T> {

	private final Collection<T> coll;

	private Iterator<T> decorated;

	private int iterCount = 0;

	public CyclingIterator(final Collection<T> coll) {
		this.coll = coll;
		this.decorated = coll.iterator();
	}

	public boolean add(final T e) {
		final boolean result = coll.add(e);
		if (result) {
			// Reset iterator in order to avoid ConcurrentModificationException
			resetDecorated();
			for (int iter = 0; iter < iterCount; ++iter) {
				decorated.next();
			}
		}
		return result;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.Iterator#hasNext()
	 */
	@Override
	public boolean hasNext() {
		return !coll.isEmpty();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.Iterator#next()
	 */
	@Override
	public T next() {
		// If the decorated Iterator used to have elements to return but no
		// longer, create a new Iterator for the Iterable backing the current
		// one
		if (hasNext() && !decorated.hasNext()) {
			resetDecorated();
		}
		final T result = decorated.next();
		iterCount++;
		return result;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder();
		builder.append("CyclingIterator [coll=");
		builder.append(coll);
		builder.append(", iterCount=");
		builder.append(iterCount);
		builder.append("]");
		return builder.toString();
	}

	private void resetDecorated() {
		decorated = coll.iterator();
		iterCount = 0;
	}

}
