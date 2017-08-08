/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.analysis.
 *
 *  se.kth.speech.coin.tangrams-restricted.analysis is free software: you can redistribute it and/or modify
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
import java.util.Map.Entry;
import java.util.function.Predicate;
import java.util.stream.Stream;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 30 May 2017
 *
 */
public final class Iterators {

	public static <T> Entry<Stream<T>, T> findElementsBeforeDelimiter(final Iterator<? extends T> iter,
			final Predicate<? super T> delimiterMatcher) {
		final Entry<Stream<T>, T> result;
		if (iter.hasNext()) {
			final Stream.Builder<T> elemsBefore = Stream.builder();
			T next = null;
			do {
				next = iter.next();
				if (delimiterMatcher.test(next)) {
					break;
				} else {
					elemsBefore.add(next);
				}
			} while (iter.hasNext());
			result = new MutablePair<>(elemsBefore.build(), next);
		} else {
			result = new MutablePair<>(Stream.empty(), null);
		}
		return result;
	}

	private Iterators() {
	}

}
