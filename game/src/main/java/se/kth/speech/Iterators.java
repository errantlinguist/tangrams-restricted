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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.function.Predicate;
import java.util.stream.Stream;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 30 May 2017
 *
 */
public final class Iterators {

	/**
	 * Returns a {@link List} of the remaining elements of an {@link Iterator}.
	 *
	 * @param iter
	 *            The {@code Iterator} to get the remaining elements of.
	 * @return A {@code List} of the remaining elements which is not guaranteed to
	 *         be mutable.
	 */
	public static <T> List<T> createRemainingElementList(final Iterator<? extends T> iter) {
		final List<T> result;
		if (iter.hasNext()) {
			final ArrayList<T> mutableResult = new ArrayList<>();
			do {
				mutableResult.add(iter.next());
			} while (iter.hasNext());
			mutableResult.trimToSize();
			result = mutableResult;
		} else {
			result = Collections.emptyList();
		}
		return result;
	}

	/**
	 * Finds the next element returned by a given {@link Iterator} which matches a
	 * given {@link Predicate} along with all elements iterated through before
	 * matching.
	 *
	 * @param <T>
	 *            The element type.
	 * @param iter
	 *            The {@code Iterator} to retrieve elements from.
	 * @param delimiterMatcher
	 *            A {@link Predicate} matching the delimiting element.
	 * @return A pair comprising a {@link Stream} of elements encountered before the
	 *         delimiting element as well as the element itself.
	 */
	public static <T> Entry<Stream<T>, Optional<T>> findElementsBeforeDelimiter(final Iterator<? extends T> iter,
			final Predicate<? super T> delimiterMatcher) {
		final Entry<Stream<T>, Optional<T>> result;
		Optional<T> nextDelimiting = Optional.empty();
		if (iter.hasNext()) {
			final Stream.Builder<T> elemsBefore = Stream.builder();
			do {
				final T next = iter.next();
				if (delimiterMatcher.test(next)) {
					nextDelimiting = Optional.of(next);
					break;
				} else {
					elemsBefore.add(next);
				}
			} while (iter.hasNext());
			result = new MutablePair<>(elemsBefore.build(), nextDelimiting);
		} else {
			result = new MutablePair<>(Stream.empty(), nextDelimiting);
		}
		return result;
	}

	private Iterators() {
	}

}
