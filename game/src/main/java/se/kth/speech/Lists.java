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

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.OptionalInt;
import java.util.stream.IntStream;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 1 Dec 2016
 *
 */
public final class Lists {

	/**
	 *
	 * @param ordering
	 *            The {@link List} to use for ordering.
	 * @return A {@link Comparator} using a given object's index in the given
	 *         {@code List} as its order, with unseen elements ordered last.
	 * @see <a href="http://stackoverflow.com/a/41128993/1391325">Original SO
	 *      answer</a>
	 */
	public static <T> Comparator<T> comparingByIndex(final List<? extends T> ordering) {
		return (t1, t2) -> Integer.compareUnsigned(ordering.indexOf(t1), ordering.indexOf(t2));
	}

	public static <T> List<T> createGreatestDivisorList(final List<T> orig, final int resultSize) {
		final List<T> result;

		final int cmp = Integer.compare(resultSize, orig.size());
		if (cmp < 0) {
			// Get the beginning slice
			result = orig.subList(0, resultSize);
		} else if (cmp > 0) {
			// http://stackoverflow.com/a/34621495/1391325
			final int from = 1;
			final int to = orig.size() + 1;
			final IntStream divisorsDecreasing = IntStream.range(from, to).map(i -> to - i + from - 1);
			final OptionalInt greatestDivisor = divisorsDecreasing.filter(divisor -> resultSize % divisor == 0)
					.findFirst();
			if (greatestDivisor.isPresent()) {
				result = new ArrayList<>(resultSize);
				final int d = greatestDivisor.getAsInt();
				for (final T pieceColor : orig.subList(0, d)) {
					final int elemsToAdd = resultSize / d;
					for (int i = 0; i < elemsToAdd; ++i) {
						result.add(pieceColor);
					}
				}
			} else {
				throw new IllegalArgumentException(String.format(
						"Could not create a new, longer list because the size of the new list is not divisible by the number of any subsequence of the original list (size %d).",
						orig.size()));
			}
		} else {
			// Exactly the same total number of specified elements in the new
			// list as there are
			// in the original one; Just use the list as-is
			result = orig;
		}

		return result;

	}

	public static <T> Optional<T> getFirstElement(final List<? extends T> list) {
		return list.isEmpty() ? Optional.empty() : Optional.of(list.iterator().next());
	}

	public static <T> Optional<T> getLastElement(final List<? extends T> list) {
		return list.isEmpty() ? Optional.empty() : Optional.of(list.listIterator(list.size()).previous());
	}

	private Lists() {
	}

}
