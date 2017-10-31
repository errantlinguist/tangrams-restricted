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
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 31 Oct 2017
 *
 */
public final class SetCombinations {

	/**
	 *
	 * @param originalSet
	 *            The {@link Set} to create a powerset for.
	 * @return A powerset of the given set.
	 *
	 * @see <a href=
	 *      "https://stackoverflow.com/a/1670871/1391325">StackOverflow</a>
	 */
	public static <T> Set<Set<T>> powerSet(final Set<T> originalSet) {
		final Set<Set<T>> sets = new HashSet<>();
		if (originalSet.isEmpty()) {
			sets.add(new HashSet<T>());
			return sets;
		}
		final List<T> list = new ArrayList<>(originalSet);
		final T head = list.get(0);
		final Set<T> rest = new HashSet<>(list.subList(1, list.size()));
		for (final Set<T> set : powerSet(rest)) {
			final Set<T> newSet = new HashSet<>();
			newSet.add(head);
			newSet.addAll(set);
			sets.add(newSet);
			sets.add(set);
		}
		return sets;
	}

	private SetCombinations() {

	}
}
