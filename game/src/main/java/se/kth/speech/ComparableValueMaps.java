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

import java.util.Collections;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since 16 Mar 2017
 *
 */
public final class ComparableValueMaps {

	public static <K, V extends Comparable<? super V>> Entry<Set<K>, V> findMinValues(final Map<K, V> counts) {
		final V val = Collections.min(counts.values());
		final Set<K> keys = counts.entrySet().stream().filter(entry -> Objects.equals(entry.getValue(), val))
				.map(Entry::getKey).collect(Collectors.toSet());
		return new MutablePair<>(keys, val);
	}

	public static <K> void incrementCount(final Map<? super K, Integer> counts, final K key) {
		counts.compute(key, (k, oldVal) -> {
			final Integer newVal;
			if (oldVal == null) {
				newVal = 1;
			} else {
				newVal = oldVal + 1;
			}
			return newVal;
		});
	}

	/**
	 *
	 */
	private ComparableValueMaps() {
	}
}
