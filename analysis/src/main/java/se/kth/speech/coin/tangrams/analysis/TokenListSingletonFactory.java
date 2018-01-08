/*
 *  This file is part of Tangrams-restricted.
 *
 *  Tangrams-restricted is free software: you can redistribute it and/or modify
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
package se.kth.speech.coin.tangrams.analysis;

import java.lang.ref.Reference;
import java.lang.ref.SoftReference;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.function.Function;

final class TokenListSingletonFactory implements Function<String[], List<String>> {

	private final ConcurrentMap<List<String>, Reference<List<String>>> singletonInstances;

	TokenListSingletonFactory(final int expectedUniqueTokenSequenceCount) {
		singletonInstances = new ConcurrentHashMap<>(expectedUniqueTokenSequenceCount);
	}

	@Override
	public List<String> apply(final String[] tokens) {
		return apply(Arrays.asList(tokens));
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder(48 * (singletonInstances.size() + 1));
		builder.append("TokenListSingletonFactory [singletonInstances=");
		builder.append(singletonInstances);
		builder.append("]");
		return builder.toString();
	}

	private List<String> apply(final List<String> tokens) {
		return singletonInstances.compute(tokens, (key, oldValue) -> {
			final Reference<List<String>> newValue;
			if (oldValue == null || oldValue.get() == null) {
				final List<String> internedTokens = Arrays
						.asList(key.stream().map(String::intern).toArray(String[]::new));
				newValue = new SoftReference<>(Collections.unmodifiableList(internedTokens));
			} else {
				newValue = oldValue;
			}
			return newValue;
		}).get();
	}
}