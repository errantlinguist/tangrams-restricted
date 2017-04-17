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
package se.kth.speech.coin.tangrams.analysis.features;

import java.util.Collection;

import it.unimi.dsi.fastutil.objects.Object2DoubleMap;
import it.unimi.dsi.fastutil.objects.Object2DoubleOpenHashMap;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 12 Apr 2017
 *
 */
final class FeatureMaps {

	static <K> Object2DoubleMap<K> createOrdinalFeatureValMap(final Collection<? extends K> keys,
			final double nullValue) {
		final Object2DoubleMap<K> result = new Object2DoubleOpenHashMap<>(keys.size());
		result.defaultReturnValue(nullValue);
		putOrdinalFeatureVals(result, keys);
		return result;
	}

	static <K> void putOrdinalFeatureVals(final Object2DoubleMap<K> map, final Collection<? extends K> keys) {
		for (final K key : keys) {
			final double featureVal = map.size();
			map.put(key, featureVal);
		}
	}

	private FeatureMaps() {

	}
}
