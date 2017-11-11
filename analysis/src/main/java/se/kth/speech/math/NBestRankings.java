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
package se.kth.speech.math;

import java.util.function.DoubleFunction;

import it.unimi.dsi.fastutil.doubles.Double2ObjectRBTreeMap;
import it.unimi.dsi.fastutil.doubles.Double2ObjectSortedMap;
import it.unimi.dsi.fastutil.doubles.DoubleComparators;
import it.unimi.dsi.fastutil.ints.IntCollection;
import it.unimi.dsi.fastutil.objects.ObjectIterable;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 17 May 2017
 *
 */
public final class NBestRankings {

	public static <G extends IntCollection> Double2ObjectSortedMap<G> createNbestGroupMap(
			final double[] observationReferenceConfidenceVals,
			final DoubleFunction<G> tieGroupContainerFactory) {
		final Double2ObjectSortedMap<G> result = new Double2ObjectRBTreeMap<>(DoubleComparators.OPPOSITE_COMPARATOR);
		for (int entityId = 0; entityId < observationReferenceConfidenceVals.length; ++entityId) {
			final double confidenceVal = observationReferenceConfidenceVals[entityId];
			G observationIds = result.get(confidenceVal);
			if (observationIds == null) {
				observationIds = tieGroupContainerFactory.apply(confidenceVal);
				result.put(confidenceVal, observationIds);
			}
			observationIds.add(entityId);
		}
		return result;
	}

	public static double findAveragedRank(final ObjectIterable<? extends IntCollection> nbestGroups,
			final int observationId) {
		int bestRankForTiedGroup = 1;
		IntCollection tiedObservationIds = null;
		for (final IntCollection nbestGroup : nbestGroups) {
			if (nbestGroup.contains(observationId)) {
				tiedObservationIds = nbestGroup;
				break;
			}
			bestRankForTiedGroup += nbestGroup.size();
		}

		final double result;
		if (tiedObservationIds == null) {
			throw new IllegalArgumentException("ID not found.");
		} else {
			// Average the ranks for sets of ties
			final int tiedObservationCount = tiedObservationIds.size();
			double groupRankSum = 0.0;
			for (int i = 0; i < tiedObservationCount; ++i) {
				final int observationRank = bestRankForTiedGroup + i;
				groupRankSum += observationRank;
			}
			result = groupRankSum / tiedObservationCount;
		}
		return result;
	}

	private NBestRankings() {
	}

}
