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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers;

import java.util.List;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import it.unimi.dsi.fastutil.doubles.Double2ObjectSortedMap;
import it.unimi.dsi.fastutil.ints.Int2DoubleMap;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import it.unimi.dsi.fastutil.ints.IntList;
import se.kth.speech.coin.tangrams.analysis.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.Utterance;
import se.kth.speech.math.NBestRankings;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 1 Jun 2017
 *
 */
public final class EventDialogueTestResults implements EventDialogueTestStatistics {

	private static final Logger LOGGER = LoggerFactory.getLogger(EventDialogueTestResults.class);

	private final int goldStandardReferentId;

	private final Int2DoubleMap referentConfidenceVals;

	private final int totalUttCount;

	private final EventDialogue transformedDiag;

	public EventDialogueTestResults(final Int2DoubleMap referentConfidenceVals, final int goldStandardReferentId,
			final EventDialogue transformedDiag, final int totalDiagUttCount) {
		this.referentConfidenceVals = referentConfidenceVals;
		this.goldStandardReferentId = goldStandardReferentId;
		this.transformedDiag = transformedDiag;
		totalUttCount = totalDiagUttCount;
	}

	/**
	 * @return the goldStandardReferentId
	 */
	public int getGoldStandardReferentId() {
		return goldStandardReferentId;
	}

	/**
	 * @return the referentConfidenceVals
	 */
	public Int2DoubleMap getReferentConfidenceVals() {
		return referentConfidenceVals;
	}

	/**
	 * @return the transformedDiag
	 */
	public EventDialogue getTransformedDiag() {
		return transformedDiag;
	}

	public double rank() {
		final Double2ObjectSortedMap<IntList> nbestGroups = NBestRankings.createNbestGroupMap(
				getReferentConfidenceVals().int2DoubleEntrySet(), confidenceVal -> new IntArrayList(1));
		final double result = NBestRankings.findAveragedRank(nbestGroups.values(), getGoldStandardReferentId());
		LOGGER.debug("Rank of correct entity: {}", result);
		return result;
	}

	public double reciprocalRank() {
		return 1.0 / rank();
	}

	@Override
	public int totalTokensTested() {
		return utterancesTested().map(Utterance::getTokens).mapToInt(List::size).sum();
	}

	@Override
	public int totalUtteranceCount() {
		return totalUttCount;
	}

	@Override
	public int totalUtterancesTested() {
		return transformedDiag.getUtts().size();
	}

	/**
	 * @return the uttsTested
	 */
	@Override
	public Stream<Utterance> utterancesTested() {
		return transformedDiag.getUtts().stream();
	}

}
