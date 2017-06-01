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

import java.util.ArrayList;
import java.util.List;
import java.util.Map.Entry;
import java.util.function.Function;
import java.util.stream.Stream;

import it.unimi.dsi.fastutil.ints.Int2IntMap;
import it.unimi.dsi.fastutil.ints.Int2IntMaps;
import it.unimi.dsi.fastutil.ints.Int2IntOpenHashMap;
import it.unimi.dsi.fastutil.ints.IntSet;
import se.kth.speech.coin.tangrams.analysis.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.Utterance;
import se.kth.speech.fastutil.IntMaps;

public final class SessionTestResults implements SessionTestStatistics {

	private final List<Entry<EventDialogue, EventDialogueTestResults>> diagTestResults;

	private final Int2IntMap goldStdReferentIdCounts;

	public SessionTestResults(final int expectedDiagTestCount) {
		diagTestResults = new ArrayList<>(expectedDiagTestCount);
		goldStdReferentIdCounts = new Int2IntOpenHashMap(expectedDiagTestCount);
		goldStdReferentIdCounts.defaultReturnValue(0);
	}

	/**
	 * @param uttDiag
	 * @param results
	 */
	public void add(final Entry<EventDialogue, EventDialogueTestResults> diagTestResults) {
		this.diagTestResults.add(diagTestResults);
		final int refId = diagTestResults.getValue().getGoldStandardReferentId();
		goldStdReferentIdCounts.put(refId, goldStdReferentIdCounts.get(refId) + 1);
	}

	public void add(final SessionTestResults other) {
		other.diagTestResults.forEach(this::add);
		diagTestResults.addAll(other.diagTestResults);
	}

	/**
	 * @return the diagResults
	 */
	public List<Entry<EventDialogue, EventDialogueTestResults>> getDialogueTestResults() {
		return diagTestResults;
	}

	/**
	 * @return the goldStdReferentIdCounts. <strong>NOTE:</strong> This has
	 *         no meaning across sessions because entity IDs are not stable
	 *         across sessions, i.e.&nbsp;the entity with ID <code>2</code>
	 *         in one game has nothing to do with the entity with ID
	 *         <code>2</code> in another game.
	 */
	public Int2IntMap getGoldStdReferentIdCounts() {
		return Int2IntMaps.unmodifiable(goldStdReferentIdCounts);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.
	 * SessionTestStatistics#meanRank()
	 */
	@Override
	public double meanRank() {
		return sumRank() / totalDialoguesTested();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.
	 * SessionTestStatistics#meanReciprocalRank()
	 */
	@Override
	public double meanReciprocalRank() {
		final double rrSum = sumReciprocalRank();
		return rrSum / totalDialoguesTested();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.
	 * SessionTestStatistics#modeReferentIds()
	 */
	@Override
	public IntSet modeReferentIds() {
		return IntMaps.createMaxValueKeySet(goldStdReferentIdCounts);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.
	 * SessionTestStatistics#totalDialoguesTested()
	 */
	@Override
	public int totalDialoguesTested() {
		return diagTestResults.size();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.
	 * EventDialogueStatistics#totalTokensTested()
	 */
	@Override
	public int totalTokensTested() {
		return diagTestResults.stream().map(Entry::getValue).mapToInt(EventDialogueTestResults::totalTokensTested)
				.sum();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.
	 * EventDialogueStatistics#totalUtteranceCount()
	 */
	@Override
	public int totalUtteranceCount() {
		return diagTestResults.stream().map(Entry::getValue).mapToInt(EventDialogueTestResults::totalUtteranceCount)
				.sum();
	}

	@Override
	public int totalUtterancesTested() {
		return diagTestResults.stream().map(Entry::getValue)
				.mapToInt(EventDialogueTestResults::totalUtterancesTested).sum();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.
	 * SessionTestStatistics#uniqueRefIdCount()
	 */
	@Override
	public int uniqueGoldStandardReferentIdCount() {
		return goldStdReferentIdCounts.size();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.
	 * EventDialogueTestStatistics#utterancesTested()
	 */
	@Override
	public Stream<Utterance> utterancesTested() {
		return diagTestResults.stream().map(Entry::getValue).map(EventDialogueTestStatistics::utterancesTested)
				.flatMap(Function.identity());
	}

	private double sumRank() {
		double result = 0.0;
		for (final Entry<EventDialogue, EventDialogueTestResults> diagTestResultsEntry : diagTestResults) {
			final EventDialogueTestResults diagTestResults = diagTestResultsEntry.getValue();
			result += diagTestResults.rank();
		}
		return result;
	}

	private double sumReciprocalRank() {
		double result = 0.0;
		// TODO: Find a better way to calculate MRR, which avoids cumulative
		// floating-point precision errors
		for (final Entry<EventDialogue, EventDialogueTestResults> diagTestResultsEntry : diagTestResults) {
			final EventDialogueTestResults diagTestResults = diagTestResultsEntry.getValue();
			final double diagRRSum = diagTestResults.reciprocalRank();
			result += diagRRSum;
		}
		return result;
	}

}