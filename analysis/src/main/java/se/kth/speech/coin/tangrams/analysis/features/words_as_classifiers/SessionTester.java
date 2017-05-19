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
import java.util.Arrays;
import java.util.List;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.concurrent.ExecutionException;
import java.util.function.Function;
import java.util.function.Supplier;

import javax.inject.Inject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.BeanFactory;

import com.google.common.cache.LoadingCache;

import it.unimi.dsi.fastutil.ints.Int2IntMap;
import it.unimi.dsi.fastutil.ints.Int2IntMaps;
import it.unimi.dsi.fastutil.ints.Int2IntOpenHashMap;
import it.unimi.dsi.fastutil.ints.IntSet;
import se.kth.speech.MutablePair;
import se.kth.speech.coin.tangrams.analysis.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.SessionDataManager;
import se.kth.speech.coin.tangrams.analysis.SessionEventDialogueManager;
import se.kth.speech.coin.tangrams.analysis.Utterance;
import se.kth.speech.coin.tangrams.analysis.features.ClassificationException;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeature;
import se.kth.speech.coin.tangrams.analysis.features.weka.EntityInstanceAttributeContext;
import se.kth.speech.fastutil.IntMaps;
import weka.classifiers.Classifier;
import weka.core.Instance;
import weka.core.Instances;

public final class SessionTester {

	public static final class Result implements SessionTestStatistics {

		private final List<Entry<EventDialogue, EventDialogueTester.Result>> diagTestResults;

		private final Int2IntMap goldStdReferentIdCounts;

		public Result(final int expectedDiagTestCount) {
			diagTestResults = new ArrayList<>(expectedDiagTestCount);
			goldStdReferentIdCounts = new Int2IntOpenHashMap(expectedDiagTestCount);
			goldStdReferentIdCounts.defaultReturnValue(0);
		}

		/**
		 * @param uttDiag
		 * @param results
		 */
		public void add(final Entry<EventDialogue, EventDialogueTester.Result> diagTestResults) {
			this.diagTestResults.add(diagTestResults);
			final int refId = diagTestResults.getValue().getGoldStandardReferentId();
			goldStdReferentIdCounts.put(refId, goldStdReferentIdCounts.get(refId) + 1);
		}

		public void add(final Result other) {
			other.diagTestResults.forEach(this::add);
			diagTestResults.addAll(other.diagTestResults);
		}

		/**
		 * @return the diagResults
		 */
		public List<Entry<EventDialogue, EventDialogueTester.Result>> getDialogueTestResults() {
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
		 * EventDialogueTestStatistics#getUtterancesTested()
		 */
		@Override
		public List<Utterance> getUtterancesTested() {
			return Arrays.asList(
					diagTestResults.stream().map(Entry::getValue).map(EventDialogueTestStatistics::getUtterancesTested)
							.map(List::stream).flatMap(Function.identity()).toArray(Utterance[]::new));
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
			return rrSum / totalUtterancesTested();
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
			return diagTestResults.stream().map(Entry::getValue).mapToInt(EventDialogueTester.Result::totalTokensTested)
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
			return diagTestResults.stream().map(Entry::getValue)
					.mapToInt(EventDialogueTester.Result::totalUtteranceCount).sum();
		}

		@Override
		public int totalUtterancesTested() {
			return diagTestResults.stream().map(Entry::getValue)
					.mapToInt(EventDialogueTester.Result::totalUtterancesTested).sum();
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

		private double sumRank() {
			double result = 0.0;
			for (final Entry<EventDialogue, EventDialogueTester.Result> diagTestResultsEntry : diagTestResults) {
				final EventDialogueTester.Result diagTestResults = diagTestResultsEntry.getValue();
				result += diagTestResults.rank();
			}
			return result;
		}

		private double sumReciprocalRank() {
			double result = 0.0;
			// TODO: Find a better way to calculate MRR, which avoids cumulative
			// floating-point precision errors
			for (final Entry<EventDialogue, EventDialogueTester.Result> diagTestResultsEntry : diagTestResults) {
				final EventDialogueTester.Result diagTestResults = diagTestResultsEntry.getValue();
				final double diagRRSum = diagTestResults.reciprocalRank();
				result += diagRRSum;
			}
			return result;
		}

	}

	private static final Logger LOGGER = LoggerFactory.getLogger(SessionTester.class);

	@Inject
	private BeanFactory beanFactory;

	@Inject
	private EntityInstanceAttributeContext entInstAttrCtx;

	@Inject
	private Supplier<LoadingCache<SessionDataManager, SessionEventDialogueManager>> sessionDiagMgrCacheSupplier;

	private final Function<EntityFeature.Extractor.Context, Instance> testInstFactory;

	private final Function<? super String, ? extends Classifier> wordClassifiers;

	/**
	 * @param wordClassifiers
	 * @param testInsts
	 */
	public SessionTester(final Function<? super String, ? extends Classifier> wordClassifiers,
			final Instances testInsts) {
		this.wordClassifiers = wordClassifiers;
		testInstFactory = ctx -> {
			final Instance result = entInstAttrCtx.createInstance(ctx, testInsts);
			result.setClassMissing();
			return result;
		};
	}

	public SessionTester.Result testSession(final SessionDataManager sessionData)
			throws ExecutionException, ClassificationException {
		final SessionEventDialogueManager sessionEventDiagMgr = sessionDiagMgrCacheSupplier.get().get(sessionData);
		final List<EventDialogue> uttDiags = sessionEventDiagMgr.createUttDialogues();
		final SessionTester.Result result = new SessionTester.Result(uttDiags.size());

		final ReferentConfidenceMapFactory referentConfidenceMapFactory = beanFactory
				.getBean(ReferentConfidenceMapFactory.class, wordClassifiers, testInstFactory);
		final UtteranceSequenceClassifier uttSeqClassifier = beanFactory.getBean(UtteranceSequenceClassifier.class,
				referentConfidenceMapFactory);
		final EventDialogueTester diagTester = beanFactory.getBean(EventDialogueTester.class, uttSeqClassifier);
		for (final EventDialogue uttDiag : uttDiags) {
			final Optional<EventDialogueTester.Result> optTestResults = diagTester.apply(uttDiag,
					sessionEventDiagMgr.getGameHistory());
			if (optTestResults.isPresent()) {
				final EventDialogueTester.Result results = optTestResults.get();
				result.add(new MutablePair<>(uttDiag, results));
			} else {
				LOGGER.debug("No utterances tested for {}.", uttDiag);
			}
		}
		return result;
	}

}