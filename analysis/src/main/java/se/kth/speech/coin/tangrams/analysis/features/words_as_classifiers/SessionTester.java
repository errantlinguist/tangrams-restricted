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
import java.util.Optional;
import java.util.concurrent.ExecutionException;
import java.util.function.Supplier;

import javax.inject.Inject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.cache.LoadingCache;

import se.kth.speech.MutablePair;
import se.kth.speech.coin.tangrams.analysis.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.SessionDataManager;
import se.kth.speech.coin.tangrams.analysis.SessionEventDialogueManager;
import se.kth.speech.coin.tangrams.analysis.features.ClassificationException;

public final class SessionTester {

	public static final class Result {

		private final List<Entry<EventDialogue, EventDialogueTester.Result>> diagTestResults;

		public Result(final List<Entry<EventDialogue, EventDialogueTester.Result>> diagResults) {
			diagTestResults = diagResults;
		}

		public void add(final Result other) {
			diagTestResults.addAll(other.diagTestResults);
		}

		/**
		 * @return the diagResults
		 */
		public List<Entry<EventDialogue, EventDialogueTester.Result>> getDiagResults() {
			return diagTestResults;
		}

		public double meanReciprocalRank() {
			final double rrSum = sumReciprocalRank();
			return rrSum / totalUtterancesTested();
		}

		public double meanUttsPerDialogue() {
			final int totalUtts = totalUtterancesTested();
			return totalUtts / (double) totalDiagsTested();
		}

		public int totalDiagsTested() {
			return diagTestResults.size();
		}

		public int totalUtterancesTested() {
			return diagTestResults.stream().map(Entry::getValue)
					.mapToInt(EventDialogueTester.Result::totalUtterancesTested).sum();
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
	private Supplier<LoadingCache<SessionDataManager, SessionEventDialogueManager>> sessionDiagMgrCacheSupplier;

//	private Instances testInsts;
	
	private final EventDialogueTester diagClassifier;
	
	public SessionTester(final EventDialogueTester diagClassifier){
		this.diagClassifier = diagClassifier;
	}

//	/**
//	 * @param testInsts
//	 *            the testInsts to set
//	 */
//	public void setTestInsts(final Instances testInsts) {
//		this.testInsts = testInsts;
//	}
//
//	/**
//	 * @param wordClassifiers
//	 *            the wordClassifiers to set
//	 */
//	public void setWordClassifiers(final Function<? super String, ? extends Classifier> wordClassifiers) {
//		this.wordClassifiers = wordClassifiers;
//	}

	public Result testSession(final SessionDataManager sessionData)
			throws ExecutionException, ClassificationException {
		final SessionEventDialogueManager sessionEventDiagMgr = sessionDiagMgrCacheSupplier.get().get(sessionData);
		final List<EventDialogue> uttDiags = sessionEventDiagMgr.createUttDialogues();
		final Result result = new Result(new ArrayList<>(uttDiags.size()));
		for (final EventDialogue uttDiag : uttDiags) {
			final Optional<EventDialogueTester.Result> optTestResults = diagClassifier.apply(uttDiag,
					sessionEventDiagMgr.getGameHistory());
			if (optTestResults.isPresent()) {
				final EventDialogueTester.Result results = optTestResults.get();
				result.diagTestResults.add(new MutablePair<>(uttDiag, results));
			} else {
				LOGGER.debug("No utterances tested for {}.", uttDiag);
			}
		}
		return result;
	}

//	private Instance createEntityTestInstance(final EntityFeature.Extractor.Context extractionContext) {
//		final Instance result = new DenseInstance(entInstAttrCtx.getAttrs().size());
//		result.setDataset(testInsts);
//		entInstAttrCtx.getExtractor().accept(result, extractionContext);
//		result.setClassMissing();
//		return result;
//	}

}