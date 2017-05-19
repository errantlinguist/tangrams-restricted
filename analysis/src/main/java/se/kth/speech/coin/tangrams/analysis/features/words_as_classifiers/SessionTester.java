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
import java.util.function.Function;
import java.util.function.Supplier;

import javax.inject.Inject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.BeanFactory;

import com.google.common.cache.LoadingCache;

import se.kth.speech.MutablePair;
import se.kth.speech.coin.tangrams.analysis.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.SessionDataManager;
import se.kth.speech.coin.tangrams.analysis.SessionEventDialogueManager;
import se.kth.speech.coin.tangrams.analysis.features.ClassificationException;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeature;
import se.kth.speech.coin.tangrams.analysis.features.weka.EntityInstanceAttributeContext;
import weka.classifiers.Classifier;
import weka.core.Instance;
import weka.core.Instances;

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

		public double meanRank() {
			return sumRank() / totalDiagsTested();
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

	public Result testSession(final SessionDataManager sessionData) throws ExecutionException, ClassificationException {
		final SessionEventDialogueManager sessionEventDiagMgr = sessionDiagMgrCacheSupplier.get().get(sessionData);
		final List<EventDialogue> uttDiags = sessionEventDiagMgr.createUttDialogues();
		final Result result = new Result(new ArrayList<>(uttDiags.size()));

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
				result.diagTestResults.add(new MutablePair<>(uttDiag, results));
			} else {
				LOGGER.debug("No utterances tested for {}.", uttDiag);
			}
		}
		return result;
	}

}