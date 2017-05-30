/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.analysis.
 *
 *  tangrams is free software: you can redistribute it and/or modify
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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.cross_validation;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.function.Function;
import java.util.stream.Stream;

import javax.inject.Inject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.BeanFactory;

import com.google.common.collect.Maps;

import it.unimi.dsi.fastutil.ints.IntSet;
import se.kth.speech.coin.tangrams.analysis.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.SessionDataManager;
import se.kth.speech.coin.tangrams.analysis.SessionEventDialogueManagerCacheSupplier;
import se.kth.speech.coin.tangrams.analysis.Utterance;
import se.kth.speech.coin.tangrams.analysis.features.ClassificationException;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeature;
import se.kth.speech.coin.tangrams.analysis.features.weka.EntityInstanceAttributeContext;
import se.kth.speech.coin.tangrams.analysis.features.weka.WordClassInstancesFactory;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.EventDialogueTester;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.ReferentConfidenceMapFactory;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.SessionTestStatistics;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.SessionTester;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.SingleGameContextReferentEventDialogueTester;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.WordClassDiscountingSmoother;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.EventDialogueClassifier;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.EventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.WordClassificationData;
import weka.classifiers.functions.Logistic;
import weka.core.Instance;
import weka.core.Instances;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Apr 17, 2017
 * @see
 *      <ul>
 *      <li><a href="http://anthology.aclweb.org/W/W15/W15-0124.pdf">Casey
 *      Kennington, Livia Dia, &amp; David Schlangen. &ldquo;A Discriminative
 *      Model for Perceptually-Grounded Incremental Reference Resolution.&rdquo;
 *      In <em>Proceedings of IWCS 2015</em><a>.</li>
 *      <li><a href="http://www.aclweb.org/anthology/P15-1029">Casey Kennington,
 *      &amp; David Schlangen. &ldquo;Simple Learning and Compositional
 *      Application of Perceptually Grounded Word Meanings for Incremental
 *      Reference Resolution&rdquo;. In <em>Proceedings of the 53<sup>rd</sup>
 *      Annual Meeting of the Association for Computational Linguistics and the
 *      7<sup>th</sup> International Joint Conference on Natural Language
 *      Processing</em><a>.</li>
 *      </ul>
 *
 */
public final class Tester {

	public static final class CrossValidationTestSummary {

		private final SessionTester.Result testResults;

		private CrossValidationTestSummary(final SessionTester.Result testResults) {
			this.testResults = testResults;
		}

		/**
		 * @return the testResults
		 */
		public SessionTester.Result getTestResults() {
			return testResults;
		}
	}

	public static final class Result implements SessionTestStatistics {

		private final int iterCount;

		private final ConcurrentMap<Path, List<CrossValidationTestSummary>> sessionResults;

		private final SessionTester.Result totalResults;

		private Result(final int expectedSessionCount, final int iterCount) {
			sessionResults = new ConcurrentHashMap<>(expectedSessionCount);
			totalResults = new SessionTester.Result(expectedSessionCount * 50);
			this.iterCount = iterCount;
		}

		public Stream<Entry<EventDialogue, EventDialogueTester.Result>> getAllDialogueTestResults() {
			return sessionResults.values().stream().flatMap(List::stream).map(CrossValidationTestSummary::getTestResults)
					.map(SessionTester.Result::getDialogueTestResults).flatMap(List::stream);
		}

		/**
		 * @return the sessionResults
		 */
		public Map<Path, List<CrossValidationTestSummary>> getSessionResults() {
			return Collections.unmodifiableMap(sessionResults);
		}

		/**
		 * @return the iterCount
		 */
		public int iterCount() {
			return iterCount;
		}

		public double meanGoldStandardUniqueReferentIdCount() {
			return sessionResults.values().stream().flatMap(List::stream).map(CrossValidationTestSummary::getTestResults)
					.mapToInt(SessionTestStatistics::uniqueGoldStandardReferentIdCount).average().getAsDouble();
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
			return totalResults.meanRank();
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
			return totalResults.meanReciprocalRank();
		}

		public double meanTokensTestedPerSession() {
			return sessionResults.values().stream().flatMap(List::stream).map(CrossValidationTestSummary::getTestResults)
					.mapToInt(SessionTestStatistics::totalTokensTested).average().getAsDouble();
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
			return totalResults.modeReferentIds();
		}

		public void put(final Path infilePath, final int iterNo, final CrossValidationTestSummary cvTestSummary) {
			sessionResults.compute(infilePath, (key, oldVal) -> {
				final List<CrossValidationTestSummary> newVal;
				if (oldVal == null) {
					newVal = Arrays.asList(new CrossValidationTestSummary[iterCount]);
				} else {
					newVal = oldVal;
				}
				final CrossValidationTestSummary oldResults = newVal.set(iterNo - 1, cvTestSummary);
				assert oldResults == null;
				return newVal;
			});
			cvTestSummary.getTestResults().getDialogueTestResults().forEach(totalResults::add);
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see
		 * se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.
		 * CrossValidationTestStatistics#totalDialoguesTested()
		 */
		@Override
		public int totalDialoguesTested() {
			return sessionResults.values().stream().flatMap(List::stream).map(CrossValidationTestSummary::getTestResults)
					.mapToInt(SessionTester.Result::totalDialoguesTested).sum();
		}

		/**
		 * @return the totalResults
		 */
		public SessionTester.Result totalResults() {
			return totalResults;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see
		 * se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.
		 * EventDialogueTestStatistics#totalTokensTested()
		 */
		@Override
		public int totalTokensTested() {
			return totalResults.totalTokensTested();
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see
		 * se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.
		 * EventDialogueTestStatistics#totalUtteranceCount()
		 */
		@Override
		public int totalUtteranceCount() {
			return totalResults.totalUtteranceCount();
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see
		 * se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.
		 * EventDialogueTestStatistics#totalUtterancesTested()
		 */
		@Override
		public int totalUtterancesTested() {
			return totalResults.totalUtterancesTested();
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see
		 * se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.
		 * SessionTestStatistics#uniqueGoldStandardReferentIdCount()
		 */
		@Override
		public int uniqueGoldStandardReferentIdCount() {
			throw new UnsupportedOperationException("Cannot compare reference IDs across sessions.");
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
			return sessionResults.values().stream().flatMap(List::stream).map(CrossValidationTestSummary::getTestResults)
					.map(SessionTestStatistics::utterancesTested).flatMap(Function.identity());
		}

	}

	private static final Logger LOGGER = LoggerFactory.getLogger(Tester.class);

	private static final String TEST_INSTS_REL_NAME = "tested_entites";

	private static int estimateTestInstanceCount(final SessionDataManager sessionData) throws IOException {
		final long lineCount = Files.lines(sessionData.getCanonicalEventLogPath()).count();
		// (Number of logged events / estimated number of events per dialogue) *
		// estimated number of entities per game *
		// estimated number of utterances per dialogue * estimated number of
		// tokens (i.e. n-grams) per utterance
		final long estimate = lineCount / 4 * 20 * 4 * 20;
		int result = Integer.MAX_VALUE;
		try {
			result = Math.toIntExact(estimate);
		} catch (final ArithmeticException e) {
			LOGGER.debug(String.format("Could not convert long value \"%d\" to an int; Returning max.", estimate), e);
		}
		return result;
	}

	@Inject
	private BeanFactory beanFactory;

	private final EventDialogueTransformer diagTransformer;

	@Inject
	private EntityInstanceAttributeContext entInstAttrCtx;

	private final ExecutorService executor;

	private int iterCount = 1;

	@Inject
	private SessionEventDialogueManagerCacheSupplier sessionDiagMgrCacheSupplier;

	@Inject
	private WordClassDiscountingSmoother smoother;

	@Inject
	private WordClassInstancesFactory testInstsFactory;

	private final TestSetFactory testSetFactory;

	public Tester(final TestSetFactory testSetFactory, final EventDialogueTransformer diagTransformer,
			final ExecutorService executor) {
		this.testSetFactory = testSetFactory;
		this.diagTransformer = diagTransformer;
		this.executor = executor;
	}

	public Result apply(final Map<SessionDataManager, Path> allSessions)
			throws ClassificationException, ExecutionException, IOException {
		final Result result = new Result(allSessions.size(), iterCount);
		LOGGER.info(
				"Starting cross-validation test using data from {} session(s), doing {} iteration(s) on each dataset.",
				allSessions.size(), iterCount);
		for (int iterNo = 1; iterNo <= iterCount; ++iterNo) {
			LOGGER.info("Training/testing iteration no. {}.", iterNo);
			final Map<Path, CrossValidationTestSummary> iterResults = crossValidate(allSessions);
			for (final Entry<Path, CrossValidationTestSummary> iterResult : iterResults.entrySet()) {
				result.put(iterResult.getKey(), iterNo, iterResult.getValue());
			}
		}
		LOGGER.info("Finished testing {} cross-validation dataset(s).", result.sessionResults.size());
		return result;
	}

	/**
	 * @return the iterCount
	 */
	public int getIterCount() {
		return iterCount;
	}

	/**
	 * @param iterCount
	 *            the iterCount to set
	 */
	public void setIterCount(final int iterCount) {
		this.iterCount = iterCount;
	}

	private ConcurrentMap<String, Logistic> createWordClassifierMap(
			final Set<Entry<String, Instances>> classInstances) {
		final ConcurrentMap<String, Logistic> result = new ConcurrentHashMap<>(classInstances.size());
		final Stream.Builder<CompletableFuture<Void>> trainingJobs = Stream.builder();
		for (final Entry<String, Instances> classInstancesEntry : classInstances) {
			final String className = classInstancesEntry.getKey();
			LOGGER.debug("Training classifier for class \"{}\".", className);
			final Instances trainingInsts = classInstancesEntry.getValue();
			LOGGER.debug("{} instance(s) for class \"{}\".", trainingInsts.size(), className);
			final CompletableFuture<Void> trainingJob = CompletableFuture.runAsync(() -> {
				try {
					final Logistic classifier = new Logistic();
					classifier.buildClassifier(trainingInsts);
					final Logistic oldClassifier = result.put(className, classifier);
					if (oldClassifier != null) {
						throw new IllegalArgumentException(
								String.format("More than one file for word class \"%s\".", className));
					}
				} catch (final Exception e) {
					throw new RuntimeException(e);
				}
			}, executor);
			trainingJobs.add(trainingJob);
		}
		CompletableFuture.allOf(trainingJobs.build().toArray(CompletableFuture[]::new)).join();
		return result;
	}

	private Map<Path, CrossValidationTestSummary> crossValidate(final Map<SessionDataManager, Path> allSessions)
			throws ExecutionException, IOException, ClassificationException {
		final Map<Path, CrossValidationTestSummary> result = Maps.newHashMapWithExpectedSize(allSessions.size());
		final Stream<Entry<SessionDataManager, WordClassificationData>> testSets = testSetFactory.apply(allSessions);
		for (final Iterator<Entry<SessionDataManager, WordClassificationData>> testSetIter = testSets
				.iterator(); testSetIter.hasNext();) {
			final Entry<SessionDataManager, WordClassificationData> testSet = testSetIter.next();
			final SessionDataManager testSessionData = testSet.getKey();
			final Path infilePath = allSessions.get(testSessionData);
			LOGGER.info("Running cross-validation test on data from \"{}\".", infilePath);

			final WordClassificationData trainingData = testSet.getValue();
			final Optional<Instances> oovInstances = smoother.redistributeMass(trainingData);
			LOGGER.debug("{} instance(s) for out-of-vocabulary class.", oovInstances.map(Instances::size).orElse(0));

			final Function<String, Logistic> wordClassifierGetter = createWordClassifierMap(
					trainingData.getClassInstances().entrySet())::get;
			final Instances testInsts = testInstsFactory.apply(TEST_INSTS_REL_NAME,
					estimateTestInstanceCount(testSessionData));
			final Function<EntityFeature.Extractor.Context, Instance> testInstFactory = entInstAttrCtx
					.createInstFactory(testInsts);
			final ReferentConfidenceMapFactory referentConfidenceMapFactory = beanFactory
					.getBean(ReferentConfidenceMapFactory.class, wordClassifierGetter, testInstFactory);
			final EventDialogueClassifier diagClassifier = beanFactory.getBean(EventDialogueClassifier.class,
					referentConfidenceMapFactory);

			final SingleGameContextReferentEventDialogueTester diagTester = new SingleGameContextReferentEventDialogueTester(
					diagClassifier, diagTransformer);
			final SessionTester sessionTester = new SessionTester(diagTester);
			final SessionTester.Result testResults = sessionTester
					.apply(sessionDiagMgrCacheSupplier.get().get(testSessionData));
			final CrossValidationTestSummary cvTestSummary = new CrossValidationTestSummary(testResults);
			result.put(infilePath, cvTestSummary);
		}
		return result;
	}

}
