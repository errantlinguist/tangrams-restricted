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
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.inject.Inject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.BeanFactory;

import com.google.common.collect.Maps;

import it.unimi.dsi.fastutil.ints.IntSet;
import se.kth.speech.coin.tangrams.analysis.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.SessionDataManager;
import se.kth.speech.coin.tangrams.analysis.Utterance;
import se.kth.speech.coin.tangrams.analysis.features.ClassificationException;
import se.kth.speech.coin.tangrams.analysis.features.TrainingException;
import se.kth.speech.coin.tangrams.analysis.features.weka.WordClassInstancesFactory;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.EventDialogueTester;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.SessionTestStatistics;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.SessionTester;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.WordClassDiscountingSmoother;
import weka.classifiers.functions.Logistic;
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

	public static final class Result implements SessionTestStatistics {

		private final int iterCount;

		private final ConcurrentMap<Path, List<SessionTester.Result>> sessionResults;

		private final SessionTester.Result totalResults;

		private Result(final int expectedSessionCount, final int iterCount) {
			sessionResults = new ConcurrentHashMap<>(expectedSessionCount);
			totalResults = new SessionTester.Result(expectedSessionCount * 50);
			this.iterCount = iterCount;
		}

		public Stream<Entry<EventDialogue, EventDialogueTester.Result>> getAllDialogueTestResults() {
			return sessionResults.values().stream().flatMap(List::stream)
					.map(SessionTester.Result::getDialogueTestResults).flatMap(List::stream);
		}

		/**
		 * @return the sessionResults
		 */
		public Map<Path, List<SessionTester.Result>> getSessionResults() {
			return Collections.unmodifiableMap(sessionResults);
		}

		/**
		 * @return the iterCount
		 */
		public int iterCount() {
			return iterCount;
		}

		public double meanGoldStandardUniqueReferentIdCount() {
			return sessionResults.values().stream().flatMap(List::stream)
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
			return sessionResults.values().stream().flatMap(List::stream)
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

		public void put(final Path infilePath, final int iterNo, final SessionTester.Result testResults) {
			sessionResults.compute(infilePath, (key, oldVal) -> {
				final List<SessionTester.Result> newVal;
				if (oldVal == null) {
					newVal = Arrays.asList(new SessionTester.Result[iterCount]);
				} else {
					newVal = oldVal;
				}
				final SessionTester.Result oldResults = newVal.set(iterNo - 1, testResults);
				assert oldResults == null;
				return newVal;
			});
			testResults.getDialogueTestResults().forEach(totalResults::add);
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
			return sessionResults.values().stream().flatMap(List::stream)
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
			return sessionResults.values().stream().flatMap(List::stream).map(SessionTestStatistics::utterancesTested)
					.flatMap(Function.identity());
		}

	}

	private final class CrossValidator implements Runnable {

		private final Map<SessionDataManager, Path> allSessions;

		private final int iterNo;

		private final Result result;

		private CrossValidator(final Map<SessionDataManager, Path> allSessions, final int iterNo, final Result result) {
			this.allSessions = allSessions;
			this.iterNo = iterNo;
			this.result = result;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.lang.Runnable#run()
		 */
		@Override
		public void run() {
			LOGGER.info("Training/testing iteration no. {}.", iterNo);
			try {
				final Map<Path, SessionTester.Result> iterResults = crossValidate(allSessions);
				for (final Entry<Path, SessionTester.Result> iterResult : iterResults.entrySet()) {
					result.put(iterResult.getKey(), iterNo, iterResult.getValue());
				}
			} catch (ExecutionException | TrainingException | IOException | ClassificationException e) {
				throw new RuntimeException(e);
			}
		}

		private Map<Path, SessionTester.Result> crossValidate(final Map<SessionDataManager, Path> allSessions)
				throws ExecutionException, TrainingException, IOException, ClassificationException {
			final Map<Path, SessionTester.Result> result = Maps.newHashMapWithExpectedSize(allSessions.size());
			final Stream<Entry<SessionDataManager, Map<String, Instances>>> testSets = testSetFactory
					.apply(allSessions);
			for (final Iterator<Entry<SessionDataManager, Map<String, Instances>>> testSetIter = testSets
					.iterator(); testSetIter.hasNext();) {
				final Entry<SessionDataManager, Map<String, Instances>> testSet = testSetIter.next();
				final SessionDataManager testSessionData = testSet.getKey();
				final Path infilePath = allSessions.get(testSessionData);
				LOGGER.info("Running cross-validation test on data from \"{}\".", infilePath);

				final Map<String, Instances> classInstances = testSet.getValue();
				final Instances oovInstances = smoother.redistributeMass(classInstances);
				LOGGER.debug("{} instances for out-of-vocabulary class.", oovInstances.size());

				final Map<String, Logistic> wordClassifiers = createWordClassifierMap(classInstances.entrySet());
				final Instances testInsts = testInstsFactory.apply(TEST_INSTS_REL_NAME,
						estimateTestInstanceCount(testSessionData));
				final Function<String, Logistic> wordClassifierGetter = wordClassifiers::get;
				final SessionTester sessionTester = beanFactory.getBean(SessionTester.class, wordClassifierGetter,
						testInsts);

				final SessionTester.Result testResults = sessionTester.testSession(testSessionData);
				result.put(infilePath, testResults);
			}
			return result;
		}

	}

	private static final Logger LOGGER = LoggerFactory.getLogger(Tester.class);

	private static final String TEST_INSTS_REL_NAME = "tested_entites";

	private static Logistic createWordClassifier(final Instances data) throws TrainingException {
		final Logistic result = new Logistic();
		try {
			result.buildClassifier(data);
		} catch (final Exception e) {
			throw new TrainingException(e);
		}
		return result;
	}

	private static Map<String, Logistic> createWordClassifierMap(final Set<Entry<String, Instances>> classInstances)
			throws TrainingException {
		final Map<String, Logistic> result = Maps.newHashMapWithExpectedSize(classInstances.size());
		for (final Entry<String, Instances> classInstancesEntry : classInstances) {
			final String className = classInstancesEntry.getKey();
			LOGGER.debug("Training classifier for class \"{}\".", className);
			final Instances trainingInsts = classInstancesEntry.getValue();
			final Logistic classifier = createWordClassifier(trainingInsts);
			final Logistic oldClassifier = result.put(className, classifier);
			if (oldClassifier != null) {
				throw new IllegalArgumentException(
						String.format("More than one file for word class \"%s\".", className));
			}
			LOGGER.debug("{} instance(s) for class \"{}\".", trainingInsts.size(), className);
		}
		return result;
	}

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

	private final Supplier<? extends ExecutorService> executorFactory;

	private final int iterCount;

	@Inject
	private WordClassDiscountingSmoother smoother;

	@Inject
	private WordClassInstancesFactory testInstsFactory;

	@Inject
	private TestSetFactory testSetFactory;

	public Tester(final int iterCount) {
		this(iterCount, Executors::newSingleThreadExecutor);
	}

	public Tester(final int iterCount, final int parallelThreadCount) {
		this(iterCount, () -> Executors.newFixedThreadPool(parallelThreadCount));
	}

	public Tester(final int iterCount, final Supplier<? extends ExecutorService> executorFactory) {
		this.iterCount = iterCount;
		this.executorFactory = executorFactory;
	}

	public Result apply(final Iterable<Path> inpaths) throws IOException {
		final Map<Path, SessionDataManager> infileSessionData = SessionDataManager.createFileSessionDataMap(inpaths);
		final Map<SessionDataManager, Path> allSessions = infileSessionData.entrySet().stream()
				.collect(Collectors.toMap(Entry::getValue, Entry::getKey));
		infileSessionData.forEach((infile, sessionData) -> allSessions.put(sessionData, infile));

		final Result result = new Result(allSessions.size(), iterCount);
		LOGGER.info(
				"Starting cross-validation test using data from {} session(s), doing {} iterations on each dataset.",
				allSessions.size(), iterCount);
		final ExecutorService executor = executorFactory.get();
		try {
			final Stream.Builder<CompletableFuture<Void>> iterResultFutures = Stream.builder();
			for (int iter = 1; iter <= iterCount; ++iter) {
				final CompletableFuture<Void> iterResultFuture = CompletableFuture
						.runAsync(new CrossValidator(allSessions, iter, result), executor);
				iterResultFutures.add(iterResultFuture);
			}
			final CompletableFuture<Void> allDone = CompletableFuture
					.allOf(iterResultFutures.build().toArray(CompletableFuture[]::new));
			allDone.join();
			LOGGER.info("Finished testing {} cross-validation dataset(s).", result.sessionResults.size());
		} finally {
			LOGGER.debug("Shutting down executor service.");
			executor.shutdown();
			LOGGER.debug("Successfully shut down executor service.");
		}
		return result;
	}

}
