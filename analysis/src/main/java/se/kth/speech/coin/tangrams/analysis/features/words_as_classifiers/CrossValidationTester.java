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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.inject.Inject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Maps;

import se.kth.speech.coin.tangrams.analysis.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.SessionDataManager;
import se.kth.speech.coin.tangrams.analysis.features.ClassificationException;
import se.kth.speech.coin.tangrams.analysis.features.TrainingException;
import se.kth.speech.coin.tangrams.analysis.features.weka.ClassInstanceFactory;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.EntityCrossValidationTester.EventDialogueTestResults;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.EntityCrossValidationTester.SessionTestResults;
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
public final class CrossValidationTester {

	public static final class Result {

		private final Map<Path, SessionTestResults> sessionResults;

		private Result(final int expectedSessionCount) {
			sessionResults = Maps.newHashMapWithExpectedSize(expectedSessionCount);
		}

		public Stream<Entry<EventDialogue, EventDialogueTestResults>> getAllDiagTestResults() {
			return sessionResults.values().stream().map(SessionTestResults::getDiagResults).flatMap(List::stream);
		}

		/**
		 * @return the sessionResults
		 */
		public Map<Path, SessionTestResults> getSessionResults() {
			return Collections.unmodifiableMap(sessionResults);
		}

		public int totalDiagsTested() {
			return sessionResults.values().stream().mapToInt(SessionTestResults::totalDiagsTested).sum();
		}

		/**
		 * @return the totalResults
		 */
		public SessionTestResults totalResults() {
			final List<Entry<EventDialogue, EventDialogueTestResults>> totalDiagResults = new ArrayList<>(
					totalDiagsTested());
			final SessionTestResults result = new SessionTestResults(totalDiagResults);
			sessionResults.forEach((inpath, sessionResults) -> result.add(sessionResults));
			return result;
		}
	}

	private static final Logger LOGGER = LoggerFactory.getLogger(CrossValidationTester.class);

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
	private WordClassDiscountingSmoother smoother;

	@Inject
	private EntityCrossValidationTester tester;

	@Inject
	private ClassInstanceFactory testInstsFactory;

	@Inject
	private CrossValidationTestSetFactory testSetFactory;

	public Result apply(final Iterable<Path> inpaths)
			throws TrainingException, ExecutionException, IOException, ClassificationException {
		final Map<Path, SessionDataManager> infileSessionData = SessionDataManager.createFileSessionDataMap(inpaths);
		final Map<SessionDataManager, Path> allSessions = infileSessionData.entrySet().stream()
				.collect(Collectors.toMap(Entry::getValue, Entry::getKey));
		infileSessionData.forEach((infile, sessionData) -> allSessions.put(sessionData, infile));

		LOGGER.info("Starting cross-validation test using data from {} session(s).", allSessions.size());
		final Stream<Entry<SessionDataManager, Map<String, Instances>>> testSets = testSetFactory.apply(allSessions);
		final Result result = new Result(allSessions.size());
		for (final Iterator<Entry<SessionDataManager, Map<String, Instances>>> testSetIter = testSets
				.iterator(); testSetIter.hasNext();) {
			final Entry<SessionDataManager, Map<String, Instances>> testSet = testSetIter.next();
			final SessionDataManager testSessionData = testSet.getKey();

			final Map<String, Instances> classInstances = testSet.getValue();
			final Instances oovInstances = smoother.redistributeMass(classInstances);
			LOGGER.debug("{} instances for out-of-vocabulary class.", oovInstances.size());

			final Map<String, Logistic> wordClassifiers = createWordClassifierMap(classInstances.entrySet());
			tester.setWordClassifiers(wordClassifiers::get);

			final Instances testInsts = testInstsFactory.apply(TEST_INSTS_REL_NAME,
					estimateTestInstanceCount(testSessionData));
			tester.setTestInsts(testInsts);
			final SessionTestResults testResults = tester.testSession(testSessionData);
			final Path infilePath = allSessions.get(testSessionData);
			result.sessionResults.put(infilePath, testResults);
		}
		LOGGER.info("Finished testing {} cross-validation dataset(s).", result.sessionResults.size());
		return result;
	}

}
