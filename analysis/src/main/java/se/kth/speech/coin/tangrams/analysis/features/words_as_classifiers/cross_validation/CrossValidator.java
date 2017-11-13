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
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.OptionalInt;
import java.util.concurrent.Executor;
import java.util.function.Function;
import java.util.stream.Stream;

import javax.inject.Inject;

import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.BeanFactory;

import it.unimi.dsi.fastutil.objects.Object2IntMap;
import se.kth.speech.coin.tangrams.analysis.GameContext;
import se.kth.speech.coin.tangrams.analysis.GameHistory;
import se.kth.speech.coin.tangrams.analysis.SessionGame;
import se.kth.speech.coin.tangrams.analysis.SessionGameManager;
import se.kth.speech.coin.tangrams.analysis.SessionGameManagerCacheSupplier;
import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.coin.tangrams.analysis.features.ClassificationException;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeature;
import se.kth.speech.coin.tangrams.analysis.features.weka.EntityInstanceAttributeContext;
import se.kth.speech.coin.tangrams.analysis.features.weka.WordClassInstancesFactory;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.EventDialogueTestResults;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.ReferentConfidenceData;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.ReferentConfidenceMapFactory;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.SessionTestResults;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.UtteranceGameContexts;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.WordClassDiscountingSmoother;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues.ClassificationContext;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues.EventDialogueClassifier;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues.EventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.WordClassificationData;
import se.kth.speech.coin.tangrams.analysis.io.SessionDataManager;
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
public final class CrossValidator
		implements Function<Map<SessionDataManager, Path>, List<CrossValidator.IterationResult>> {

	public static final class CrossValidationTestSummary {

		private final LocalDateTime sessionStartTime;

		private final SessionTestResults testResults;

		private final Object2IntMap<String> trainingInstanceCounts;

		private CrossValidationTestSummary(final SessionTestResults testResults,
				final Object2IntMap<String> trainingInstanceCounts, final LocalDateTime sessionStartTime) {
			this.sessionStartTime = sessionStartTime;
			this.testResults = testResults;
			this.trainingInstanceCounts = trainingInstanceCounts;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.lang.Object#equals(java.lang.Object)
		 */
		@Override
		public boolean equals(final Object obj) {
			if (this == obj) {
				return true;
			}
			if (obj == null) {
				return false;
			}
			if (!(obj instanceof CrossValidationTestSummary)) {
				return false;
			}
			final CrossValidationTestSummary other = (CrossValidationTestSummary) obj;
			if (sessionStartTime == null) {
				if (other.sessionStartTime != null) {
					return false;
				}
			} else if (!sessionStartTime.equals(other.sessionStartTime)) {
				return false;
			}
			if (testResults == null) {
				if (other.testResults != null) {
					return false;
				}
			} else if (!testResults.equals(other.testResults)) {
				return false;
			}
			if (trainingInstanceCounts == null) {
				if (other.trainingInstanceCounts != null) {
					return false;
				}
			} else if (!trainingInstanceCounts.equals(other.trainingInstanceCounts)) {
				return false;
			}
			return true;
		}

		/**
		 * @return the sessionStartTime
		 */
		public LocalDateTime getSessionStartTime() {
			return sessionStartTime;
		}

		/**
		 * @return the testResults
		 */
		public SessionTestResults getTestResults() {
			return testResults;
		}

		/**
		 * @return the trainingInstanceCounts
		 */
		public Object2IntMap<String> getTrainingInstanceCounts() {
			return trainingInstanceCounts;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.lang.Object#hashCode()
		 */
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + (sessionStartTime == null ? 0 : sessionStartTime.hashCode());
			result = prime * result + (testResults == null ? 0 : testResults.hashCode());
			result = prime * result + (trainingInstanceCounts == null ? 0 : trainingInstanceCounts.hashCode());
			return result;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.lang.Object#toString()
		 */
		@Override
		public String toString() {
			final StringBuilder builder = new StringBuilder(1024);
			builder.append("CrossValidationTestSummary [sessionStartTime=");
			builder.append(sessionStartTime);
			builder.append(", testResults=");
			builder.append(testResults);
			builder.append(", trainingInstanceCounts=");
			builder.append(trainingInstanceCounts);
			builder.append("]");
			return builder.toString();
		}
	}

	public static final class IterationResult {

		private final Stream<Entry<Path, CrossValidationTestSummary>> cvTestResults;

		private final int iterNo;

		private IterationResult(final int iterNo, final Stream<Entry<Path, CrossValidationTestSummary>> cvTestResults) {
			this.iterNo = iterNo;
			this.cvTestResults = cvTestResults;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.lang.Object#equals(java.lang.Object)
		 */
		@Override
		public boolean equals(final Object obj) {
			if (this == obj) {
				return true;
			}
			if (obj == null) {
				return false;
			}
			if (!(obj instanceof IterationResult)) {
				return false;
			}
			final IterationResult other = (IterationResult) obj;
			if (cvTestResults == null) {
				if (other.cvTestResults != null) {
					return false;
				}
			} else if (!cvTestResults.equals(other.cvTestResults)) {
				return false;
			}
			if (iterNo != other.iterNo) {
				return false;
			}
			return true;
		}

		/**
		 * @return the cvTestResults
		 */
		public Stream<Entry<Path, CrossValidationTestSummary>> getCvTestResults() {
			return cvTestResults;
		}

		/**
		 * @return the iterNo
		 */
		public int getIterNo() {
			return iterNo;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.lang.Object#hashCode()
		 */
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + (cvTestResults == null ? 0 : cvTestResults.hashCode());
			result = prime * result + iterNo;
			return result;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.lang.Object#toString()
		 */
		@Override
		public String toString() {
			final StringBuilder builder = new StringBuilder();
			builder.append("IterationResult [iterNo=");
			builder.append(iterNo);
			builder.append(", cvTestResults=");
			builder.append(cvTestResults);
			builder.append("]");
			return builder.toString();
		}

	}

	private static final Logger LOGGER = LoggerFactory.getLogger(CrossValidator.class);

	private static final String TEST_INSTS_REL_NAME = "tested_entites";

	private static int estimateTestInstanceCount(final SessionDataManager sessionData) throws IOException {
		final long lineCount = Files.lines(sessionData.getCanonicalEventLogPath()).count();
		// (Number of logged events / estimated number of events per dialogue) *
		// estimated number of entities per game *
		// estimated number of utterances per dialogue * estimated number of
		// tokens (i.e. n-grams) per utterance
		final long estimate = lineCount / 4 * 20 * 4 * 10;
		int result = Integer.MAX_VALUE;
		try {
			result = Math.toIntExact(estimate);
		} catch (final ArithmeticException e) {
			LOGGER.debug(String.format("Could not convert long value \"%d\" to an int; Returning max.", estimate), e);
		}
		return result;
	}

	private final Executor backgroundJobExecutor;

	@Inject
	private BeanFactory beanFactory;

	private final Function<? super ClassificationContext, ? extends EventDialogueClassifier> classifierFactory;

	private final EventDialogueTransformer diagTransformer;

	@Inject
	private EntityInstanceAttributeContext entInstAttrCtx;

	private int iterCount = 1;

	@Inject
	private SessionGameManagerCacheSupplier sessionDiagMgrCacheSupplier;

	private final WordClassDiscountingSmoother smoother;

	@Inject
	private WordClassInstancesFactory testInstsFactory;

	private final Function<Map<SessionDataManager, Path>, Stream<Entry<SessionDataManager, WordClassificationData>>> testSetFactory;

	public CrossValidator( // NO_UCD (unused code)
			final Function<Map<SessionDataManager, Path>, Stream<Entry<SessionDataManager, WordClassificationData>>> testSetFactory,
			final EventDialogueTransformer diagTransformer,
			final Function<? super ClassificationContext, ? extends EventDialogueClassifier> classifierFactory,
			final WordClassDiscountingSmoother smoother, final Executor backgroundJobExecutor) {
		this.testSetFactory = testSetFactory;
		this.diagTransformer = diagTransformer;
		this.classifierFactory = classifierFactory;
		this.smoother = smoother;
		this.backgroundJobExecutor = backgroundJobExecutor;
	}

	@Override
	public List<IterationResult> apply(final Map<SessionDataManager, Path> allSessions) {
		final List<IterationResult> result = new ArrayList<>(iterCount);
		LOGGER.info(
				"Starting cross-validation test using data from {} session(s), doing {} iteration(s) on each dataset.",
				allSessions.size(), iterCount);

		for (int iterNo = 1; iterNo <= iterCount; ++iterNo) {
			LOGGER.info("Training/testing iteration no. {}.", iterNo);
			final Stream<Entry<Path, CrossValidationTestSummary>> iterResults = crossValidate(allSessions);
			result.add(new IterationResult(iterNo, iterResults));
		}
		LOGGER.info("Finished testing {} cross-validation dataset(s).", result.size());
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

	private EventDialogueClassifier createDialogueClassifier(final WordClassificationData trainingData,
			final SessionDataManager testSessionData) throws IOException {
		final Instances testInsts = testInstsFactory.apply(TEST_INSTS_REL_NAME,
				estimateTestInstanceCount(testSessionData));
		final Function<EntityFeature.Extractor.Context, Instance> testInstFactory = entInstAttrCtx
				.createInstFactory(testInsts);
		final ReferentConfidenceMapFactory referentConfidenceMapFactory = beanFactory
				.getBean(ReferentConfidenceMapFactory.class, testInstFactory, smoother);
		return classifierFactory
				.apply(new ClassificationContext(trainingData, backgroundJobExecutor, referentConfidenceMapFactory));
	}

	private Stream<Entry<Path, CrossValidationTestSummary>> crossValidate(
			final Map<SessionDataManager, Path> allSessions) {
		final Stream<Entry<SessionDataManager, WordClassificationData>> testSets = testSetFactory.apply(allSessions);
		return testSets.map(testSet -> {
			final SessionDataManager testSessionData = testSet.getKey();
			final Path infilePath = allSessions.get(testSessionData);
			LOGGER.info("Running cross-validation test on data from \"{}\".", infilePath);

			final WordClassificationData trainingData = testSet.getValue();
			try {
				final EventDialogueClassifier diagClassifier = createDialogueClassifier(trainingData, testSessionData);
				final SessionGameManager sessionGameMgr = sessionDiagMgrCacheSupplier.get()
						.getUnchecked(testSessionData);
				final SessionGame sessionGame = sessionGameMgr.getCanonicalGame();
				final SessionTestResults testResults = testSession(sessionGame, diagClassifier);
				final CrossValidationTestSummary cvTestSummary = new CrossValidationTestSummary(testResults,
						trainingData.getTrainingInstanceCounts(), sessionGame.getHistory().getStartTime());
				return Pair.of(infilePath, cvTestSummary);

			} catch (final IOException e) {
				throw new UncheckedIOException(e);
			}
		});
	}

	private Optional<EventDialogueTestResults> testDialogue(final EventDialogue uttDiag, final GameHistory history,
			final EventDialogueClassifier diagClassifier) throws ClassificationException {
		final Optional<EventDialogueTestResults> result;
		// TODO: Extract this from the method so that dialogue testing can be
		// parallelized
		final EventDialogue transformedDiag = diagTransformer.apply(uttDiag);

		final List<Utterance> allUtts = transformedDiag.getUtterances();
		if (allUtts.isEmpty()) {
			result = Optional.empty();
		} else {
			// Just use the game context for the first utterance for all
			// utterances processed for the given dialogue
			final Utterance firstUtt = allUtts.get(0);
			final GameContext uttCtx = UtteranceGameContexts.createSingleContext(firstUtt, history);
			final OptionalInt optLastSelectedEntityId = uttCtx.findLastSelectedEntityId();
			if (optLastSelectedEntityId.isPresent()) {
				final Optional<ReferentConfidenceData> optReferentConfidenceData = diagClassifier.apply(transformedDiag,
						uttCtx);
				if (optReferentConfidenceData.isPresent()) {
					final ReferentConfidenceData referentConfidenceData = optReferentConfidenceData.get();
					final int goldStandardEntityId = optLastSelectedEntityId.getAsInt();
					result = Optional.of(new EventDialogueTestResults(referentConfidenceData, goldStandardEntityId,
							transformedDiag, uttDiag.getUtterances().size()));

				} else {
					result = Optional.empty();
				}
			} else {
				result = Optional.empty();
			}
		}

		return result;
	}

	private SessionTestResults testSession(final SessionGame canonicalGame,
			final EventDialogueClassifier diagClassifier) throws ClassificationException {
		final List<EventDialogue> uttDiags = canonicalGame.getEventDialogues();
		final SessionTestResults result = new SessionTestResults(uttDiags.size());

		LOGGER.info("Testing {} individual dialogue(s).", uttDiags.size());
		for (final EventDialogue uttDiag : uttDiags) {
			final Optional<EventDialogueTestResults> optTestResults = testDialogue(uttDiag, canonicalGame.getHistory(),
					diagClassifier);
			if (optTestResults.isPresent()) {
				final EventDialogueTestResults results = optTestResults.get();
				result.add(Pair.of(uttDiag, results));
			} else {
				LOGGER.debug("No utterances tested for {}.", uttDiag);
			}
		}
		return result;
	}

}
