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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.cross_validation;

import java.nio.file.Path;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.Executor;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;

import se.kth.speech.coin.tangrams.analysis.SessionGameManager;
import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeatureExtractionContextFactory;
import se.kth.speech.coin.tangrams.analysis.features.weka.EntityInstanceAttributeContext;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.WordClassDiscountingSmoother;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues.EventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues.UtteranceRelation;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.TrainingInstancesFactory;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.WordClassificationData;
import se.kth.speech.coin.tangrams.analysis.io.SessionDataManager;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 13 Nov 2017
 *
 */
final class UtteranceMappingBatchJobTester implements Consumer<UtteranceMappingBatchJobTester.Input> {

	public static class Input {

		private final Map<SessionDataManager, Path> allSessionData;

		private final EventDialogueTransformer diagTransformer;

		private final Supplier<? extends Map<SessionDataManager, SessionGameManager>> sessionGameMgrMapSupplier;

		private final Training trainingMethod;

		public Input(final Map<SessionDataManager, Path> allSessionData,
				final Supplier<? extends Map<SessionDataManager, SessionGameManager>> sessionGameMgrMapSupplier,
				final Training trainingMethod, final EventDialogueTransformer diagTransformer) {
			this.allSessionData = allSessionData;
			this.sessionGameMgrMapSupplier = sessionGameMgrMapSupplier;
			this.trainingMethod = trainingMethod;
			this.diagTransformer = diagTransformer;
		}

		/**
		 * @return the allSessionData
		 */
		public Map<SessionDataManager, Path> getAllSessionData() {
			return allSessionData;
		}

		/**
		 * @return the diagTransformer
		 */
		public EventDialogueTransformer getDiagTransformer() {
			return diagTransformer;
		}

		/**
		 * @return the sessionGameMgrMapSupplier
		 */
		public Supplier<? extends Map<SessionDataManager, SessionGameManager>> getSessionGameMgrMapSupplier() {
			return sessionGameMgrMapSupplier;
		}

		/**
		 * @return the trainingMethod
		 */
		public Training getTrainingMethod() {
			return trainingMethod;
		}
	}

	final static class BatchJobSummary {

		private final TestParameters testParams;

		private final List<CrossValidator.IterationResult> testResults;

		private final LocalDateTime testTimestamp;

		BatchJobSummary(final LocalDateTime testTimestamp, final TestParameters testParams,
				final List<CrossValidator.IterationResult> testResults) {
			this.testTimestamp = testTimestamp;
			this.testParams = testParams;
			this.testResults = testResults;
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
			if (!(obj instanceof BatchJobSummary)) {
				return false;
			}
			final BatchJobSummary other = (BatchJobSummary) obj;
			if (testParams == null) {
				if (other.testParams != null) {
					return false;
				}
			} else if (!testParams.equals(other.testParams)) {
				return false;
			}
			if (testResults == null) {
				if (other.testResults != null) {
					return false;
				}
			} else if (!testResults.equals(other.testResults)) {
				return false;
			}
			if (testTimestamp == null) {
				if (other.testTimestamp != null) {
					return false;
				}
			} else if (!testTimestamp.equals(other.testTimestamp)) {
				return false;
			}
			return true;
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
			result = prime * result + (testParams == null ? 0 : testParams.hashCode());
			result = prime * result + (testResults == null ? 0 : testResults.hashCode());
			result = prime * result + (testTimestamp == null ? 0 : testTimestamp.hashCode());
			return result;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.lang.Object#toString()
		 */
		@Override
		public String toString() {
			final StringBuilder builder = new StringBuilder(128);
			builder.append("BatchJobSummary [testParams=");
			builder.append(testParams);
			builder.append(", testResults=");
			builder.append(testResults);
			builder.append(", testTimestamp=");
			builder.append(testTimestamp);
			builder.append(']');
			return builder.toString();
		}

		/**
		 * @return the testParams
		 */
		TestParameters getTestParams() {
			return testParams;
		}

		/**
		 * @return the testResults
		 */
		List<CrossValidator.IterationResult> getTestResults() {
			return testResults;
		}

		/**
		 * @return the testTimestamp
		 */
		LocalDateTime getTestTimestamp() {
			return testTimestamp;
		}
	}

	static final class IncompleteResults {

		private final TestParameters testParams;

		private final LocalDateTime testStartTime;

		private IncompleteResults(final TestParameters testParams, final LocalDateTime testStartTime) {
			this.testParams = testParams;
			this.testStartTime = testStartTime;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.lang.Object#toString()
		 */
		@Override
		public String toString() {
			final StringBuilder builder = new StringBuilder(512);
			builder.append("IncompleteResults [testParams=");
			builder.append(testParams);
			builder.append(", testStartTime=");
			builder.append(testStartTime);
			builder.append("]");
			return builder.toString();
		}

		/**
		 * @return the testParams
		 */
		TestParameters getTestParams() {
			return testParams;
		}

		/**
		 * @return the testStartTime
		 */
		LocalDateTime getTestStartTime() {
			return testStartTime;
		}
	}

	static final class TestParameters {

		private final Training trainingMethod;

		private final Map<WordClassifierTrainingParameter, Object> trainingParams;

		TestParameters(final Training trainingMethod,
				final Map<WordClassifierTrainingParameter, Object> trainingParams) {
			this.trainingMethod = trainingMethod;
			this.trainingParams = trainingParams;
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
			if (!(obj instanceof TestParameters)) {
				return false;
			}
			final TestParameters other = (TestParameters) obj;
			if (trainingMethod != other.trainingMethod) {
				return false;
			}
			if (trainingParams == null) {
				if (other.trainingParams != null) {
					return false;
				}
			} else if (!trainingParams.equals(other.trainingParams)) {
				return false;
			}
			return true;
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
			result = prime * result + (trainingMethod == null ? 0 : trainingMethod.hashCode());
			result = prime * result + (trainingParams == null ? 0 : trainingParams.hashCode());
			return result;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.lang.Object#toString()
		 */
		@Override
		public String toString() {
			final StringBuilder builder = new StringBuilder(512);
			builder.append("TestParameters [trainingMethod=");
			builder.append(trainingMethod);
			builder.append(", trainingParams=");
			builder.append(trainingParams);
			builder.append("]");
			return builder.toString();
		}

		/**
		 * @return the trainingMethod
		 */
		Training getTrainingMethod() {
			return trainingMethod;
		}

		/**
		 * @return the trainingParams
		 */
		Map<WordClassifierTrainingParameter, Object> getTrainingParams() {
			return trainingParams;
		}
	}

	private static final Logger LOGGER = LoggerFactory.getLogger(UtteranceMappingBatchJobTester.class);

	private final ApplicationContext appCtx;

	private final Executor backgroundJobExecutor;

	private final Consumer<? super BatchJobSummary> batchJobResultHandler;

	private final BiConsumer<? super IncompleteResults, ? super Throwable> errorHandler;

	private final TestSetFactoryFactory testSetFactoryFactory;

	private final Map<WordClassifierTrainingParameter, Object> trainingParams;

	private final BiConsumer<? super EventDialogue, ? super List<UtteranceRelation>> uttRelHandler;

	private final int crossValidationParallelismLevel;

	UtteranceMappingBatchJobTester(final Executor backgroundJobExecutor, final ApplicationContext appCtx,
			final Consumer<? super BatchJobSummary> batchJobResultHandler,
			final BiConsumer<? super IncompleteResults, ? super Throwable> errorHandler,
			final BiConsumer<? super EventDialogue, ? super List<UtteranceRelation>> uttRelHandler,
			final Map<WordClassifierTrainingParameter, Object> trainingParams,
			final TestSetFactoryFactory testSetFactoryFactory, final int crossValidationParallelismLevel) {
		this.backgroundJobExecutor = backgroundJobExecutor;
		this.appCtx = appCtx;
		this.batchJobResultHandler = batchJobResultHandler;
		this.errorHandler = errorHandler;
		this.uttRelHandler = uttRelHandler;
		this.trainingParams = trainingParams;
		this.testSetFactoryFactory = testSetFactoryFactory;
		this.crossValidationParallelismLevel = crossValidationParallelismLevel;
	}

	@Override
	public void accept(final Input input) {
		final Training trainingMethod = input.getTrainingMethod();
		final EventDialogueTransformer diagTransformer = trainingMethod
				.createSymmetricalTrainingTestingEventDiagTransformer(input.getDiagTransformer());
		final EntityInstanceAttributeContext entityInstAttrCtx = appCtx.getBean(EntityInstanceAttributeContext.class);
		final EntityFeatureExtractionContextFactory extCtxFactory = appCtx
				.getBean(EntityFeatureExtractionContextFactory.class);
		final Integer smoothingMinCount = (Integer) trainingParams
				.get(WordClassifierTrainingParameter.SMOOTHING_MIN_COUNT);
		final WordClassDiscountingSmoother smoother = appCtx.getBean(WordClassDiscountingSmoother.class,
				smoothingMinCount, crossValidationParallelismLevel);
		final TrainingContext trainingCtx = new TrainingContext(diagTransformer, smoother, entityInstAttrCtx,
				extCtxFactory, uttRelHandler, trainingParams);
		final TrainingInstancesFactory trainingInstsFactory = trainingMethod.createTrainingInstsFactory(trainingCtx);
		final Map<SessionDataManager, SessionGameManager> sessionGameMgrs = input.getSessionGameMgrMapSupplier().get();
		final Function<Map<SessionDataManager, Path>, Stream<Entry<SessionDataManager, WordClassificationData>>> testSetFactory = testSetFactoryFactory
				.apply(trainingInstsFactory, sessionGameMgrs);
		final CrossValidator crossValidator = appCtx.getBean(CrossValidator.class, sessionGameMgrs, testSetFactory,
				trainingMethod.getClassifierFactory(trainingCtx), smoother, backgroundJobExecutor, crossValidationParallelismLevel);
		crossValidator.setIterCount(trainingMethod.getIterCount());
		final TestParameters testParams = new TestParameters(trainingMethod, trainingParams);
		LOGGER.info("Testing {}.", testParams);
		final LocalDateTime testTimestamp = LocalDateTime.now();
		try {
			final List<CrossValidator.IterationResult> testResults = crossValidator.apply(input.getAllSessionData());
			final BatchJobSummary batchSummary = new BatchJobSummary(testTimestamp, testParams, testResults);
			batchJobResultHandler.accept(batchSummary);
		} catch (final Throwable thrown) {
			errorHandler.accept(new IncompleteResults(testParams, testTimestamp), thrown);
		}
	}

}
