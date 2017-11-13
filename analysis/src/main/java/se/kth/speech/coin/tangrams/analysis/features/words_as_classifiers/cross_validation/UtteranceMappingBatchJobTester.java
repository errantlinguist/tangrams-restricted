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

import java.io.IOException;
import java.nio.file.Path;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;

import com.google.common.cache.LoadingCache;

import edu.stanford.nlp.trees.Tree;
import edu.stanford.nlp.util.CoreMap;
import se.kth.speech.coin.tangrams.analysis.SessionGameManager;
import se.kth.speech.coin.tangrams.analysis.SessionGameManagerCacheSupplier;
import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.features.ClassificationException;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues.CachingEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues.EventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues.UtteranceRelation;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.TrainingInstancesFactory;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.WordClassificationData;
import se.kth.speech.coin.tangrams.analysis.io.SessionDataManager;
import se.kth.speech.coin.tangrams.analysis.tokenization.Cleaning;
import se.kth.speech.coin.tangrams.analysis.tokenization.TokenFiltering;
import se.kth.speech.coin.tangrams.analysis.tokenization.TokenType;
import se.kth.speech.coin.tangrams.analysis.tokenization.Tokenization;
import se.kth.speech.nlp.stanford.AnnotationCacheFactory;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 13 Nov 2017
 *
 */
final class UtteranceMappingBatchJobTester implements Consumer<UtteranceMappingBatchJobTester.Input> {

	static final class IncompleteResults {


		private final LocalDateTime testStartTime;

		private IncompleteResults(final LocalDateTime testStartTime) {
			this.testStartTime = testStartTime;
		}

		/**
		 * @return the testStartTime
		 */
		public LocalDateTime getTestStartTime() {
			return testStartTime;
		}
	}

	public static class Input {

		private final Training trainingMethod;

		/**
		 * @return the trainingMethod
		 */
		public Training getTrainingMethod() {
			return trainingMethod;
		}

		/**
		 * @return the allSessionData
		 */
		public Map<SessionDataManager, Path> getAllSessionData() {
			return allSessionData;
		}

		private final Map<SessionDataManager, Path> allSessionData;

		public Input(final Map<SessionDataManager, Path> allSessionData, final Training trainingMethod) {
			this.allSessionData = allSessionData;
			this.trainingMethod = trainingMethod;
		}
	}
	
	public static final class BatchJobSummary {

		private BatchJobSummary() {

		}
	}


	private static final Logger LOGGER = LoggerFactory.getLogger(UtteranceMappingBatchJobTester.class);

	private final ApplicationContext appCtx;

	private final ExecutorService backgroundJobExecutor;

	private final Consumer<? super BatchJobSummary> batchJobResultHandler;

	private final BiConsumer<? super IncompleteResults, ? super Throwable> errorHandler;

	private final Consumer<? super CrossValidator> testerConfigurator;

	private final TestSetFactoryFactory testSetFactoryFactory;

	private final Map<WordClassifierTrainingParameter, Object> trainingParams;

	private final BiConsumer<? super EventDialogue, ? super List<UtteranceRelation>> uttRelHandler;

	UtteranceMappingBatchJobTester(final ExecutorService backgroundJobExecutor, final ApplicationContext appCtx,
			final Consumer<? super BatchJobSummary> batchJobResultHandler,
			final BiConsumer<? super IncompleteResults, ? super Throwable> errorHandler,
			final Consumer<? super CrossValidator> testerConfigurator,
			final BiConsumer<? super EventDialogue, ? super List<UtteranceRelation>> uttRelHandler,
			final Map<WordClassifierTrainingParameter, Object> trainingParams,
			final TestSetFactoryFactory testSetFactoryFactory) {
		this.backgroundJobExecutor = backgroundJobExecutor;
		this.appCtx = appCtx;
		this.batchJobResultHandler = batchJobResultHandler;
		this.errorHandler = errorHandler;
		this.testerConfigurator = testerConfigurator;
		this.uttRelHandler = uttRelHandler;
		this.trainingParams = trainingParams;
		this.testSetFactoryFactory = testSetFactoryFactory;
	}

	@Override
	public void accept(final Input input) {
		final SessionGameManagerCacheSupplier sessionDiagMgrCacheSupplier = appCtx
				.getBean(SessionGameManagerCacheSupplier.class);
		final LoadingCache<SessionDataManager, SessionGameManager> sessionGameMgrs = sessionDiagMgrCacheSupplier.get();

		final Training trainingMethod = input.getTrainingMethod();
//		final CachingEventDialogueTransformer symmetricalDiagTransformer = trainingMethod
//				.createSymmetricalTrainingTestingEventDiagTransformer(Arrays.asList(tokenizer, tokenFilter));
//		final TrainingContext trainingCtx = new TrainingContext(symmetricalDiagTransformer, appCtx, uttRelHandler,
//				trainingParams);
//		final TrainingInstancesFactory trainingInstsFactory = trainingMethod.createTrainingInstsFactory(trainingCtx);
//		final Function<Map<SessionDataManager, Path>, Stream<Entry<SessionDataManager, WordClassificationData>>> testSetFactory = testSetFactoryFactory
//				.apply(trainingInstsFactory, sessionGameMgrs);
//		final CrossValidator crossValidator = appCtx.getBean(CrossValidator.class, testSetFactory,
//				symmetricalDiagTransformer, trainingMethod.getClassifierFactory(trainingCtx), backgroundJobExecutor);
//		crossValidator.setIterCount(trainingMethod.getIterCount());
//		testerConfigurator.accept(crossValidator);
//		final TestParameters testParams = new TestParameters(cleaningMethodSet, tokenizationMethod, tokenType,
//				tokenFilteringMethod, trainingMethod, trainingParams);
//		LOGGER.info("Testing {}.", testParams);
//
//		final LocalDateTime testTimestamp = LocalDateTime.now();
//		try {
//			final List<CrossValidator.IterationResult> testResults = crossValidator.apply(input.getAllSessionData());
//			final BatchJobSummary batchSummary = new BatchJobSummary(testTimestamp, testParams, testResults);
//			batchJobResultHandler.accept(batchSummary);
//		} catch (final Throwable thrown) {
//			errorHandler.accept(new IncompleteResults(testParams, testTimestamp), thrown);
//		}
		// TODO: Finish
	}

}
