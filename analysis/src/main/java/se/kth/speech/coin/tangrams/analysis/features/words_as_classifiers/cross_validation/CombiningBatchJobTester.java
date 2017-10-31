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
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;

import edu.stanford.nlp.trees.Tree;
import edu.stanford.nlp.util.CoreMap;
import se.kth.speech.coin.tangrams.analysis.SessionGameManagerCacheSupplier;
import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.features.ClassificationException;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues.CachingEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues.EventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues.UtteranceRelation;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.TrainingInstancesFactory;
import se.kth.speech.coin.tangrams.analysis.io.SessionDataManager;
import se.kth.speech.nlp.stanford.AnnotationCacheFactory;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 24 May 2017
 *
 */
final class CombiningBatchJobTester {

	static final class IncompleteResults {

		private final TestParameters testParams;

		private final LocalDateTime testStartTime;

		private IncompleteResults(final TestParameters testParams, final LocalDateTime testStartTime) {
			this.testParams = testParams;
			this.testStartTime = testStartTime;
		}

		/**
		 * @return the testParams
		 */
		public TestParameters getTestParams() {
			return testParams;
		}

		/**
		 * @return the testStartTime
		 */
		public LocalDateTime getTestStartTime() {
			return testStartTime;
		}
	}

	static final class Input {

		private final Map<SessionDataManager, Path> allSessions;

		private final Iterable<Set<Cleaning>> cleaningMethods;

		private final Iterable<TokenFiltering> tokenFilteringMethods;

		private final Iterable<Tokenization> tokenizationMethods;

		private final Iterable<TokenType> tokenTypes;

		private final Iterable<Training> trainingMethods;

		Input(final Iterable<Set<Cleaning>> cleaningMethods, final Iterable<Tokenization> tokenizationMethods,
				final Iterable<TokenType> tokenTypes, final Iterable<TokenFiltering> tokenFilteringMethods,
				final Iterable<Training> trainingMethods, final Map<SessionDataManager, Path> allSessions) {
			this.cleaningMethods = cleaningMethods;
			this.tokenizationMethods = tokenizationMethods;
			this.tokenTypes = tokenTypes;
			this.tokenFilteringMethods = tokenFilteringMethods;
			this.trainingMethods = trainingMethods;
			this.allSessions = Collections.unmodifiableMap(allSessions);
		}

		/**
		 * @return the allSessions
		 */
		Map<SessionDataManager, Path> getAllSessions() {
			return allSessions;
		}

		/**
		 * @return the cleaningMethods
		 */
		Iterable<Set<Cleaning>> getCleaningMethods() {
			return cleaningMethods;
		}

		/**
		 * @return the tokenFilteringMethods
		 */
		Iterable<TokenFiltering> getTokenFilteringMethods() {
			return tokenFilteringMethods;
		}

		/**
		 * @return the tokenizationMethods
		 */
		Iterable<Tokenization> getTokenizationMethods() {
			return tokenizationMethods;
		}

		/**
		 * @return the tokenTypes
		 */
		Iterable<TokenType> getTokenTypes() {
			return tokenTypes;
		}

		/**
		 * @return the trainingMethods
		 */
		Iterable<Training> getTrainingMethods() {
			return trainingMethods;
		}
	}

	/**
	 * Parsing can take up huge amounts of memory, so it's single-threaded and
	 * thus the caches are created designed for single-threaded operation.
	 */
	private static final AnnotationCacheFactory ANNOTATION_CACHE_FACTORY = new AnnotationCacheFactory(1);

	private static final Logger LOGGER = LoggerFactory.getLogger(CombiningBatchJobTester.class);

	private final ApplicationContext appCtx;

	private final ExecutorService backgroundJobExecutor;

	private final Consumer<? super BatchJobSummary> batchJobResultHandler;

	private final BiConsumer<? super IncompleteResults, ? super Throwable> errorHandler;

	private final BiConsumer<? super CoreMap, ? super List<Tree>> extractionResultsHook;

	private final Consumer<? super CrossValidator> testerConfigurator;

	private final Map<WordClassifierTrainingParameter, Object> trainingParams;

	private final BiConsumer<? super EventDialogue, ? super List<UtteranceRelation>> uttRelHandler;

	CombiningBatchJobTester(final ExecutorService backgroundJobExecutor, final ApplicationContext appCtx,
			final Consumer<? super BatchJobSummary> batchJobResultHandler,
			final BiConsumer<? super IncompleteResults, ? super Throwable> errorHandler,
			final Consumer<? super CrossValidator> testerConfigurator,
			final BiConsumer<? super CoreMap, ? super List<Tree>> extractionResultsHook,
			final BiConsumer<? super EventDialogue, ? super List<UtteranceRelation>> uttRelHandler,
			final Map<WordClassifierTrainingParameter, Object> trainingParams) {
		this.backgroundJobExecutor = backgroundJobExecutor;
		this.appCtx = appCtx;
		this.batchJobResultHandler = batchJobResultHandler;
		this.errorHandler = errorHandler;
		this.testerConfigurator = testerConfigurator;
		this.extractionResultsHook = extractionResultsHook;
		this.uttRelHandler = uttRelHandler;
		this.trainingParams = trainingParams;
	}

	void accept(final Input input) throws ClassificationException, IOException {
		LOGGER.debug("Bean names: {}", Arrays.toString(appCtx.getBeanDefinitionNames()));
		final SessionGameManagerCacheSupplier sessionDiagMgrCacheSupplier = appCtx
				.getBean(SessionGameManagerCacheSupplier.class);

		for (final Set<Cleaning> cleaningMethodSet : input.cleaningMethods) {
			for (final Training trainingMethod : input.trainingMethods) {
				for (final Tokenization tokenizationMethod : input.tokenizationMethods) {
					for (final TokenType tokenType : input.tokenTypes) {
						final TokenizationContext tokenizationContext = new TokenizationContext(cleaningMethodSet,
								tokenType, extractionResultsHook, ANNOTATION_CACHE_FACTORY);
						final EventDialogueTransformer tokenizer = tokenizationMethod.apply(tokenizationContext);

						for (final TokenFiltering tokenFilteringMethod : input.tokenFilteringMethods) {
							final EventDialogueTransformer tokenFilter = tokenFilteringMethod.get();

							final CachingEventDialogueTransformer symmetricalDiagTransformer = trainingMethod
									.createSymmetricalTrainingTestingEvgDiagTransformer(
											Arrays.asList(tokenizer, tokenFilter));
							final TrainingContext trainingCtx = new TrainingContext(symmetricalDiagTransformer, appCtx,
									uttRelHandler, trainingParams);
							final TrainingInstancesFactory trainingInstsFactory = trainingMethod
									.createTrainingInstsFactory(trainingCtx);
							final TestSetFactory testSetFactory = new TestSetFactory(trainingInstsFactory,
									sessionDiagMgrCacheSupplier, (Integer) trainingParams
											.get(WordClassifierTrainingParameter.TRAINING_SET_SIZE_DISCOUNTING_CONSTANT));
							final CrossValidator crossValidator = appCtx.getBean(CrossValidator.class, testSetFactory,
									symmetricalDiagTransformer, trainingMethod.getClassifierFactory(trainingCtx),
									backgroundJobExecutor);
							crossValidator.setIterCount(trainingMethod.getIterCount());
							testerConfigurator.accept(crossValidator);
							final TestParameters testParams = new TestParameters(cleaningMethodSet, tokenizationMethod,
									tokenType, tokenFilteringMethod, trainingMethod, trainingParams);
							LOGGER.info("Testing {}.", testParams);

							final LocalDateTime testTimestamp = LocalDateTime.now();
							try {
								final CrossValidator.Result testResults = crossValidator.apply(input.allSessions);
								final BatchJobSummary batchSummary = new BatchJobSummary(testTimestamp, testParams,
										testResults);
								batchJobResultHandler.accept(batchSummary);
							} catch (final Throwable thrown) {
								errorHandler.accept(new IncompleteResults(testParams, testTimestamp), thrown);
							}
						}
					}
				}
			}
		}
	}

}
