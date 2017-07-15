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
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;

import edu.stanford.nlp.trees.Tree;
import edu.stanford.nlp.util.CoreMap;
import se.kth.speech.coin.tangrams.analysis.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.SessionDataManager;
import se.kth.speech.coin.tangrams.analysis.SessionEventDialogueManagerCacheSupplier;
import se.kth.speech.coin.tangrams.analysis.features.ClassificationException;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.CachingEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.EventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.UtteranceRelation;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.TrainingInstancesFactory;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 24 May 2017
 *
 */
public final class CombiningBatchJobTester {

	public static final class IncompleteResults {

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

	public static final class Input {

		private final Map<SessionDataManager, Path> allSessions;

		private final Iterable<Set<Cleaning>> cleaningMethods;

		private final Iterable<TokenFiltering> tokenFilteringMethods;

		private final Iterable<Tokenization> tokenizationMethods;

		private final Iterable<TokenType> tokenTypes;

		private final Iterable<Training> trainingMethods;

		public Input(final Iterable<Set<Cleaning>> cleaningMethods, final Iterable<Tokenization> tokenizationMethods,
				final Iterable<TokenType> tokenTypes, final Iterable<TokenFiltering> tokenFilteringMethods,
				final Iterable<Training> trainingMethods, final Map<SessionDataManager, Path> allSessions) {
			this.cleaningMethods = cleaningMethods;
			this.tokenizationMethods = tokenizationMethods;
			this.tokenTypes = tokenTypes;
			this.tokenFilteringMethods = tokenFilteringMethods;
			this.trainingMethods = trainingMethods;
			this.allSessions = allSessions;
		}
	}

	private static final Logger LOGGER = LoggerFactory.getLogger(CombiningBatchJobTester.class);

	private final ApplicationContext appCtx;

	private final ExecutorService backgroundJobExecutor;

	private final Consumer<? super BatchJobSummary> batchJobResultHandler;

	private final BiConsumer<? super IncompleteResults, ? super Throwable> errorHandler;

	private final BiConsumer<? super CoreMap, ? super List<Tree>> extractionResultsHook;

	private final Consumer<? super Tester> testerConfigurator;

	private final BiConsumer<? super EventDialogue, ? super List<UtteranceRelation>> uttRelHandler;

	public CombiningBatchJobTester(final ExecutorService backgroundJobExecutor, final ApplicationContext appCtx,
			final Consumer<? super BatchJobSummary> batchJobResultHandler,
			final BiConsumer<? super IncompleteResults, ? super Throwable> errorHandler,
			final Consumer<? super Tester> testerConfigurator,
			final BiConsumer<? super CoreMap, ? super List<Tree>> extractionResultsHook,
			final BiConsumer<? super EventDialogue, ? super List<UtteranceRelation>> uttRelHandler) {
		this.backgroundJobExecutor = backgroundJobExecutor;
		this.appCtx = appCtx;
		this.batchJobResultHandler = batchJobResultHandler;
		this.errorHandler = errorHandler;
		this.testerConfigurator = testerConfigurator;
		this.extractionResultsHook = extractionResultsHook;
		this.uttRelHandler = uttRelHandler;
	}

	public void accept(final Input input) throws ClassificationException, ExecutionException, IOException {
		LOGGER.debug("Bean names: {}", Arrays.toString(appCtx.getBeanDefinitionNames()));
		final SessionEventDialogueManagerCacheSupplier sessionDiagMgrCacheSupplier = appCtx
				.getBean(SessionEventDialogueManagerCacheSupplier.class);

		for (final Set<Cleaning> cleaningMethodSet : input.cleaningMethods) {
			for (final Training trainingMethod : input.trainingMethods) {
				for (final Tokenization tokenizationMethod : input.tokenizationMethods) {
					for (final TokenType tokenType : input.tokenTypes) {
						final TokenizationContext tokenizationContext = new TokenizationContext(cleaningMethodSet,
								tokenType, backgroundJobExecutor, extractionResultsHook);
						final EventDialogueTransformer tokenizer = tokenizationMethod.apply(tokenizationContext);

						for (final TokenFiltering tokenFilteringMethod : input.tokenFilteringMethods) {
							final EventDialogueTransformer tokenFilter = tokenFilteringMethod.get();

							final CachingEventDialogueTransformer symmetricalDiagTransformer = trainingMethod
									.createSymmetricalTrainingTestingEvgDiagTransformer(
											Arrays.asList(tokenizer, tokenFilter));
							final TrainingContext trainingCtx = new TrainingContext(symmetricalDiagTransformer, appCtx,
									backgroundJobExecutor, uttRelHandler);
							final TrainingInstancesFactory trainingInstsFactory = trainingMethod
									.createTrainingInstsFactoryIterCount(trainingCtx);
							final TestSetFactory testSetFactory = new TestSetFactory(trainingInstsFactory,
									sessionDiagMgrCacheSupplier);
							final Tester tester = appCtx.getBean(Tester.class, testSetFactory,
									symmetricalDiagTransformer, trainingMethod.getClassifierFactory(),
									backgroundJobExecutor);
							tester.setIterCount(trainingMethod.getIterCount());
							testerConfigurator.accept(tester);
							final TestParameters testParams = new TestParameters(cleaningMethodSet, tokenizationMethod,
									tokenType, tokenFilteringMethod, trainingMethod);
							LOGGER.info("Testing {}.", testParams);

							final LocalDateTime testTimestamp = LocalDateTime.now();
							try {
								final Tester.Result testResults = tester.apply(input.allSessions);
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
