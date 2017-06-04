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
import java.util.EnumSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;

import se.kth.speech.coin.tangrams.analysis.SessionDataManager;
import se.kth.speech.coin.tangrams.analysis.SessionEventDialogueManagerCacheSupplier;
import se.kth.speech.coin.tangrams.analysis.features.ClassificationException;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.CachingEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.ChainedEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.EventDialogueTransformer;
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

		private final Iterable<UtteranceFiltering> uttFilteringMethods;

		public Input(final Iterable<UtteranceFiltering> uttFilteringMethods,
				final Iterable<Set<Cleaning>> cleaningMethods, final Iterable<Tokenization> tokenizationMethods,
				final Iterable<TokenType> tokenTypes, final Iterable<TokenFiltering> tokenFilteringMethods,
				final Iterable<Training> trainingMethods, final Map<SessionDataManager, Path> allSessions) {
			this.uttFilteringMethods = uttFilteringMethods;
			this.cleaningMethods = cleaningMethods;
			this.tokenizationMethods = tokenizationMethods;
			this.tokenTypes = tokenTypes;
			this.tokenFilteringMethods = tokenFilteringMethods;
			this.trainingMethods = trainingMethods;
			this.allSessions = allSessions;
		}
	}

	private static final Logger LOGGER = LoggerFactory.getLogger(CombiningBatchJobTester.class);

	// private static final Set<Cleaning>
	// MIN_REQUIRED_PRE_SENTIMENT_CLEANING_METHODS =
	// EnumSet.of(Cleaning.DISFLUENCIES,
	// Cleaning.DUPLICATES);

	private static final Set<Cleaning> MIN_REQUIRED_PRE_SENTIMENT_CLEANING_METHODS = EnumSet.noneOf(Cleaning.class);

	private final ApplicationContext appCtx;

	private final ExecutorService backgroundJobExecutor;

	private final Consumer<? super BatchJobSummary> batchJobResultHandler;

	private final BiConsumer<? super IncompleteResults, ? super Throwable> errorHandler;

	private final Consumer<? super Tester> testerConfigurator;

	public CombiningBatchJobTester(final ExecutorService backgroundJobExecutor, final ApplicationContext appCtx,
			final Consumer<? super BatchJobSummary> batchJobResultHandler,
			final BiConsumer<? super IncompleteResults, ? super Throwable> errorHandler,
			final Consumer<? super Tester> testerConfigurator) {
		this.backgroundJobExecutor = backgroundJobExecutor;
		this.appCtx = appCtx;
		this.batchJobResultHandler = batchJobResultHandler;
		this.errorHandler = errorHandler;
		this.testerConfigurator = testerConfigurator;
	}

	public void accept(final Input input) throws ClassificationException, ExecutionException, IOException {
		final SessionEventDialogueManagerCacheSupplier sessionDiagMgrCacheSupplier = appCtx
				.getBean(SessionEventDialogueManagerCacheSupplier.class);

		for (final Set<Cleaning> cleaningMethodSet : input.cleaningMethods) {
			for (final Training trainingMethod : input.trainingMethods) {
				switch (trainingMethod) {
				case SENTIMENT: {
					if (cleaningMethodSet.containsAll(MIN_REQUIRED_PRE_SENTIMENT_CLEANING_METHODS)) {
						test(input, cleaningMethodSet, trainingMethod, sessionDiagMgrCacheSupplier);
					} else {
						LOGGER.warn("Cleaning methods ({}) not ideal for sentiment analysis; Skipping.",
								cleaningMethodSet);
					}
					break;
				}
				default: {
					// Use all combinations
					test(input, cleaningMethodSet, trainingMethod, sessionDiagMgrCacheSupplier);
					break;
				}
				}
			}
		}
	}

	private void test(final Input input, final Set<Cleaning> cleaningMethodSet, final Tokenization tokenizationMethod,
			final Training trainingMethod, final SessionEventDialogueManagerCacheSupplier sessionDiagMgrCacheSupplier) {
		for (final UtteranceFiltering uttFilteringMethod : input.uttFilteringMethods) {
			for (final TokenType tokenType : input.tokenTypes) {
				final TokenizationContext tokenizationContext = new TokenizationContext(cleaningMethodSet, tokenType,
						backgroundJobExecutor);
				final EventDialogueTransformer tokenizer = tokenizationMethod.apply(tokenizationContext);

				for (final TokenFiltering tokenFilteringMethod : input.tokenFilteringMethods) {
					final EventDialogueTransformer tokenFilter = tokenFilteringMethod.get();

					final List<EventDialogueTransformer> diagTransformers = Arrays.asList(uttFilteringMethod.get(),
							tokenizer, tokenFilter);
					final CachingEventDialogueTransformer cachingDiagTransformer = new CachingEventDialogueTransformer(
							new ChainedEventDialogueTransformer(diagTransformers));
					final TrainingContext trainingCtx = new TrainingContext(cachingDiagTransformer, appCtx,
							backgroundJobExecutor);
					final Entry<TrainingInstancesFactory, Integer> trainingInstsFactoryIterCount = trainingMethod
							.apply(trainingCtx);
					final TestSetFactory testSetFactory = new TestSetFactory(trainingInstsFactoryIterCount.getKey(),
							sessionDiagMgrCacheSupplier);
					final Tester tester = appCtx.getBean(Tester.class, testSetFactory, cachingDiagTransformer,
							backgroundJobExecutor);
					tester.setIterCount(trainingInstsFactoryIterCount.getValue());
					testerConfigurator.accept(tester);
					final TestParameters testParams = new TestParameters(uttFilteringMethod, cleaningMethodSet,
							tokenizationMethod, tokenType, tokenFilteringMethod, trainingMethod);
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

	private void test(final Input input, final Set<Cleaning> cleaningMethodSet, final Training trainingMethod,
			final SessionEventDialogueManagerCacheSupplier sessionDiagMgrCacheSupplier) {
		for (final UtteranceFiltering uttFilteringMethod : input.uttFilteringMethods) {
			for (final Tokenization tokenizationMethod : input.tokenizationMethods) {
				switch (tokenizationMethod) {
				case BASIC: {
					// Use all combinations
					test(input, cleaningMethodSet, tokenizationMethod, trainingMethod, sessionDiagMgrCacheSupplier);
					break;
				}
				default: {
					// Only do if cleaning
					if (cleaningMethodSet.containsAll(MIN_REQUIRED_PRE_SENTIMENT_CLEANING_METHODS)) {
						test(input, cleaningMethodSet, tokenizationMethod, trainingMethod, sessionDiagMgrCacheSupplier);
					} else {
						LOGGER.warn("Cleaning methods ({}) not ideal for parsing; Skipping.", cleaningMethodSet);
					}
					break;
				}
				}

				for (final TokenType tokenType : input.tokenTypes) {
					final TokenizationContext tokenizationContext = new TokenizationContext(cleaningMethodSet,
							tokenType, backgroundJobExecutor);
					final EventDialogueTransformer tokenizer = tokenizationMethod.apply(tokenizationContext);

					for (final TokenFiltering tokenFilteringMethod : input.tokenFilteringMethods) {
						final EventDialogueTransformer tokenFilter = tokenFilteringMethod.get();

						final List<EventDialogueTransformer> diagTransformers = Arrays.asList(uttFilteringMethod.get(),
								tokenizer, tokenFilter);
						final CachingEventDialogueTransformer cachingDiagTransformer = new CachingEventDialogueTransformer(
								new ChainedEventDialogueTransformer(diagTransformers));
						final TrainingContext trainingCtx = new TrainingContext(cachingDiagTransformer, appCtx,
								backgroundJobExecutor);
						final Entry<TrainingInstancesFactory, Integer> trainingInstsFactoryIterCount = trainingMethod
								.apply(trainingCtx);
						final TestSetFactory testSetFactory = new TestSetFactory(trainingInstsFactoryIterCount.getKey(),
								sessionDiagMgrCacheSupplier);
						final Tester tester = appCtx.getBean(Tester.class, testSetFactory, cachingDiagTransformer,
								backgroundJobExecutor);
						tester.setIterCount(trainingInstsFactoryIterCount.getValue());
						testerConfigurator.accept(tester);
						final TestParameters testParams = new TestParameters(uttFilteringMethod, cleaningMethodSet,
								tokenizationMethod, tokenType, tokenFilteringMethod, trainingMethod);
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
