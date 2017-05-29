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
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
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

	private static final Logger LOGGER = LoggerFactory.getLogger(CombiningBatchJobTester.class);

	private final ApplicationContext appCtx;

	private final ExecutorService executor;

	private final Consumer<? super BatchJobSummary> batchJobResultHandler;

	private final Consumer<? super Tester> testerConfigurator;

	public CombiningBatchJobTester(final ExecutorService executor, final ApplicationContext appCtx,
			final Consumer<? super BatchJobSummary> batchJobResultHandler) {
		this(executor, appCtx, batchJobResultHandler, tester -> {
			// Do nothing
		});
	}

	public CombiningBatchJobTester(final ExecutorService executor, final ApplicationContext appCtx,
			final Consumer<? super BatchJobSummary> batchJobResultHandler,
			final Consumer<? super Tester> testerConfigurator) {
		this.executor = executor;
		this.appCtx = appCtx;
		this.batchJobResultHandler = batchJobResultHandler;
		this.testerConfigurator = testerConfigurator;
	}

	public void accept(final Map<SessionDataManager, Path> allSessions)
			throws ClassificationException, ExecutionException, IOException {
		final SessionEventDialogueManagerCacheSupplier sessionDiagMgrCacheSupplier = appCtx
				.getBean(SessionEventDialogueManagerCacheSupplier.class);
		for (final UtteranceFiltering uttFilteringMethod : UtteranceFiltering.values()) {
			final EventDialogueTransformer uttFilter = uttFilteringMethod.get();
			for (final Tokenization tokenizationMethod : Tokenization.values()) {
				final EventDialogueTransformer tokenizer = tokenizationMethod.get();

				for (final TokenFiltering tokenFilteringMethod : TokenFiltering.values()) {
					final EventDialogueTransformer tokenFilter = tokenFilteringMethod.get();

					final List<EventDialogueTransformer> diagTransformers = Arrays.asList(uttFilter, tokenizer,
							tokenFilter);
					final CachingEventDialogueTransformer cachingDiagTransformer = new CachingEventDialogueTransformer(
							new ChainedEventDialogueTransformer(diagTransformers));
					final TrainingContext trainingCtx = new TrainingContext(uttFilteringMethod, tokenizationMethod,
							tokenFilteringMethod, cachingDiagTransformer, appCtx);
					for (final Training trainingMethod : Training.values()) {
						final Entry<TrainingInstancesFactory, Integer> trainingInstsFactoryIterCount = trainingMethod
								.apply(trainingCtx);

						final TestSetFactory testSetFactory = new TestSetFactory(trainingInstsFactoryIterCount.getKey(),
								sessionDiagMgrCacheSupplier);
						final Tester tester = appCtx.getBean(Tester.class, testSetFactory, cachingDiagTransformer,
								executor);
						tester.setIterCount(trainingInstsFactoryIterCount.getValue());
						testerConfigurator.accept(tester);
						final TestParameters testParams = new TestParameters(uttFilteringMethod, tokenizationMethod,
								tokenFilteringMethod, trainingMethod);
						LOGGER.info("Testing {}.", testParams);
						final Tester.Result testResults = tester.apply(allSessions);
						final BatchJobSummary batchSummary = new BatchJobSummary(testParams, testResults);
						batchJobResultHandler.accept(batchSummary);
					}
				}
			}
		}
	}

}
