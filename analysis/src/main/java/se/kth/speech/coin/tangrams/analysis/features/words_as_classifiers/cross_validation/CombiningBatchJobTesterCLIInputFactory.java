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
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.MissingOptionException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.coin.tangrams.analysis.SessionDataManager;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.cross_validation.CombiningBatchJobTester.Input;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 5 Jun 2017
 *
 */
final class CombiningBatchJobTesterCLIInputFactory {

	private static final Logger LOGGER = LoggerFactory.getLogger(CombiningBatchJobTesterCLIInputFactory.class);

	private final ExecutorService backgroundJobExecutor;

	public CombiningBatchJobTesterCLIInputFactory(final ExecutorService backgroundJobExecutor) {
		this.backgroundJobExecutor = backgroundJobExecutor;
	}

	public Input apply(final CommandLine cl) throws MissingOptionException, InterruptedException, ExecutionException {
		final List<Path> inpaths = Arrays.asList(cl.getArgList().stream().map(String::trim)
				.filter(path -> !path.isEmpty()).map(Paths::get).toArray(Path[]::new));
		if (inpaths.isEmpty()) {
			throw new MissingOptionException("No input path(s) specified.");
		} else {
			final Future<Map<SessionDataManager, Path>> allSessionDataFuture = backgroundJobExecutor
					.submit(() -> TestSessionData.readTestSessionData(inpaths));
			final Set<UtteranceFiltering> uttFilteringMethods = CLITestParameter.parseUttFilteringMethods(cl);
			LOGGER.info("Utterance filtering methods: {}", uttFilteringMethods);
			if (uttFilteringMethods.isEmpty()) {
				throw new IllegalArgumentException("No utterance filtering method(s) specified.");
			}
			final Set<Cleaning> cleaningMethods = CLITestParameter.parseCleaningMethods(cl);
			LOGGER.info("Cleaning methods: {}", cleaningMethods);
			// final Future<Set<Set<Cleaning>>> cleaningMethodSets =
			// backgroundJobExecutor
			// .submit(() -> Sets.powerSet(cleaningMethods));
			final Set<Tokenization> tokenizationMethods = CLITestParameter.parseTokenizationMethods(cl);
			if (tokenizationMethods.isEmpty()) {
				throw new IllegalArgumentException("No tokenization method(s) specified.");
			}
			LOGGER.info("Tokenization methods: {}", tokenizationMethods);
			final Set<TokenType> tokenTypes = CLITestParameter.parseTokenTypes(cl);
			if (tokenTypes.isEmpty()) {
				throw new IllegalArgumentException("No token type(s) specified.");
			}
			LOGGER.info("Token types: {}", tokenTypes);
			final Set<TokenFiltering> tokenFilteringMethods = CLITestParameter.parseTokenFilteringMethods(cl);
			if (tokenFilteringMethods.isEmpty()) {
				throw new IllegalArgumentException("No token filtering method(s) specified.");
			}
			LOGGER.info("Token filtering methods: {}", tokenFilteringMethods);
			final Set<Training> trainingMethods = CLITestParameter.parseTrainingMethods(cl);
			if (trainingMethods.isEmpty()) {
				throw new IllegalArgumentException("No training method(s) specified.");
			}
			LOGGER.info("Training methods: {}", trainingMethods);

			return new CombiningBatchJobTester.Input(uttFilteringMethods, Collections.singleton(cleaningMethods),
					tokenizationMethods, tokenTypes, tokenFilteringMethods, trainingMethods,
					allSessionDataFuture.get());
		}
	}

}
