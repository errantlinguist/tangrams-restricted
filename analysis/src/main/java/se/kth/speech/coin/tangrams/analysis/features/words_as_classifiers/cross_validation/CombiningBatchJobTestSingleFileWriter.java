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

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumMap;
import java.util.EnumSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.OptionalInt;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.Future;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.regex.Pattern;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.MissingOptionException;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import com.google.common.cache.LoadingCache;

import edu.stanford.nlp.trees.Tree;
import edu.stanford.nlp.util.CoreMap;
import edu.stanford.nlp.util.Sets;
import it.unimi.dsi.fastutil.objects.Object2IntMap;
import se.kth.speech.coin.tangrams.CLIParameters;
import se.kth.speech.coin.tangrams.analysis.BackgroundJobs;
import se.kth.speech.coin.tangrams.analysis.DataLanguageDefaults;
import se.kth.speech.coin.tangrams.analysis.SessionGameManager;
import se.kth.speech.coin.tangrams.analysis.SessionGameManagerCacheSupplier;
import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.coin.tangrams.analysis.dialogues.UtteranceDialogueRepresentationStringFactory;
import se.kth.speech.coin.tangrams.analysis.features.weka.EntityInstanceAttributeContext;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.EventDialogueTestResults;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.WordClassDiscountingSmoother;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.cross_validation.CrossValidator.CrossValidationTestSummary;
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
import se.kth.speech.coin.tangrams.iristk.EventTimes;
import se.kth.speech.nlp.stanford.AnnotationCacheFactory;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 24 May 2017
 *
 */
final class CombiningBatchJobTestSingleFileWriter { // NO_UCD (unused code)

	private static class BatchJobSummary {

		private final TestParameters testParams;

		private final List<CrossValidator.IterationResult> testResults;

		private final LocalDateTime testTimestamp;

		private BatchJobSummary(final LocalDateTime testTimestamp, final TestParameters testParams,
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
		private TestParameters getTestParams() {
			return testParams;
		}

		/**
		 * @return the testResults
		 */
		private List<CrossValidator.IterationResult> getTestResults() {
			return testResults;
		}

		/**
		 * @return the testTimestamp
		 */
		private LocalDateTime getTestTimestamp() {
			return testTimestamp;
		}
	}

	/**
	 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
	 * @since 5 Jun 2017
	 *
	 */
	private static class CLIInputFactory {

		private static final Logger LOGGER = LoggerFactory.getLogger(CLIInputFactory.class);

		private final ExecutorService backgroundJobExecutor;

		private CLIInputFactory(final ExecutorService backgroundJobExecutor) {
			this.backgroundJobExecutor = backgroundJobExecutor;
		}

		public CombiningBatchJobTester.Input apply(final CommandLine cl)
				throws MissingOptionException, InterruptedException, ExecutionException {
			final List<Path> inpaths = Arrays.asList(cl.getArgList().stream().map(String::trim)
					.filter(path -> !path.isEmpty()).map(Paths::get).toArray(Path[]::new));
			if (inpaths.isEmpty()) {
				throw new MissingOptionException("No input path(s) specified.");
			} else {
				final Future<Map<SessionDataManager, Path>> allSessionDataFuture = backgroundJobExecutor
						.submit(() -> TestSessionData.readTestSessionData(inpaths));
				final Set<Set<Cleaning>> cleaningMethodSets = Parameter.parseCleaningMethodSets(cl);
				if (cleaningMethodSets.isEmpty()) {
					throw new IllegalArgumentException("No cleaning method set(s) specified.");
				}
				LOGGER.info("Cleaning method sets: {}", cleaningMethodSets);
				final Set<Tokenization> tokenizationMethods = Parameter.parseTokenizationMethods(cl);
				if (tokenizationMethods.isEmpty()) {
					throw new IllegalArgumentException("No tokenization method(s) specified.");
				}
				LOGGER.info("Tokenization methods: {}", tokenizationMethods);
				final Set<TokenType> tokenTypes = Parameter.parseTokenTypes(cl);
				if (tokenTypes.isEmpty()) {
					throw new IllegalArgumentException("No token type(s) specified.");
				}
				LOGGER.info("Token types: {}", tokenTypes);
				final Set<TokenFiltering> tokenFilteringMethods = Parameter.parseTokenFilteringMethods(cl);
				if (tokenFilteringMethods.isEmpty()) {
					throw new IllegalArgumentException("No token filtering method(s) specified.");
				}
				LOGGER.info("Token filtering methods: {}", tokenFilteringMethods);
				final Set<Training> trainingMethods = Parameter.parseTrainingMethods(cl);
				if (trainingMethods.isEmpty()) {
					throw new IllegalArgumentException("No training method(s) specified.");
				}
				LOGGER.info("Training methods: {}", trainingMethods);

				return new CombiningBatchJobTester.Input(cleaningMethodSets, tokenizationMethods, tokenTypes,
						tokenFilteringMethods, trainingMethods, allSessionDataFuture.get());
			}
		}

	}

	/**
	 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
	 * @since 24 May 2017
	 *
	 */
	private static class CombiningBatchJobTester implements Consumer<CombiningBatchJobTester.Input> {

		private static class IncompleteResults {

			private final TestParameters testParams;

			private final LocalDateTime testStartTime;

			private IncompleteResults(final TestParameters testParams, final LocalDateTime testStartTime) {
				this.testParams = testParams;
				this.testStartTime = testStartTime;
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
				if (!(obj instanceof IncompleteResults)) {
					return false;
				}
				final IncompleteResults other = (IncompleteResults) obj;
				if (testParams == null) {
					if (other.testParams != null) {
						return false;
					}
				} else if (!testParams.equals(other.testParams)) {
					return false;
				}
				if (testStartTime == null) {
					if (other.testStartTime != null) {
						return false;
					}
				} else if (!testStartTime.equals(other.testStartTime)) {
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
				result = prime * result + (testStartTime == null ? 0 : testStartTime.hashCode());
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
			private TestParameters getTestParams() {
				return testParams;
			}

			/**
			 * @return the testStartTime
			 */
			private LocalDateTime getTestStartTime() {
				return testStartTime;
			}
		}

		private static class Input {

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
				if (!(obj instanceof Input)) {
					return false;
				}
				final Input other = (Input) obj;
				if (allSessions == null) {
					if (other.allSessions != null) {
						return false;
					}
				} else if (!allSessions.equals(other.allSessions)) {
					return false;
				}
				if (cleaningMethods == null) {
					if (other.cleaningMethods != null) {
						return false;
					}
				} else if (!cleaningMethods.equals(other.cleaningMethods)) {
					return false;
				}
				if (tokenFilteringMethods == null) {
					if (other.tokenFilteringMethods != null) {
						return false;
					}
				} else if (!tokenFilteringMethods.equals(other.tokenFilteringMethods)) {
					return false;
				}
				if (tokenTypes == null) {
					if (other.tokenTypes != null) {
						return false;
					}
				} else if (!tokenTypes.equals(other.tokenTypes)) {
					return false;
				}
				if (tokenizationMethods == null) {
					if (other.tokenizationMethods != null) {
						return false;
					}
				} else if (!tokenizationMethods.equals(other.tokenizationMethods)) {
					return false;
				}
				if (trainingMethods == null) {
					if (other.trainingMethods != null) {
						return false;
					}
				} else if (!trainingMethods.equals(other.trainingMethods)) {
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
				result = prime * result + (allSessions == null ? 0 : allSessions.hashCode());
				result = prime * result + (cleaningMethods == null ? 0 : cleaningMethods.hashCode());
				result = prime * result + (tokenFilteringMethods == null ? 0 : tokenFilteringMethods.hashCode());
				result = prime * result + (tokenTypes == null ? 0 : tokenTypes.hashCode());
				result = prime * result + (tokenizationMethods == null ? 0 : tokenizationMethods.hashCode());
				result = prime * result + (trainingMethods == null ? 0 : trainingMethods.hashCode());
				return result;
			}

			/*
			 * (non-Javadoc)
			 *
			 * @see java.lang.Object#toString()
			 */
			@Override
			public String toString() {
				final StringBuilder builder = new StringBuilder(2048);
				builder.append("Input [allSessions=");
				builder.append(allSessions);
				builder.append(", cleaningMethods=");
				builder.append(cleaningMethods);
				builder.append(", tokenFilteringMethods=");
				builder.append(tokenFilteringMethods);
				builder.append(", tokenizationMethods=");
				builder.append(tokenizationMethods);
				builder.append(", tokenTypes=");
				builder.append(tokenTypes);
				builder.append(", trainingMethods=");
				builder.append(trainingMethods);
				builder.append("]");
				return builder.toString();
			}
		}

		/**
		 * Parsing can take up huge amounts of memory, so it's single-threaded
		 * and thus the caches are created designed for single-threaded
		 * operation.
		 */
		private static final AnnotationCacheFactory ANNOTATION_CACHE_FACTORY = new AnnotationCacheFactory(1);

		private static final Logger LOGGER = LoggerFactory.getLogger(CombiningBatchJobTester.class);

		private final ApplicationContext appCtx;

		private final ExecutorService backgroundJobExecutor;

		private final Consumer<? super BatchJobSummary> batchJobResultHandler;

		private final BiConsumer<? super IncompleteResults, ? super Throwable> errorHandler;

		private final BiConsumer<? super CoreMap, ? super List<Tree>> extractionResultsHook;

		private final Consumer<? super CrossValidator> testerConfigurator;

		private final TestSetFactoryFactory testSetFactoryFactory;

		private final Map<WordClassifierTrainingParameter, Object> trainingParams;

		private final BiConsumer<? super EventDialogue, ? super List<UtteranceRelation>> uttRelHandler;

		CombiningBatchJobTester(final ExecutorService backgroundJobExecutor, final ApplicationContext appCtx,
				final Consumer<? super BatchJobSummary> batchJobResultHandler,
				final BiConsumer<? super IncompleteResults, ? super Throwable> errorHandler,
				final Consumer<? super CrossValidator> testerConfigurator,
				final BiConsumer<? super CoreMap, ? super List<Tree>> extractionResultsHook,
				final BiConsumer<? super EventDialogue, ? super List<UtteranceRelation>> uttRelHandler,
				final Map<WordClassifierTrainingParameter, Object> trainingParams,
				final TestSetFactoryFactory testSetFactoryFactory) {
			this.backgroundJobExecutor = backgroundJobExecutor;
			this.appCtx = appCtx;
			this.batchJobResultHandler = batchJobResultHandler;
			this.errorHandler = errorHandler;
			this.testerConfigurator = testerConfigurator;
			this.extractionResultsHook = extractionResultsHook;
			this.uttRelHandler = uttRelHandler;
			this.trainingParams = trainingParams;
			this.testSetFactoryFactory = testSetFactoryFactory;
		}

		@Override
		public void accept(final Input input) {
			LOGGER.debug("Bean names: {}", Arrays.toString(appCtx.getBeanDefinitionNames()));
			final SessionGameManagerCacheSupplier sessionDiagMgrCacheSupplier = appCtx
					.getBean(SessionGameManagerCacheSupplier.class);
			final LoadingCache<SessionDataManager, SessionGameManager> sessionGameMgrs = sessionDiagMgrCacheSupplier
					.get();

			for (final Set<Cleaning> cleaningMethodSet : input.cleaningMethods) {
				for (final Training trainingMethod : input.trainingMethods) {
					for (final Tokenization tokenizationMethod : input.tokenizationMethods) {
						for (final TokenType tokenType : input.tokenTypes) {
							final Tokenization.Context tokenizationContext = new Tokenization.Context(cleaningMethodSet,
									tokenType, extractionResultsHook, ANNOTATION_CACHE_FACTORY);
							final EventDialogueTransformer tokenizer = tokenizationMethod.apply(tokenizationContext);

							for (final TokenFiltering tokenFilteringMethod : input.tokenFilteringMethods) {
								final EventDialogueTransformer tokenFilter = tokenFilteringMethod.get();

								final CachingEventDialogueTransformer symmetricalDiagTransformer = trainingMethod
										.createSymmetricalTrainingTestingEventDiagTransformer(tokenizer, tokenFilter);
								final TrainingContext trainingCtx = new TrainingContext(symmetricalDiagTransformer,
										appCtx, uttRelHandler, trainingParams);
								final TrainingInstancesFactory trainingInstsFactory = trainingMethod
										.createTrainingInstsFactory(trainingCtx);
								final Function<Map<SessionDataManager, Path>, Stream<Entry<SessionDataManager, WordClassificationData>>> testSetFactory = testSetFactoryFactory
										.apply(trainingInstsFactory, sessionGameMgrs);
								final Integer smoothingMinCount = (Integer) trainingCtx.getTrainingParams()
										.get(WordClassifierTrainingParameter.SMOOTHING_MIN_COUNT);
								final WordClassDiscountingSmoother smoother = appCtx
										.getBean(WordClassDiscountingSmoother.class, smoothingMinCount);
								final CrossValidator crossValidator = appCtx.getBean(CrossValidator.class,
										testSetFactory, symmetricalDiagTransformer,
										trainingMethod.getClassifierFactory(trainingCtx), smoother,
										backgroundJobExecutor);
								crossValidator.setIterCount(trainingMethod.getIterCount());
								testerConfigurator.accept(crossValidator);
								final TestParameters testParams = new TestParameters(cleaningMethodSet,
										tokenizationMethod, tokenType, tokenFilteringMethod, trainingMethod,
										trainingParams);
								LOGGER.info("Testing {}.", testParams);

								final LocalDateTime testTimestamp = LocalDateTime.now();
								try {
									final List<CrossValidator.IterationResult> testResults = crossValidator
											.apply(input.allSessions);
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

	private static class NonDiscountingTestSetFactoryFactory implements TestSetFactoryFactory {

		private final Map<WordClassifierTrainingParameter, Object> trainingParams;

		private NonDiscountingTestSetFactoryFactory(final Map<WordClassifierTrainingParameter, Object> trainingParams) {
			this.trainingParams = trainingParams;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.util.function.BiFunction#apply(java.lang.Object,
		 * java.lang.Object)
		 */
		@Override
		public Function<Map<SessionDataManager, Path>, Stream<Entry<SessionDataManager, WordClassificationData>>> apply(
				final TrainingInstancesFactory trainingInstsFactory,
				final LoadingCache<SessionDataManager, SessionGameManager> sessionGameMgrs) {
			final Function<Map<SessionDataManager, Path>, Stream<Entry<SessionDataManager, WordClassificationData>>> result;
			final int trainingSetSizeDiscountingConstant = (Integer) trainingParams
					.get(WordClassifierTrainingParameter.TRAINING_SET_SIZE_DISCOUNTING_CONSTANT);
			if (trainingSetSizeDiscountingConstant == 0) {
				result = new TestSetFactory(trainingInstsFactory, sessionGameMgrs);
			} else {
				throw new IllegalArgumentException("Training set size discounting not supported.");
			}
			return result;
		}

	}

	/**
	 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
	 * @since 5 Jun 2017
	 *
	 */
	private enum Parameter implements Supplier<Option> {
		CLEANING("c") {
			@Override
			public Option get() {
				final Cleaning[] possibleVals = Cleaning.values();
				return Option.builder(optName).longOpt("cleaning")
						.desc("A list of cleaning method(s) to use. Possible values: " + Arrays.toString(possibleVals))
						.hasArg().argName("names").required().build();
			}
		},
		HELP("?") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("help").desc("Prints this message.").build();
			}
		},
		ITER_COUNT("i") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("iter-count")
						.desc("The number of training/testing iterations to run for each cross-validation dataset.")
						.hasArg().argName("count").type(Number.class).build();
			}
		},
		OUTPATH("o") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("outpath").desc("The path to write the data to.").hasArg()
						.argName("path").type(File.class).required().build();
			}
		},
		TEST_CLEANING_POWERSET("cp") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("cleaning-powerset")
						.desc("If this flag is present, the powerset of the supplied cleaning methods is tested rather than the set itself.")
						.build();
			}
		},
		TOKEN_FILTERS("tf") {
			@Override
			public Option get() {
				final TokenFiltering[] possibleVals = TokenFiltering.values();
				return Option.builder(optName).longOpt("token-filters").desc(
						"A list of token filtering method(s) to use. Possible values: " + Arrays.toString(possibleVals))
						.hasArg().argName("names").required().build();
			}
		},
		TOKEN_TYPES("tt") {
			@Override
			public Option get() {
				final TokenType[] possibleVals = TokenType.values();
				return Option.builder(optName).longOpt("token-types")
						.desc("A list of token type(s) to use. Possible values: " + Arrays.toString(possibleVals))
						.hasArg().argName("names").required().build();
			}
		},
		TOKENIZERS("tok") {
			@Override
			public Option get() {
				final Tokenization[] possibleVals = Tokenization.values();
				return Option.builder(optName).longOpt("tokenizers").desc(
						"A list of tokenization method(s) to use. Possible values: " + Arrays.toString(possibleVals))
						.hasArg().argName("names").required().build();
			}
		},
		TRAINING("tr") {
			@Override
			public Option get() {
				final Training[] possibleVals = Training.values();
				return Option.builder(optName).longOpt("training")
						.desc("A list of training method(s) to use. Possible values: " + Arrays.toString(possibleVals))
						.hasArg().argName("names").required().build();
			}
		};

		private static final Logger LOGGER = LoggerFactory.getLogger(Parameter.class);

		private static final Pattern MULTI_OPT_VALUE_DELIMITER = Pattern.compile("\\s+");

		private static Set<Cleaning> parseCleaningMethods(final CommandLine cl) {
			final String[] names = parseOptEnumValueNames(cl, Parameter.CLEANING.optName);
			final Stream<Cleaning> insts = names == null ? Stream.empty()
					: Arrays.stream(names).map(String::trim).filter(str -> !str.isEmpty()).map(Cleaning::valueOf);
			final EnumSet<Cleaning> result = EnumSet.noneOf(Cleaning.class);
			insts.forEach(result::add);
			return result;
		}

		private static String[] parseOptEnumValueNames(final CommandLine cl, final String optName) {
			final String val = cl.getOptionValue(optName);
			return val == null ? null : MULTI_OPT_VALUE_DELIMITER.split(val);
		}

		static Set<Set<Cleaning>> parseCleaningMethodSets(final CommandLine cl) {
			final Set<Cleaning> cleaningMethods = parseCleaningMethods(cl);
			LOGGER.info("Cleaning methods: {}", cleaningMethods);
			return cl.hasOption(Parameter.TEST_CLEANING_POWERSET.optName) ? Sets.powerSet(cleaningMethods)
					: Collections.singleton(cleaningMethods);
		}

		static Set<TokenFiltering> parseTokenFilteringMethods(final CommandLine cl) {
			final String[] names = parseOptEnumValueNames(cl, Parameter.TOKEN_FILTERS.optName);
			final Stream<TokenFiltering> insts = names == null ? Stream.empty()
					: Arrays.stream(names).map(String::trim).filter(str -> !str.isEmpty()).map(TokenFiltering::valueOf);
			final EnumSet<TokenFiltering> result = EnumSet.noneOf(TokenFiltering.class);
			insts.forEach(result::add);
			return result;
		}

		static Set<Tokenization> parseTokenizationMethods(final CommandLine cl) {
			final String[] names = parseOptEnumValueNames(cl, Parameter.TOKENIZERS.optName);
			final Stream<Tokenization> insts = names == null ? Stream.empty()
					: Arrays.stream(names).map(String::trim).filter(str -> !str.isEmpty()).map(Tokenization::valueOf);
			final EnumSet<Tokenization> result = EnumSet.noneOf(Tokenization.class);
			insts.forEach(result::add);
			return result;
		}

		static Set<TokenType> parseTokenTypes(final CommandLine cl) {
			final String[] names = parseOptEnumValueNames(cl, Parameter.TOKEN_TYPES.optName);
			final Stream<TokenType> insts = names == null ? Stream.empty()
					: Arrays.stream(names).map(String::trim).filter(str -> !str.isEmpty()).map(TokenType::valueOf);
			final EnumSet<TokenType> result = EnumSet.noneOf(TokenType.class);
			insts.forEach(result::add);
			return result;
		}

		static Set<Training> parseTrainingMethods(final CommandLine cl) {
			final String[] names = parseOptEnumValueNames(cl, Parameter.TRAINING.optName);
			final Stream<Training> insts = names == null ? Stream.empty()
					: Arrays.stream(names).map(String::trim).filter(str -> !str.isEmpty()).map(Training::valueOf);
			final EnumSet<Training> result = EnumSet.noneOf(Training.class);
			insts.forEach(result::add);
			return result;
		}

		protected final String optName;

		private Parameter(final String optName) {
			this.optName = optName;
		}
	}

	/**
	 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
	 * @since 5 Jun 2017
	 *
	 */
	private static class TestParameterReporting {

		/**
		 * <strong>NOTE:</strong> This is for SPSS compatibility, which does not
		 * allow e.g.&nbsp;<code>"-"</code> as part of a variable name.
		 *
		 * @see <a href=
		 *      "https://www.ibm.com/support/knowledgecenter/en/SSLVMB_21.0.0/com.ibm.spss.statistics.help/syn_variables_variable_names.htm">SPSS
		 *      documentation</a>
		 */
		private static final String SUBCOL_NAME_DELIM = ".";

		static final DateTimeFormatter TIMESTAMP_FORMATTER = EventTimes.FORMATTER;

		private static Stream<String> createCleaningMethodBooleanValues(
				final Collection<? super Cleaning> cleaningMethods) {
			final IntStream vals = Arrays.stream(Cleaning.values()).map(cleaningMethods::contains)
					.mapToInt(boolVal -> boolVal ? 1 : 0);
			return vals.mapToObj(Integer::toString);
		}

		private static Stream<String> createColHeaders(
				final List<DialogueAnalysisSummaryFactory.SummaryDatum> summaryDataToWrite) {
			final Stream.Builder<String> resultBuilder = Stream.builder();
			resultBuilder.add("TIME");
			createTestMethodColumnHeaders().forEachOrdered(resultBuilder);
			createTrainingDataColHeaders().forEachOrdered(resultBuilder);
			summaryDataToWrite.stream().map(DialogueAnalysisSummaryFactory.SummaryDatum::toString)
					.forEachOrdered(resultBuilder);
			return resultBuilder.build();
		}

		private static Stream<String> createTestMethodColumnHeaders() {
			final Stream.Builder<String> resultBuilder = Stream.builder();
			final String cleaningMethodPrefix = Cleaning.class.getSimpleName() + SUBCOL_NAME_DELIM;
			Arrays.stream(Cleaning.values()).map(method -> cleaningMethodPrefix + method).forEachOrdered(resultBuilder);
			resultBuilder.add(Tokenization.class.getSimpleName().toString());
			resultBuilder.add(TokenType.class.getSimpleName());
			resultBuilder.add(TokenFiltering.class.getSimpleName());
			resultBuilder.add(Training.class.getSimpleName());
			return resultBuilder.build();
		}

		private static Stream<String> createTestMethodRowCellValues(final TestParameters testParams,
				final Function<? super Set<Cleaning>, Stream<String>> cleaningMethodReprFactory) {
			final Stream.Builder<String> resultBuilder = Stream.builder();
			final Set<Cleaning> cleaningMethods = testParams.getCleaning();
			cleaningMethodReprFactory.apply(cleaningMethods).forEachOrdered(resultBuilder);
			resultBuilder.add(testParams.getTokenization().toString());
			resultBuilder.add(testParams.getTokenType().toString());
			resultBuilder.add(testParams.getTokenFiltering().toString());
			resultBuilder.add(testParams.getTrainingMethod().toString());
			return resultBuilder.build();
		}

		private static Stream<String> createTestParamRowCellValues(final BatchJobSummary summary) {
			final Stream.Builder<String> resultBuilder = Stream.builder();
			resultBuilder.add(TIMESTAMP_FORMATTER.format(summary.getTestTimestamp()));
			createTestMethodRowCellValues(summary.getTestParams(),
					TestParameterReporting::createCleaningMethodBooleanValues).forEachOrdered(resultBuilder);
			return resultBuilder.build();
		}

		private static Stream<String> createTrainingDataColHeaders() {
			return EntityInstanceAttributeContext.getClassValues().stream()
					.map(classVal -> "TRAIN_INSTS" + SUBCOL_NAME_DELIM + classVal);
		}

		private static Stream<Object> createTrainingDataRowCellValues(final CrossValidationTestSummary cvTestSummary) {
			final Object2IntMap<String> trainingInstCounts = cvTestSummary.getTrainingInstanceCounts();
			return EntityInstanceAttributeContext.getClassValues().stream().map(trainingInstCounts::getInt);
		}

		private TestParameterReporting() {
		}

	}

	private static class TestParameters {

		private final Set<Cleaning> cleaning;

		private final TokenFiltering tokenFiltering;

		private final Tokenization tokenization;

		private final TokenType tokenType;

		private final Training trainingMethod;

		private final Map<WordClassifierTrainingParameter, Object> trainingParams;

		private TestParameters(final Set<Cleaning> cleaning, final Tokenization tokenization, final TokenType tokenType,
				final TokenFiltering tokenFiltering, final Training trainingMethod,
				final Map<WordClassifierTrainingParameter, Object> trainingParams) {
			this.cleaning = cleaning;
			this.tokenization = tokenization;
			this.tokenType = tokenType;
			this.tokenFiltering = tokenFiltering;
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
			if (cleaning == null) {
				if (other.cleaning != null) {
					return false;
				}
			} else if (!cleaning.equals(other.cleaning)) {
				return false;
			}
			if (tokenFiltering != other.tokenFiltering) {
				return false;
			}
			if (tokenType != other.tokenType) {
				return false;
			}
			if (tokenization != other.tokenization) {
				return false;
			}
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
			result = prime * result + (cleaning == null ? 0 : cleaning.hashCode());
			result = prime * result + (tokenFiltering == null ? 0 : tokenFiltering.hashCode());
			result = prime * result + (tokenType == null ? 0 : tokenType.hashCode());
			result = prime * result + (tokenization == null ? 0 : tokenization.hashCode());
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
			builder.append("TestParameters [cleaning=");
			builder.append(cleaning);
			builder.append(", tokenFiltering=");
			builder.append(tokenFiltering);
			builder.append(", tokenization=");
			builder.append(tokenization);
			builder.append(", tokenType=");
			builder.append(tokenType);
			builder.append(", trainingMethod=");
			builder.append(trainingMethod);
			builder.append(", trainingParams=");
			builder.append(trainingParams);
			builder.append("]");
			return builder.toString();
		}

		/**
		 * @return the cleaning
		 */
		private Set<Cleaning> getCleaning() {
			return cleaning;
		}

		/**
		 * @return the tokenFiltering
		 */
		private TokenFiltering getTokenFiltering() {
			return tokenFiltering;
		}

		/**
		 * @return the tokenization
		 */
		private Tokenization getTokenization() {
			return tokenization;
		}

		/**
		 * @return the tokenType
		 */
		private TokenType getTokenType() {
			return tokenType;
		}

		/**
		 * @return the trainingMethod
		 */
		private Training getTrainingMethod() {
			return trainingMethod;
		}

		/**
		 * @return the trainingParams
		 */
		private Map<WordClassifierTrainingParameter, Object> getTrainingParams() {
			return trainingParams;
		}
	}

	static final class TestException extends RuntimeException {

		/**
		 *
		 */
		private static final long serialVersionUID = -2393007769462670325L;

		/**
		 * @param cause
		 */
		private TestException(final Throwable cause) {
			super(cause);
		}

	}

	private static final List<DialogueAnalysisSummaryFactory.SummaryDatum> DEFAULT_DATA_TO_WRITE = createDefaultDatumOrderingList();

	private static final String EXTRACTION_LOG_FILE_SUFFIX = ".extraction.tsv";

	private static final Logger LOGGER = LoggerFactory.getLogger(CombiningBatchJobTestSingleFileWriter.class);

	private static final String NULL_CELL_VALUE_REPR = "?";

	private static final Options OPTIONS = createOptions();

	private static final Charset OUTPUT_ENCODING = StandardCharsets.UTF_8;

	private static final Collector<CharSequence, ?, String> ROW_CELL_JOINER = Collectors.joining("\t");

	private static final String UTT_REL_LOG_FILE_SUFFIX = ".uttrels.tsv";

	public static void main(final String[] args) throws CrossValidationTestException {
		final CommandLineParser parser = new DefaultParser();
		try {
			final CommandLine cl = parser.parse(OPTIONS, args);
			main(cl);
		} catch (final ParseException e) {
			System.out.println(String.format("An error occured while parsing the command-line arguments: %s", e));
			printHelp();
		}
	}

	private static List<DialogueAnalysisSummaryFactory.SummaryDatum> createDefaultDatumOrderingList() {
		final Set<DialogueAnalysisSummaryFactory.SummaryDatum> excludedData = EnumSet.of(
				DialogueAnalysisSummaryFactory.SummaryDatum.DIALOGUE,
				DialogueAnalysisSummaryFactory.SummaryDatum.DIALOGUE_AS_TESTED,
				DialogueAnalysisSummaryFactory.SummaryDatum.GOLD_STD_ID);
		return Arrays.asList(DialogueAnalysisSummaryFactory.getDefaultSummaryDatumOrdering().stream()
				.filter(datum -> !excludedData.contains(datum))
				.toArray(DialogueAnalysisSummaryFactory.SummaryDatum[]::new));
	}

	private static BufferedWriter createExtrLogFileWriter(final File outFile) throws IOException {
		Path extractionLogOutPath;
		final String suffix = EXTRACTION_LOG_FILE_SUFFIX;
		if (outFile == null) {
			final String filename = CombiningBatchJobTester.class.getSimpleName() + "-" + System.currentTimeMillis()
					+ suffix;
			extractionLogOutPath = Paths.get(System.getProperty("user.dir"), filename);
		} else {
			extractionLogOutPath = Paths.get(outFile.getPath() + suffix);
		}
		LOGGER.info("Will write phrase-extraction log to \"{}\".", extractionLogOutPath);
		return Files.newBufferedWriter(extractionLogOutPath, OUTPUT_ENCODING, StandardOpenOption.CREATE,
				StandardOpenOption.TRUNCATE_EXISTING);
	}

	private static Options createOptions() {
		final Options result = new Options();
		Arrays.stream(Parameter.values()).map(Parameter::get).forEach(result::addOption);
		return result;
	}

	private static BufferedWriter createUttRelFileWriter(final File outFile) throws IOException {
		Path uttRelLogOutPath;
		final String suffix = UTT_REL_LOG_FILE_SUFFIX;
		if (outFile == null) {
			final String filename = CombiningBatchJobTester.class.getSimpleName() + "-" + System.currentTimeMillis()
					+ suffix;
			uttRelLogOutPath = Paths.get(System.getProperty("user.dir"), filename);
		} else {
			uttRelLogOutPath = Paths.get(outFile.getPath() + suffix);
		}
		LOGGER.info("Will write dialogue utterance relation log to \"{}\".", uttRelLogOutPath);
		return Files.newBufferedWriter(uttRelLogOutPath, OUTPUT_ENCODING, StandardOpenOption.CREATE,
				StandardOpenOption.TRUNCATE_EXISTING);
	}

	private static void main(final CommandLine cl) throws CrossValidationTestException {
		if (cl.hasOption(Parameter.HELP.optName)) {
			printHelp();
		} else {
			final ForkJoinPool backgroundJobExecutor = BackgroundJobs.getBackgroundJobExecutor();
			final CLIInputFactory inputFactory = new CLIInputFactory(backgroundJobExecutor);
			try {
				final CombiningBatchJobTester.Input input = inputFactory.apply(cl);
				final Consumer<CrossValidator> testerConfigurator;
				{
					final OptionalInt optIterCount = CLIParameters
							.parseIterCount((Number) cl.getParsedOptionValue(Parameter.ITER_COUNT.optName));
					if (optIterCount.isPresent()) {
						final int iterCount = optIterCount.getAsInt();
						LOGGER.info("Will run {} training/testing iteration(s).", iterCount);
						testerConfigurator = tester -> tester.setIterCount(iterCount);
					} else {
						testerConfigurator = tester -> {
							// Do nothing
						};
					}
				}

				final File outFile = (File) cl.getParsedOptionValue(Parameter.OUTPATH.optName);
				try (PrintWriter out = CLIParameters.parseOutpath(outFile, OUTPUT_ENCODING)) {
					final CombiningBatchJobTestSingleFileWriter writer = new CombiningBatchJobTestSingleFileWriter(out,
							true);
					try (BufferedWriter extrLogOut = createExtrLogFileWriter(outFile)) {
						try (BufferedWriter uttRelLogOut = createUttRelFileWriter(outFile)) {

							try (final ClassPathXmlApplicationContext appCtx = new ClassPathXmlApplicationContext(
									"cross-validation.xml", CombiningBatchJobTestSingleFileWriter.class)) {
								final Map<WordClassifierTrainingParameter, Object> trainingParams = WordClassifierTrainingParameter
										.createDefaultMap();
								final CombiningBatchJobTester tester = new CombiningBatchJobTester(
										backgroundJobExecutor, appCtx, writer::write, writer::writeError,
										testerConfigurator, new ExtractionLogWriter(extrLogOut),
										new UtteranceRelationLogWriter(uttRelLogOut, NULL_CELL_VALUE_REPR),
										trainingParams, new NonDiscountingTestSetFactoryFactory(trainingParams));
								tester.accept(input);
							}
							LOGGER.info("Shutting down executor service.");
							backgroundJobExecutor.shutdown();
							LOGGER.info("Successfully shut down executor service.");

						}
					}
				}
			} catch (final Exception e) {
				shutdownExceptionally(backgroundJobExecutor);
				throw new CrossValidationTestException(e);
			}
		}
	}

	private static void printHelp() {
		final HelpFormatter formatter = new HelpFormatter();
		formatter.printHelp(CombiningBatchJobTestSingleFileWriter.class.getSimpleName() + " INPATHS...", OPTIONS);
	}

	private static void shutdownExceptionally(final ExecutorService executor) {
		LOGGER.debug("Emergency executor service shutdown.");
		executor.shutdownNow();
		LOGGER.debug("Successfully shut down executor service.");
	}

	private final List<DialogueAnalysisSummaryFactory.SummaryDatum> dataToWrite;

	private final PrintWriter out;

	private final DialogueAnalysisSummaryFactory rowDataFactory;

	private boolean writeHeader;

	private CombiningBatchJobTestSingleFileWriter(final PrintWriter out, final boolean writeHeader) {
		this(out, writeHeader, new UtteranceDialogueRepresentationStringFactory(DataLanguageDefaults.getLocale()),
				DEFAULT_DATA_TO_WRITE);
	}

	private CombiningBatchJobTestSingleFileWriter(final PrintWriter out, final boolean writeHeader,
			final Function<? super Iterator<Utterance>, String> uttDiagReprFactory,
			final List<DialogueAnalysisSummaryFactory.SummaryDatum> dataToWrite) {
		this.out = out;
		this.writeHeader = writeHeader;
		this.dataToWrite = dataToWrite;
		rowDataFactory = new DialogueAnalysisSummaryFactory(uttDiagReprFactory, dataToWrite);
	}

	public void write(final BatchJobSummary summary) {
		if (writeHeader) {
			out.println(TestParameterReporting.createColHeaders(dataToWrite).collect(ROW_CELL_JOINER));
			writeHeader = false;
		}
		final String[] testParamRowCellValues = TestParameterReporting.createTestParamRowCellValues(summary)
				.toArray(String[]::new);
		final List<CrossValidator.IterationResult> testResults = summary.getTestResults();
		for (final CrossValidator.IterationResult iterResult : testResults) {
			final int iterNo = iterResult.getIterNo();
			iterResult.getCvTestResults().forEach(infileSessionResults -> {
				final Path inpath = infileSessionResults.getKey();
				final CrossValidationTestSummary cvTestSummary = infileSessionResults.getValue();
				final String[] trainingRowCellVals = TestParameterReporting
						.createTrainingDataRowCellValues(cvTestSummary).map(Object::toString).toArray(String[]::new);
				// NOTE: This should remain here, after "Iterator.next()", so
				// that the printed first iteration is "1" rather than "0"
				int sessionDialogueOrder = 1;
				for (final Entry<EventDialogue, EventDialogueTestResults> diagTestResults : cvTestSummary
						.getTestResults().getDialogueTestResults()) {
					final Map<DialogueAnalysisSummaryFactory.SummaryDatum, Object> rowData = rowDataFactory
							.apply(new DialogueAnalysisSummaryFactory.Input(inpath, "Success", iterNo,
									sessionDialogueOrder++, diagTestResults,
									summary.getTestParams().getTrainingParams(), cvTestSummary.getSessionStartTime()));
					final Stream<String> diagAnalysisRowCellVals = dataToWrite.stream().map(rowData::get)
							.map(Object::toString);
					final Stream.Builder<String> rowCellValBuilder = Stream.builder();
					Arrays.stream(testParamRowCellValues).forEachOrdered(rowCellValBuilder);
					Arrays.stream(trainingRowCellVals).forEachOrdered(rowCellValBuilder);
					diagAnalysisRowCellVals.forEachOrdered(rowCellValBuilder);
					final String row = rowCellValBuilder.build().collect(ROW_CELL_JOINER);
					out.println(row);
				}
			});
		}

	}

	public void writeError(final CombiningBatchJobTester.IncompleteResults incompleteResults, final Throwable thrown) {
		LOGGER.error(
				String.format("An error occurred while running test which was started at \"%s\".",
						TestParameterReporting.TIMESTAMP_FORMATTER.format(incompleteResults.getTestStartTime())),
				thrown);
		final String errorDesc = String.format("%s: %s", thrown.getClass().getName(), thrown.getLocalizedMessage());

		final Stream.Builder<String> rowCellValBuilder = Stream.builder();
		rowCellValBuilder.add(TestParameterReporting.TIMESTAMP_FORMATTER.format(incompleteResults.getTestStartTime()));
		TestParameterReporting.createTestMethodRowCellValues(incompleteResults.getTestParams(),
				TestParameterReporting::createCleaningMethodBooleanValues).forEachOrdered(rowCellValBuilder);
		EntityInstanceAttributeContext.getClassValues().stream().map(val -> NULL_CELL_VALUE_REPR)
				.forEach(rowCellValBuilder);
		final Map<DialogueAnalysisSummaryFactory.SummaryDatum, Object> rowData = new EnumMap<>(
				DialogueAnalysisSummaryFactory.SummaryDatum.class);
		dataToWrite.forEach(datum -> rowData.put(datum, NULL_CELL_VALUE_REPR));
		rowData.put(DialogueAnalysisSummaryFactory.SummaryDatum.DESCRIPTION, errorDesc);
		final Stream<String> diagAnalysisRowCellVals = dataToWrite.stream().map(rowData::get).map(Object::toString);
		diagAnalysisRowCellVals.forEachOrdered(rowCellValBuilder);
		final String row = rowCellValBuilder.build().collect(ROW_CELL_JOINER);
		out.println(row);
		throw new TestException(thrown);
	}

}
