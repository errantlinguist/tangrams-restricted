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
import java.util.Arrays;
import java.util.Collections;
import java.util.EnumMap;
import java.util.EnumSet;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
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
import se.kth.speech.coin.tangrams.CLIParameters;
import se.kth.speech.coin.tangrams.analysis.BackgroundJobs;
import se.kth.speech.coin.tangrams.analysis.DataLanguageDefaults;
import se.kth.speech.coin.tangrams.analysis.SessionGameManager;
import se.kth.speech.coin.tangrams.analysis.SessionGameManagerCacheSupplier;
import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.coin.tangrams.analysis.dialogues.UtteranceDialogueRepresentationStringFactory;
import se.kth.speech.coin.tangrams.analysis.features.ClassificationException;
import se.kth.speech.coin.tangrams.analysis.features.weka.EntityInstanceAttributeContext;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.EventDialogueTestResults;
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
import se.kth.speech.nlp.stanford.AnnotationCacheFactory;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 24 May 2017
 *
 */
final class CombiningBatchJobTestSingleFileWriter { // NO_UCD (unused code)

	/**
	 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
	 * @since 5 Jun 2017
	 *
	 */
	private static class CLIInputFactory {

		private static final Logger LOGGER = LoggerFactory.getLogger(CLIInputFactory.class);

		private final ExecutorService backgroundJobExecutor;

		public CLIInputFactory(final ExecutorService backgroundJobExecutor) {
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
	private static class CombiningBatchJobTester {

		private static class IncompleteResults {

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

		void accept(final Input input) throws ClassificationException, IOException {
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
										.createSymmetricalTrainingTestingEventDiagTransformer(
												Arrays.asList(tokenizer, tokenFilter));
								final TrainingContext trainingCtx = new TrainingContext(symmetricalDiagTransformer,
										appCtx, uttRelHandler, trainingParams);
								final TrainingInstancesFactory trainingInstsFactory = trainingMethod
										.createTrainingInstsFactory(trainingCtx);
								final Function<Map<SessionDataManager, Path>, Stream<Entry<SessionDataManager, WordClassificationData>>> testSetFactory = testSetFactoryFactory
										.apply(trainingInstsFactory, sessionGameMgrs);
								final CrossValidator crossValidator = appCtx.getBean(CrossValidator.class,
										testSetFactory, symmetricalDiagTransformer,
										trainingMethod.getClassifierFactory(trainingCtx), backgroundJobExecutor);
								crossValidator.setIterCount(trainingMethod.getIterCount());
								testerConfigurator.accept(crossValidator);
								final TestParameters testParams = new TestParameters(cleaningMethodSet,
										tokenizationMethod, tokenType, tokenFilteringMethod, trainingMethod,
										trainingParams);
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

	static final class TestException extends RuntimeException {

		/**
		 *
		 */
		private static final long serialVersionUID = -2393007769462670325L;

		/**
		 * @param cause
		 */
		public TestException(final Throwable cause) {
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

	public static void main(final String[] args) throws BatchJobTestException {
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

	private static void main(final CommandLine cl) throws BatchJobTestException {
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
									"combining-batch-tester.xml", CombiningBatchJobTestSingleFileWriter.class)) {
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
				throw new BatchJobTestException(e);
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
		final CrossValidator.Result testResults = summary.getTestResults();
		for (final Entry<Path, List<CrossValidationTestSummary>> infileSessionResults : testResults.getSessionResults()
				.entrySet()) {
			final Path inpath = infileSessionResults.getKey();
			final List<CrossValidationTestSummary> sessionResultList = infileSessionResults.getValue();
			for (final ListIterator<CrossValidationTestSummary> sessionResultIter = sessionResultList
					.listIterator(); sessionResultIter.hasNext();) {
				final CrossValidationTestSummary cvTestSummary = sessionResultIter.next();
				final String[] trainingRowCellVals = TestParameterReporting
						.createTrainingDataRowCellValues(cvTestSummary).map(Object::toString).toArray(String[]::new);
				// NOTE: This should remain here, after "Iterator.next()", so
				// that the printed first iteration is "1" rather than "0"
				final int iterNo = sessionResultIter.nextIndex();
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
			}
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
