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
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.OptionalInt;
import java.util.Random;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.function.BiFunction;
import java.util.function.Supplier;
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

import edu.stanford.nlp.trees.CollinsHeadFinder;
import se.kth.speech.MutablePair;
import se.kth.speech.coin.tangrams.analysis.SessionDataManager;
import se.kth.speech.coin.tangrams.analysis.SessionEventDialogueManagerCacheSupplier;
import se.kth.speech.coin.tangrams.analysis.features.ClassificationException;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeatureExtractionContextFactory;
import se.kth.speech.coin.tangrams.analysis.features.weka.EntityInstanceAttributeContext;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.cross_validation.StatisticsWriter.SummaryDatum;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.CachingEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.ChainedEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.DummyEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.EventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.FallbackTokenizingEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.InstructorUtteranceFilteringEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.TokenFilteringEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.TokenizingEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.OnePositiveMaximumNegativeInstancesFactory;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.OnePositiveOneNegativeInstanceFactory;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.TrainingInstancesFactory;
import se.kth.speech.nlp.EnglishLocationalPrepositions;
import se.kth.speech.nlp.PatternTokenizer;
import se.kth.speech.nlp.PhrasalHeadFilteringPredicate;
import se.kth.speech.nlp.SnowballPorter2EnglishStopwordSetFactory;
import se.kth.speech.nlp.SnowballPorter2EnglishStopwords;
import se.kth.speech.nlp.StanfordCoreNLPConfigurationVariant;
import se.kth.speech.nlp.StanfordCoreNLPLemmatizer;
import se.kth.speech.nlp.StanfordCoreNLPParsingTokenizer;
import se.kth.speech.nlp.StanfordCoreNLPTokenizer;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 24 May 2017
 *
 */
public final class CombiningBatchApplicationContextTester {

	private enum Parameter implements Supplier<Option> {
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
						.hasArgs().argName("count").type(Number.class).build();
			}
		},
		OUTPATH("o") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("outpath").desc("The path to write the data to.").hasArg()
						.argName("path").type(File.class).required().build();
			}
		};

		private static final Options OPTIONS = createOptions();

		private static Options createOptions() {
			final Options result = new Options();
			Arrays.stream(Parameter.values()).map(Parameter::get).forEach(result::addOption);
			return result;
		}

		private static OptionalInt parseIterCount(final CommandLine cl) throws ParseException {
			final Number optVal = (Number) cl.getParsedOptionValue(Parameter.ITER_COUNT.optName);
			final OptionalInt result;
			if (optVal == null) {
				result = OptionalInt.empty();
			} else {
				final int val = optVal.intValue();
				LOGGER.info("Will run {} training/testing iteration(s).", val);
				result = OptionalInt.of(val);
			}
			return result;
		}

		private static void printHelp() {
			final HelpFormatter formatter = new HelpFormatter();
			formatter.printHelp(CombiningBatchApplicationContextTester.class.getSimpleName() + " INPATHS...", OPTIONS);
		}

		protected final String optName;

		private Parameter(final String optName) {
			this.optName = optName;
		}

	}

	private static class Summary {

		private static final List<String> COL_HEADERS;

		private static final List<SummaryDatum> SUMMARY_DATA_TO_WRITE;

		static {
			SUMMARY_DATA_TO_WRITE = Arrays.asList(SummaryDatum.MEAN_RANK, SummaryDatum.MRR, SummaryDatum.DIALOGUE_COUNT,
					SummaryDatum.UTTERANCES_TESTED, SummaryDatum.MEAN_UTTERANCES_PER_DIALOGUE);
			COL_HEADERS = createColHeaderList(SUMMARY_DATA_TO_WRITE);
		}

		private static List<String> createColHeaderList(final List<SummaryDatum> summaryDataToWrite) {
			final Stream.Builder<String> colHeaderStreamBuilder = Stream.builder();
			TestParameters.createColumnHeaders().forEachOrdered(colHeaderStreamBuilder);
			colHeaderStreamBuilder.add("OUTPATH");
			colHeaderStreamBuilder.add("ITER_COUNT");
			summaryDataToWrite.stream().map(SummaryDatum::toString).forEachOrdered(colHeaderStreamBuilder);
			return Arrays.asList(colHeaderStreamBuilder.build().toArray(String[]::new));
		}

		/**
		 * @return the colHeaders
		 */
		private static List<String> getColHeaders() {
			return COL_HEADERS;
		}

		private final Path outdirPath;

		private final TestParameters testParams;

		private final Tester.Result testResults;

		private Summary(final TestParameters testParams, final Tester.Result testResults, final Path outdirPath) {
			this.testParams = testParams;
			this.testResults = testResults;
			this.outdirPath = outdirPath;
		}

		private Stream<String> createRowCellValues() {
			final Stream.Builder<String> resultBuilder = Stream.builder();
			testParams.createRowCellValues().forEachOrdered(resultBuilder);
			final Map<SummaryDatum, Object> configSummary = StatisticsWriter.createSummaryDataMap(outdirPath.toString(),
					testResults);
			resultBuilder.add(configSummary.get(SummaryDatum.KEY).toString());
			resultBuilder.add(configSummary.get(SummaryDatum.TEST_ITERATION).toString());
			SUMMARY_DATA_TO_WRITE.stream().map(configSummary::get).map(Object::toString).forEachOrdered(resultBuilder);
			return resultBuilder.build();
		}
	}

	private static class TestParameters {

		private static Stream<String> createColumnHeaders() {
			return Stream.of(UtteranceFiltering.class.getSimpleName(), Tokenization.class.getSimpleName(),
					TokenFiltering.class.getSimpleName(), Training.class.getSimpleName());
		}

		private final TokenFiltering tokenFilteringMethod;

		private final Tokenization tokenizationMethod;

		private final Training trainingMethod;

		private final UtteranceFiltering uttFilteringMethod;

		private TestParameters(final UtteranceFiltering uttFilteringMethod, final Tokenization tokenizationMethod,
				final TokenFiltering tokenFilteringMethod, final Training trainingMethod) {
			this.uttFilteringMethod = uttFilteringMethod;
			this.tokenizationMethod = tokenizationMethod;
			this.tokenFilteringMethod = tokenFilteringMethod;
			this.trainingMethod = trainingMethod;
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
			if (tokenFilteringMethod != other.tokenFilteringMethod) {
				return false;
			}
			if (tokenizationMethod != other.tokenizationMethod) {
				return false;
			}
			if (trainingMethod != other.trainingMethod) {
				return false;
			}
			if (uttFilteringMethod != other.uttFilteringMethod) {
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
			result = prime * result + (tokenFilteringMethod == null ? 0 : tokenFilteringMethod.hashCode());
			result = prime * result + (tokenizationMethod == null ? 0 : tokenizationMethod.hashCode());
			result = prime * result + (trainingMethod == null ? 0 : trainingMethod.hashCode());
			result = prime * result + (uttFilteringMethod == null ? 0 : uttFilteringMethod.hashCode());
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
			builder.append("TestParameters [uttFilteringMethod=");
			builder.append(uttFilteringMethod);
			builder.append(", tokenizationMethod=");
			builder.append(tokenizationMethod);
			builder.append(", tokenFilteringMethod=");
			builder.append(tokenFilteringMethod);
			builder.append(", trainingMethod=");
			builder.append(trainingMethod);
			builder.append("]");
			return builder.toString();
		}

		private String createBatchOutdirName() {
			final Stream<String> keyNames = Stream.of(uttFilteringMethod.getKeyName(), tokenizationMethod.getKeyName(),
					tokenFilteringMethod.getKeyName(), trainingMethod.getKeyName());
			return keyNames.collect(METHOD_KEY_NAME_JOINER);
		}

		private Stream<String> createRowCellValues() {
			return Stream.of(uttFilteringMethod.keyName, tokenizationMethod.keyName, tokenFilteringMethod.keyName,
					trainingMethod.keyName);
		}
	}

	private enum TokenFiltering implements Supplier<EventDialogueTransformer> {
		NO_FILTER(DUMMY_EVT_DIAG_TRANSFORMER, "noFilter"), STOPWORDS_FILLERS(
				createStopwordFilteringTransformer(EnumSet.of(SnowballPorter2EnglishStopwords.Variant.CANONICAL,
						SnowballPorter2EnglishStopwords.Variant.FILLERS)),
				"stopsFillers");

		private final EventDialogueTransformer held;

		private final String keyName;

		private TokenFiltering(final EventDialogueTransformer held, final String keyName) {
			this.held = held;
			this.keyName = keyName;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.util.function.Supplier#get()
		 */
		@Override
		public EventDialogueTransformer get() {
			return held;
		}

		/**
		 * @return the keyName
		 */
		protected String getKeyName() {
			return keyName;
		}

	}

	private enum Tokenization implements Supplier<EventDialogueTransformer> {
		BASIC_TOKENIZER("tokenized") {

			@Override
			public TokenizingEventDialogueTransformer get() {
				return new TokenizingEventDialogueTransformer(
						new StanfordCoreNLPTokenizer(StanfordCoreNLPConfigurationVariant.TOKENIZING.get()));
			}
		},
		LEMMATIZER("lemmatized") {

			@Override
			public TokenizingEventDialogueTransformer get() {
				return new TokenizingEventDialogueTransformer(new StanfordCoreNLPLemmatizer(
						StanfordCoreNLPConfigurationVariant.TOKENIZING_LEMMATIZING.get()));
			}
		},
		PP_REMOVER("remFillers-ppRemoval") {

			@Override
			public ChainedEventDialogueTransformer get() {
				final TokenFilteringEventDialogueTransformer fillerRemover = createStopwordFilteringTransformer(
						EnumSet.of(SnowballPorter2EnglishStopwords.Variant.FILLERS));
				final Map<String, Set<List<String>>> labelHeadBlacklists = Collections.singletonMap("PP",
						EnglishLocationalPrepositions.loadSet());
				final PhrasalHeadFilteringPredicate pred = new PhrasalHeadFilteringPredicate(labelHeadBlacklists,
						new CollinsHeadFinder());
				final FallbackTokenizingEventDialogueTransformer tokenizer = new FallbackTokenizingEventDialogueTransformer(
						new StanfordCoreNLPParsingTokenizer(
								StanfordCoreNLPConfigurationVariant.TOKENIZING_LEMMATIZING_PARSING.get(), pred),
						new PatternTokenizer());
				return new ChainedEventDialogueTransformer(Arrays.asList(fillerRemover, tokenizer));
			}
		};

		private final String keyName;

		private Tokenization(final String keyName) {
			this.keyName = keyName;
		}

		/**
		 * @return the keyName
		 */
		protected String getKeyName() {
			return keyName;
		}
	}

	private enum Training implements
			BiFunction<EventDialogueTransformer, ApplicationContext, Entry<TrainingInstancesFactory, Integer>> {
		ALL_NEG("allNeg") {
			@Override
			public Entry<TrainingInstancesFactory, Integer> apply(final EventDialogueTransformer diagTransformer,
					final ApplicationContext appCtx) {
				final EntityInstanceAttributeContext entityInstAttrCtx = appCtx
						.getBean(EntityInstanceAttributeContext.class);
				final EntityFeatureExtractionContextFactory extCtxFactory = appCtx
						.getBean(EntityFeatureExtractionContextFactory.class);
				final OnePositiveMaximumNegativeInstancesFactory instsFactory = new OnePositiveMaximumNegativeInstancesFactory(
						entityInstAttrCtx, diagTransformer, extCtxFactory);
				return new MutablePair<>(instsFactory, 1);
			}
		},
		ONE_NEG("oneNeg") {

			@Override
			public Entry<TrainingInstancesFactory, Integer> apply(final EventDialogueTransformer diagTransformer,
					final ApplicationContext appCtx) {
				final EntityInstanceAttributeContext entityInstAttrCtx = appCtx
						.getBean(EntityInstanceAttributeContext.class);
				final EntityFeatureExtractionContextFactory extCtxFactory = appCtx
						.getBean(EntityFeatureExtractionContextFactory.class);
				final OnePositiveOneNegativeInstanceFactory instsFactory = new OnePositiveOneNegativeInstanceFactory(
						entityInstAttrCtx, diagTransformer, extCtxFactory, RND);
				return new MutablePair<>(instsFactory, 10);
			}

		};

		private final String keyName;

		private Training(final String keyName) {
			this.keyName = keyName;
		}

		/**
		 * @return the keyName
		 */
		protected String getKeyName() {
			return keyName;
		}
	}

	private enum UtteranceFiltering implements Supplier<EventDialogueTransformer> {
		ALL_UTTS(DUMMY_EVT_DIAG_TRANSFORMER, "allUtts"), INSTRUCTOR_UTTS(
				new InstructorUtteranceFilteringEventDialogueTransformer(), "instructorUtts");

		private final EventDialogueTransformer held;

		private final String keyName;

		private UtteranceFiltering(final EventDialogueTransformer held, final String keyName) {
			this.held = held;
			this.keyName = keyName;
		}

		@Override
		public EventDialogueTransformer get() {
			return held;
		}

		/**
		 * @return the keyName
		 */
		protected String getKeyName() {
			return keyName;
		}
	}

	private static final DummyEventDialogueTransformer DUMMY_EVT_DIAG_TRANSFORMER = new DummyEventDialogueTransformer();

	private static final Logger LOGGER = LoggerFactory.getLogger(CombiningBatchApplicationContextTester.class);

	private static final Collector<CharSequence, ?, String> METHOD_KEY_NAME_JOINER = Collectors.joining("_");

	private static final Random RND = new Random(1);

	private static final Collector<CharSequence, ?, String> ROW_CELL_JOINER = Collectors.joining("\t");

	public static void main(final CommandLine cl)
			throws ParseException, InterruptedException, ExecutionException, ClassificationException, IOException {
		if (cl.hasOption(Parameter.HELP.optName)) {
			Parameter.printHelp();
		} else {
			final List<Path> inpaths = Arrays.asList(cl.getArgList().stream().map(Paths::get).toArray(Path[]::new));
			if (inpaths.isEmpty()) {
				throw new MissingOptionException("No input path(s) specified.");

			} else {
				final OptionalInt iterCount = Parameter.parseIterCount(cl);
				final Path outdir = ((File) cl.getParsedOptionValue(Parameter.OUTPATH.optName)).toPath();
				LOGGER.info("Will write data to \"{}\".", outdir);

				final ExecutorService executor = createExecutorService();
				try {
					final Future<Map<SessionDataManager, Path>> allSessionDataFuture = executor
							.submit(() -> TestSessionData.readTestSessionData(inpaths));

					try (final ClassPathXmlApplicationContext appCtx = new ClassPathXmlApplicationContext(
							"combining-batch-tester.xml", CombiningBatchApplicationContextTester.class)) {
						final CombiningBatchApplicationContextTester tester = new CombiningBatchApplicationContextTester(
								executor, iterCount, outdir, appCtx);
						tester.apply(allSessionDataFuture.get());
					}
					LOGGER.info("Shutting down executor service.");
					executor.shutdown();
					LOGGER.info("Successfully shut down executor service.");

				} catch (final InterruptedException e) {
					shutdownExceptionally(executor);
					throw e;
				} catch (final ExecutionException e) {
					shutdownExceptionally(executor);
					throw e;
				} catch (final ClassificationException e) {
					shutdownExceptionally(executor);
					throw e;
				} catch (final IOException e) {
					shutdownExceptionally(executor);
					throw e;
				} catch (final RuntimeException e) {
					shutdownExceptionally(executor);
					throw e;
				}
			}
		}
	}

	public static void main(final String[] args)
			throws IOException, ClassificationException, ExecutionException, InterruptedException {
		final CommandLineParser parser = new DefaultParser();
		try {
			final CommandLine cl = parser.parse(Parameter.OPTIONS, args);
			main(cl);
		} catch (final ParseException e) {
			System.out.println(String.format("An error occured while parsing the command-line arguments: %s", e));
			Parameter.printHelp();
		}
	}

	private static ExecutorService createExecutorService() {
		return createExecutorService(Math.max(Runtime.getRuntime().availableProcessors() - 1, 1));
	}

	private static ExecutorService createExecutorService(final int parallelThreadCount) {
		LOGGER.info("Will run with {} parallel thread(s).", parallelThreadCount);
		return Executors.newFixedThreadPool(parallelThreadCount);
	}

	private static TokenFilteringEventDialogueTransformer createStopwordFilteringTransformer(
			final Collection<SnowballPorter2EnglishStopwords.Variant> variantsToUnify) {
		final SnowballPorter2EnglishStopwordSetFactory factory = new SnowballPorter2EnglishStopwordSetFactory();
		factory.setVariantsToUnify(variantsToUnify);
		return new TokenFilteringEventDialogueTransformer(factory.getObject());
	}

	private static void shutdownExceptionally(final ExecutorService executor) {
		LOGGER.debug("Emergency executor service shutdown.");
		executor.shutdownNow();
		LOGGER.debug("Successfully shut down executor service.");
	}

	private final ApplicationContext appCtx;

	private final ExecutorService executor;

	private final OptionalInt iterCount;

	private final Path outdir;

	private final Path summaryFile;

	public CombiningBatchApplicationContextTester(final ExecutorService executor, final OptionalInt iterCount,
			final Path outdir, final ApplicationContext appCtx) {
		this.executor = executor;
		this.iterCount = iterCount;
		this.outdir = outdir;
		this.appCtx = appCtx;

		summaryFile = outdir.resolve("batch-summary.tsv");
	}

	public void apply(final Map<SessionDataManager, Path> allSessions)
			throws ClassificationException, ExecutionException, IOException {
		writeInitialSummaryFile();

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

					for (final Training trainingMethod : Training.values()) {
						final Entry<TrainingInstancesFactory, Integer> trainingInstsFactoryIterCount = trainingMethod
								.apply(cachingDiagTransformer, appCtx);

						final TestSetFactory testSetFactory = new TestSetFactory(trainingInstsFactoryIterCount.getKey(),
								sessionDiagMgrCacheSupplier);
						final Tester tester = appCtx.getBean(Tester.class, testSetFactory, cachingDiagTransformer,
								executor);
						tester.setIterCount(trainingInstsFactoryIterCount.getValue());
						iterCount.ifPresent(tester::setIterCount);
						final TestParameters testParams = new TestParameters(uttFilteringMethod, tokenizationMethod,
								tokenFilteringMethod, trainingMethod);
						LOGGER.info("Testing {}.", testParams);
						final Tester.Result testResults = tester.apply(allSessions);
						final String batchOutdirName = testParams.createBatchOutdirName();
						final Path outdirPath = Files.createDirectories(outdir.resolve(batchOutdirName));
						LOGGER.info("Will write results of testing {} to \"{}\".", testParams, outdirPath);
						final BatchTestResultWriter testWriter = new BatchTestResultWriter(outdirPath);
						testWriter.apply(testResults);
						writeSummary(new Summary(testParams, testResults, outdirPath));
					}
				}
			}
		}
	}

	private void writeInitialSummaryFile() throws IOException {
		Files.createDirectories(summaryFile.getParent());
		try (final BufferedWriter summaryWriter = Files.newBufferedWriter(summaryFile, StandardOpenOption.CREATE,
				StandardOpenOption.TRUNCATE_EXISTING)) {
			summaryWriter.write(Summary.getColHeaders().stream().collect(ROW_CELL_JOINER));
		}
	}

	private void writeSummary(final Summary summary) throws IOException {
		try (final BufferedWriter summaryWriter = Files.newBufferedWriter(summaryFile, StandardOpenOption.APPEND)) {
			summaryWriter.newLine();
			final Stream<String> rowCellVals = summary.createRowCellValues();
			summaryWriter.write(rowCellVals.collect(ROW_CELL_JOINER));
		}
	}

}
