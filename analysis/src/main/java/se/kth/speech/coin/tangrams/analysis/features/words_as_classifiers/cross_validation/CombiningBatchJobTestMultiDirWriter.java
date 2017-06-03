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
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.EnumMap;
import java.util.EnumSet;
import java.util.List;
import java.util.Map;
import java.util.OptionalInt;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.Future;
import java.util.function.Consumer;
import java.util.function.Function;
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
import org.springframework.context.support.ClassPathXmlApplicationContext;

import com.google.common.collect.Sets;

import se.kth.speech.coin.tangrams.CLIParameters;
import se.kth.speech.coin.tangrams.analysis.SessionDataManager;
import se.kth.speech.coin.tangrams.analysis.features.ClassificationException;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.cross_validation.CombiningBatchJobTester.IncompleteResults;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.cross_validation.StatisticsWriter.SummaryDatum;
import se.kth.speech.coin.tangrams.iristk.EventTimes;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 24 May 2017
 *
 */
public final class CombiningBatchJobTestMultiDirWriter {

	private enum Parameter implements Supplier<Option> {
		APPEND_SUMMARY("a") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("append")
						.desc("If this flag is present, the summary file is appended to rather than being truncated and written anew.")
						.build();
			}
		},
		CLEANING("c") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("cleaning").desc(
						"A list of cleaning method(s) to use Possible values: " + Arrays.toString(Cleaning.values()))
						.hasArgs().argName("name").build();
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
						.hasArgs().argName("count").type(Number.class).build();
			}
		},
		NO_CLOBBER("nc"){
			public Option get() {
				return Option.builder(optName).longOpt("append")
						.desc("If this flag is present, existing batch result directories will not be overwritten.")
						.build();
			}
		},
		OUTPATH("o") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("outpath").desc("The path to write the data to.").hasArg()
						.argName("path").type(File.class).required().build();
			}
		},
		TOKEN_FILTERS("tf") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("token-filters")
						.desc("A list of token filtering method(s) to use. Possible values: "
								+ Arrays.toString(TokenFiltering.values()))
						.hasArgs().argName("name").build();
			}
		},
		TOKEN_TYPES("ty") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("token-types")
						.desc("A list of token type(s) to use Possible values: " + Arrays.toString(TokenType.values()))
						.hasArgs().argName("name").build();
			}
		},
		TOKENIZERS("to") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("tokenizers")
						.desc("A list of tokenization method(s) to use Possible values: "
								+ Arrays.toString(Tokenization.values()))
						.hasArgs().argName("name").build();
			}
		},
		TRAINING("tr") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("training").desc(
						"A list of training method(s) to use Possible values: " + Arrays.toString(Training.values()))
						.hasArgs().argName("name").build();
			}
		},
		UTT_FILTERS("u") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("utt-filters")
						.desc("A list of utterance filtering method(s) to use Possible values: "
								+ Arrays.toString(UtteranceFiltering.values()))
						.hasArgs().argName("name").build();
			}
		};

		private static final Options OPTIONS = createOptions();

		private static Options createOptions() {
			final Options result = new Options();
			Arrays.stream(Parameter.values()).map(Parameter::get).forEach(result::addOption);
			return result;
		}

		private static Set<Cleaning> parseCleaningMethods(final CommandLine cl) {
			final String[] names = cl.getOptionValues(Parameter.CLEANING.optName);
			final Stream<Cleaning> insts = names == null ? Arrays.stream(Cleaning.values())
					: Arrays.stream(names).map(Cleaning::valueOf);
			final EnumSet<Cleaning> result = EnumSet.noneOf(Cleaning.class);
			insts.forEach(result::add);
			return result;
		}

		private static Set<TokenFiltering> parseTokenFilteringMethods(final CommandLine cl) {
			final String[] names = cl.getOptionValues(Parameter.TOKEN_FILTERS.optName);
			final Stream<TokenFiltering> insts = names == null ? Arrays.stream(TokenFiltering.values())
					: Arrays.stream(names).map(TokenFiltering::valueOf);
			final EnumSet<TokenFiltering> result = EnumSet.noneOf(TokenFiltering.class);
			insts.forEach(result::add);
			return result;
		}

		private static Set<Tokenization> parseTokenizationMethods(final CommandLine cl) {
			final String[] names = cl.getOptionValues(Parameter.TOKENIZERS.optName);
			final Stream<Tokenization> insts = names == null ? Arrays.stream(Tokenization.values())
					: Arrays.stream(names).map(Tokenization::valueOf);
			final EnumSet<Tokenization> result = EnumSet.noneOf(Tokenization.class);
			insts.forEach(result::add);
			return result;
		}

		private static Set<TokenType> parseTokenTypes(final CommandLine cl) {
			final String[] names = cl.getOptionValues(Parameter.TOKEN_TYPES.optName);
			final Stream<TokenType> insts = names == null ? Arrays.stream(TokenType.values())
					: Arrays.stream(names).map(TokenType::valueOf);
			final EnumSet<TokenType> result = EnumSet.noneOf(TokenType.class);
			insts.forEach(result::add);
			return result;
		}

		private static Set<Training> parseTrainingMethods(final CommandLine cl) {
			final String[] names = cl.getOptionValues(Parameter.TRAINING.optName);
			final Stream<Training> insts = names == null ? Arrays.stream(Training.values())
					: Arrays.stream(names).map(Training::valueOf);
			final EnumSet<Training> result = EnumSet.noneOf(Training.class);
			insts.forEach(result::add);
			return result;
		}

		private static Set<UtteranceFiltering> parseUttFilteringMethods(final CommandLine cl) {
			final String[] names = cl.getOptionValues(Parameter.UTT_FILTERS.optName);
			final Stream<UtteranceFiltering> insts = names == null ? Arrays.stream(UtteranceFiltering.values())
					: Arrays.stream(names).map(UtteranceFiltering::valueOf);
			final EnumSet<UtteranceFiltering> result = EnumSet.noneOf(UtteranceFiltering.class);
			insts.forEach(result::add);
			return result;
		}

		private static void printHelp() {
			final HelpFormatter formatter = new HelpFormatter();
			formatter.printHelp(CombiningBatchJobTestMultiDirWriter.class.getSimpleName() + " INPATHS...", OPTIONS);
		}

		protected final String optName;

		private Parameter(final String optName) {
			this.optName = optName;
		}

	}

	private static final List<String> COL_HEADERS;

	private static final Logger LOGGER = LoggerFactory.getLogger(CombiningBatchJobTestMultiDirWriter.class);

	private static final Collector<CharSequence, ?, String> BATCH_DIR_METHOD_NAME_JOINER = Collectors.joining(",");

	private static final String NULL_CELL_VALUE_REPR = "?";

	private static final Collector<CharSequence, ?, String> ROW_CELL_JOINER = Collectors.joining("\t");

	private static final List<SummaryDatum> SUMMARY_DATA_TO_WRITE;

	private static final DateTimeFormatter TIMESTAMP_FORMATTER = EventTimes.FORMATTER;

	static {
		SUMMARY_DATA_TO_WRITE = Arrays.asList(SummaryDatum.MEAN_RANK, SummaryDatum.MRR, SummaryDatum.DIALOGUE_COUNT,
				SummaryDatum.UTTERANCES_TESTED, SummaryDatum.MEAN_UTTERANCES_PER_DIALOGUE);
		COL_HEADERS = createColHeaderList(SUMMARY_DATA_TO_WRITE);
	}
	
	public static void main(final CommandLine cl)
			throws ParseException, InterruptedException, ExecutionException, ClassificationException, IOException {
		if (cl.hasOption(Parameter.HELP.optName)) {
			Parameter.printHelp();
		} else {
			final List<Path> inpaths = Arrays.asList(cl.getArgList().stream().map(Paths::get).toArray(Path[]::new));
			if (inpaths.isEmpty()) {
				throw new MissingOptionException("No input path(s) specified.");

			} else {
				final ExecutorService backgroundJobExecutor = createBackgroundJobExecutor();
				final Future<Map<SessionDataManager, Path>> allSessionDataFuture = backgroundJobExecutor
						.submit(() -> TestSessionData.readTestSessionData(inpaths));

				final Consumer<Tester> testerConfigurator;
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

				final Path outdir = ((File) cl.getParsedOptionValue(Parameter.OUTPATH.optName)).toPath();
				LOGGER.info("Will write data to \"{}\".", outdir);
				final boolean appendSummary = cl.hasOption(Parameter.APPEND_SUMMARY.optName);
				LOGGER.info("Append to summary rather than truncate? {}", appendSummary);
				final boolean noClobber = cl.hasOption(Parameter.NO_CLOBBER.optName);
				LOGGER.info("Don't clobber old batch results? {}", appendSummary);
				final Set<UtteranceFiltering> uttFilteringMethods = Parameter.parseUttFilteringMethods(cl);
				LOGGER.info("Utterance filtering methods: {}", uttFilteringMethods);
				final Set<Cleaning> cleaningMethods = Parameter.parseCleaningMethods(cl);
				LOGGER.info("Cleaning methods: {}", cleaningMethods);
				final Future<Set<Set<Cleaning>>> cleaningMethodSets = backgroundJobExecutor
						.submit(() -> Sets.powerSet(cleaningMethods));
				final Set<Tokenization> tokenizationMethods = Parameter.parseTokenizationMethods(cl);
				LOGGER.info("Tokenization methods: {}", tokenizationMethods);
				final Set<TokenType> tokenTypes = Parameter.parseTokenTypes(cl);
				LOGGER.info("Token types: {}", tokenTypes);
				final Set<TokenFiltering> tokenFilteringMethods = Parameter.parseTokenFilteringMethods(cl);
				LOGGER.info("Token filtering methods: {}", tokenFilteringMethods);
				final Set<Training> trainingMethods = Parameter.parseTrainingMethods(cl);
				LOGGER.info("Training methods: {}", trainingMethods);

				try {

					final CombiningBatchJobTestMultiDirWriter writer = new CombiningBatchJobTestMultiDirWriter(outdir,
							!appendSummary);
					try (final ClassPathXmlApplicationContext appCtx = new ClassPathXmlApplicationContext(
							"combining-batch-tester.xml", CombiningBatchJobTestMultiDirWriter.class)) {
						final CombiningBatchJobTester tester = new CombiningBatchJobTester(backgroundJobExecutor,
								appCtx, writer::write, writer::writeError, testerConfigurator);
						final CombiningBatchJobTester.Input input = new CombiningBatchJobTester.Input(
								uttFilteringMethods, cleaningMethodSets.get(), tokenizationMethods, tokenTypes,
								tokenFilteringMethods, trainingMethods, allSessionDataFuture.get());
						tester.accept(input);
					}
					LOGGER.info("Shutting down executor service.");
					backgroundJobExecutor.shutdown();
					LOGGER.info("Successfully shut down executor service.");

				} catch (final InterruptedException e) {
					shutdownExceptionally(backgroundJobExecutor);
					throw e;
				} catch (final ExecutionException e) {
					shutdownExceptionally(backgroundJobExecutor);
					throw e;
				} catch (final ClassificationException e) {
					shutdownExceptionally(backgroundJobExecutor);
					throw e;
				} catch (final IOException e) {
					shutdownExceptionally(backgroundJobExecutor);
					throw e;
				} catch (final RuntimeException e) {
					shutdownExceptionally(backgroundJobExecutor);
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

	private static ExecutorService createBackgroundJobExecutor() {
		return ForkJoinPool.commonPool();
	}

	private static String createBatchOutdirName(final TestParameters testParams) {
		final Stream<String> rowCellVals = createTestMethodRowCellValues(testParams,
				CombiningBatchJobTestMultiDirWriter::createCleaningMethodSetValues);
		return rowCellVals.collect(BATCH_DIR_METHOD_NAME_JOINER);
	}

	private static Stream<String> createCleaningMethodBooleanValues(final Set<Cleaning> cleaningMethods) {
		return Arrays.stream(Cleaning.values()).map(cleaningMethods::contains).map(Object::toString);
	}

	private static Stream<String> createCleaningMethodSetValues(final EnumSet<Cleaning> cleaningMethods) {
		return cleaningMethods.stream().map(Cleaning::toString);
	}

	private static Stream<String> createCleaningMethodSetValues(final Set<Cleaning> cleaningMethods) {
		final EnumSet<Cleaning> downcast;
		if (cleaningMethods instanceof EnumSet<?>) {
			downcast = (EnumSet<Cleaning>) cleaningMethods;
		} else if (cleaningMethods.isEmpty()) {
			downcast = EnumSet.noneOf(Cleaning.class);
		} else {
			downcast = EnumSet.copyOf(cleaningMethods);
		}
		return createCleaningMethodSetValues(downcast);
	}

	private static List<String> createColHeaderList(final List<SummaryDatum> summaryDataToWrite) {
		final Stream.Builder<String> resultBuilder = Stream.builder();
		resultBuilder.add("TIME");
		resultBuilder.accept("DESC");
		createTestMethodColumnHeaders().forEachOrdered(resultBuilder);
		resultBuilder.add("OUTDIR");
		resultBuilder.add("ITER_COUNT");
		summaryDataToWrite.stream().map(SummaryDatum::toString).forEachOrdered(resultBuilder);
		return Arrays.asList(resultBuilder.build().toArray(String[]::new));
	}

	private static Stream<String> createRowCellValues(final BatchJobSummary summary, final Path outdirPath) {
		final Stream.Builder<String> resultBuilder = Stream.builder();
		resultBuilder.add(TIMESTAMP_FORMATTER.format(summary.getTestTimestamp()));
		resultBuilder.add("Success");
		createTestMethodRowCellValues(summary.getTestParams(),
				CombiningBatchJobTestMultiDirWriter::createCleaningMethodBooleanValues).forEachOrdered(resultBuilder);
		final Map<SummaryDatum, Object> configSummary = StatisticsWriter.createSummaryDataMap(null,
				summary.getTestResults());
		resultBuilder.add(outdirPath.toString());
		resultBuilder.add(configSummary.get(SummaryDatum.TEST_ITERATION).toString());
		SUMMARY_DATA_TO_WRITE.stream().map(configSummary::get).map(Object::toString).forEachOrdered(resultBuilder);
		return resultBuilder.build();
	}

	private static Stream<String> createRowCellValues(final IncompleteResults incompleteResults, final String errorDesc,
			final Path outdirPath) {
		final Stream.Builder<String> resultBuilder = Stream.builder();
		resultBuilder.add(TIMESTAMP_FORMATTER.format(incompleteResults.getTestStartTime()));
		resultBuilder.add(errorDesc);
		createTestMethodRowCellValues(incompleteResults.getTestParams(),
				CombiningBatchJobTestMultiDirWriter::createCleaningMethodBooleanValues).forEachOrdered(resultBuilder);
		final Map<SummaryDatum, Object> configSummary = new EnumMap<>(SummaryDatum.class);
		Arrays.stream(SummaryDatum.values()).forEach(datum -> configSummary.put(datum, NULL_CELL_VALUE_REPR));
		resultBuilder.add(outdirPath.toString());
		resultBuilder.add(configSummary.get(SummaryDatum.TEST_ITERATION).toString());
		SUMMARY_DATA_TO_WRITE.stream().map(configSummary::get).map(Object::toString).forEachOrdered(resultBuilder);
		return resultBuilder.build();
	}

	private static Stream<String> createTestMethodColumnHeaders() {
		final Stream.Builder<String> resultBuilder = Stream.builder();
		resultBuilder.add(UtteranceFiltering.class.getSimpleName());
		final String cleaningMethodPrefix = Cleaning.class.getSimpleName() + "-";
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
		resultBuilder.add(testParams.getUttFiltering().toString());
		final Set<Cleaning> cleaningMethods = testParams.getCleaning();
		cleaningMethodReprFactory.apply(cleaningMethods).forEachOrdered(resultBuilder);
		resultBuilder.add(testParams.getTokenization().toString());
		resultBuilder.add(testParams.getTokenType().toString());
		resultBuilder.add(testParams.getTokenFiltering().toString());
		resultBuilder.add(testParams.getTrainingMethod().toString());
		return resultBuilder.build();
	}

	private static void shutdownExceptionally(final ExecutorService executor) {
		LOGGER.debug("Emergency executor service shutdown.");
		executor.shutdownNow();
		LOGGER.debug("Successfully shut down executor service.");
	}

	private boolean createNewSummary;

	private final Path outdir;

	private final Path summaryFile;

	public CombiningBatchJobTestMultiDirWriter(final Path outdir, final boolean createNewSummary) {
		this.outdir = outdir;
		this.createNewSummary = createNewSummary;

		summaryFile = outdir.resolve("batch-summary.tsv");
	}

	public void write(final BatchJobSummary summary) {
		final TestParameters testParams = summary.getTestParams();
		try {
			final Path batchOutdir = Files.createDirectories(createBatchOutdir(testParams));
			LOGGER.info("Will write results of testing {} to \"{}\".", testParams, batchOutdir);
			final BatchTestResultWriter testWriter = new BatchTestResultWriter(batchOutdir);
			final Tester.Result testResults = summary.getTestResults();
			testWriter.accept(testResults);
			writeSummary(summary, batchOutdir);
		} catch (final IOException e) {
			throw new UncheckedIOException(e);
		}
	}

	public void writeError(final IncompleteResults incompleteResults, final Throwable thrown) {
		if (thrown instanceof OutOfMemoryError) {
			Runtime.getRuntime().gc();
		}
		try {
			if (createNewSummary) {
				writeInitialSummaryFile();
				createNewSummary = false;
			}
			final Path batchOutdir = Files.createDirectories(createBatchOutdir(incompleteResults.getTestParams()));
			final String errorDesc = String.format("%s: %s", thrown.getClass().getName(), thrown.getLocalizedMessage());
			try (final BufferedWriter summaryWriter = createAppendingSummaryWriter()) {
				summaryWriter.newLine();
				final Stream<String> rowCellVals = createRowCellValues(incompleteResults, errorDesc, batchOutdir);
				summaryWriter.write(rowCellVals.collect(ROW_CELL_JOINER));
			}
		} catch (final IOException e) {
			throw new UncheckedIOException(e);
		}
	}

	private BufferedWriter createAppendingSummaryWriter() throws IOException {
		return Files.newBufferedWriter(summaryFile, StandardOpenOption.APPEND);
	}

	private Path createBatchOutdir(final TestParameters testParams) {
		final String batchOutdirName = createBatchOutdirName(testParams);
		return outdir.resolve(batchOutdirName);
	}

	private void writeInitialSummaryFile() throws IOException {
		Files.createDirectories(summaryFile.getParent());
		try (final BufferedWriter summaryWriter = Files.newBufferedWriter(summaryFile, StandardOpenOption.CREATE,
				StandardOpenOption.TRUNCATE_EXISTING)) {
			summaryWriter.write(COL_HEADERS.stream().collect(ROW_CELL_JOINER));
		}
	}

	private void writeSummary(final BatchJobSummary summary, final Path batchOutdir) throws IOException {
		if (createNewSummary) {
			writeInitialSummaryFile();
			createNewSummary = false;
		}
		try (final BufferedWriter summaryWriter = createAppendingSummaryWriter()) {
			summaryWriter.newLine();
			final Stream<String> rowCellVals = createRowCellValues(summary, batchOutdir);
			summaryWriter.write(rowCellVals.collect(ROW_CELL_JOINER));
		}
	}

}
