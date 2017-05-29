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
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.OptionalInt;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.function.Consumer;
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

import se.kth.speech.coin.tangrams.analysis.SessionDataManager;
import se.kth.speech.coin.tangrams.analysis.features.ClassificationException;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.cross_validation.StatisticsWriter.SummaryDatum;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 24 May 2017
 *
 */
public final class CombininingBatchJobTestWriter implements Consumer<BatchJobSummary> {

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
			formatter.printHelp(CombininingBatchJobTestWriter.class.getSimpleName() + " INPATHS...", OPTIONS);
		}

		protected final String optName;

		private Parameter(final String optName) {
			this.optName = optName;
		}

	}

	private static final List<String> COL_HEADERS;

	private static final List<SummaryDatum> SUMMARY_DATA_TO_WRITE;

	static {
		SUMMARY_DATA_TO_WRITE = Arrays.asList(SummaryDatum.MEAN_RANK, SummaryDatum.MRR, SummaryDatum.DIALOGUE_COUNT,
				SummaryDatum.UTTERANCES_TESTED, SummaryDatum.MEAN_UTTERANCES_PER_DIALOGUE);
		COL_HEADERS = createColHeaderList(SUMMARY_DATA_TO_WRITE);
	}

	private static final Logger LOGGER = LoggerFactory.getLogger(CombininingBatchJobTestWriter.class);

	private static final Collector<CharSequence, ?, String> ROW_CELL_JOINER = Collectors.joining("\t");

	private static final Collector<CharSequence, ?, String> METHOD_KEY_NAME_JOINER = Collectors.joining("_");

	public static void main(final CommandLine cl)
			throws ParseException, InterruptedException, ExecutionException, ClassificationException, IOException {
		if (cl.hasOption(Parameter.HELP.optName)) {
			Parameter.printHelp();
		} else {
			final List<Path> inpaths = Arrays.asList(cl.getArgList().stream().map(Paths::get).toArray(Path[]::new));
			if (inpaths.isEmpty()) {
				throw new MissingOptionException("No input path(s) specified.");

			} else {
				final Consumer<Tester> testerConfigurator;
				{
					final OptionalInt optIterCount = Parameter.parseIterCount(cl);
					if (optIterCount.isPresent()) {
						final int iterCount = optIterCount.getAsInt();
						LOGGER.info("Will set iteration count for all tests to {}.", iterCount);
						testerConfigurator = tester -> tester.setIterCount(iterCount);
					} else {
						testerConfigurator = tester -> {
							// Do nothing
						};
					}
				}
				final Path outdir = ((File) cl.getParsedOptionValue(Parameter.OUTPATH.optName)).toPath();
				LOGGER.info("Will write data to \"{}\".", outdir);

				final ExecutorService executor = createExecutorService();
				try {
					final Future<Map<SessionDataManager, Path>> allSessionDataFuture = executor
							.submit(() -> TestSessionData.readTestSessionData(inpaths));
					final CombininingBatchJobTestWriter writer = new CombininingBatchJobTestWriter(outdir);
					try (final ClassPathXmlApplicationContext appCtx = new ClassPathXmlApplicationContext(
							"combining-batch-tester.xml", CombininingBatchJobTestWriter.class)) {
						final CombiningBatchJobTester tester = new CombiningBatchJobTester(executor, appCtx, writer,
								testerConfigurator);
						final CombiningBatchJobTester.Input input = new CombiningBatchJobTester.Input(
								Arrays.asList(UtteranceFiltering.values()), Arrays.asList(Tokenization.values()),
								Arrays.asList(TokenFiltering.values()), Arrays.asList(Training.values()),
								allSessionDataFuture.get());
						tester.accept(input);
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

	private static String createBatchOutdirName(final TestParameters testParams) {
		final Stream<String> rowCellVals = createRowCellValues(testParams);
		return rowCellVals.collect(METHOD_KEY_NAME_JOINER);
	}

	private static List<String> createColHeaderList(final List<SummaryDatum> summaryDataToWrite) {
		final Stream.Builder<String> colHeaderStreamBuilder = Stream.builder();
		createColumnHeaders().forEachOrdered(colHeaderStreamBuilder);
		colHeaderStreamBuilder.add("OUTDIR");
		colHeaderStreamBuilder.add("ITER_COUNT");
		summaryDataToWrite.stream().map(SummaryDatum::toString).forEachOrdered(colHeaderStreamBuilder);
		return Arrays.asList(colHeaderStreamBuilder.build().toArray(String[]::new));
	}

	private static Stream<String> createColumnHeaders() {
		return Stream.of(UtteranceFiltering.class.getSimpleName(), Tokenization.class.getSimpleName(),
				TokenFiltering.class.getSimpleName(), Training.class.getSimpleName());
	}

	private static ExecutorService createExecutorService() {
		return createExecutorService(Math.max(Runtime.getRuntime().availableProcessors() - 1, 1));
	}

	private static ExecutorService createExecutorService(final int parallelThreadCount) {
		LOGGER.info("Will run with {} parallel thread(s).", parallelThreadCount);
		return Executors.newFixedThreadPool(parallelThreadCount);
	}

	private static Stream<String> createRowCellValues(final BatchJobSummary summary, final Path outdirPath) {
		final Stream.Builder<String> resultBuilder = Stream.builder();
		createRowCellValues(summary.getTestParams()).forEachOrdered(resultBuilder);
		final Map<SummaryDatum, Object> configSummary = StatisticsWriter.createSummaryDataMap(null,
				summary.getTestResults());
		resultBuilder.add(outdirPath.toString());
		resultBuilder.add(configSummary.get(SummaryDatum.TEST_ITERATION).toString());
		SUMMARY_DATA_TO_WRITE.stream().map(configSummary::get).map(Object::toString).forEachOrdered(resultBuilder);
		return resultBuilder.build();
	}

	private static Stream<String> createRowCellValues(final TestParameters testParams) {
		final Stream<HasKeyName> nameable = Stream.of(testParams.getUttFilteringMethod(),
				testParams.getTokenizationMethod(), testParams.getTokenFilteringMethod(),
				testParams.getTrainingMethod());
		return nameable.map(HasKeyName::getKeyName);
	}

	private static void shutdownExceptionally(final ExecutorService executor) {
		LOGGER.debug("Emergency executor service shutdown.");
		executor.shutdownNow();
		LOGGER.debug("Successfully shut down executor service.");
	}

	private final Path outdir;

	private final Path summaryFile;

	private boolean isRunning;

	public CombininingBatchJobTestWriter(final Path outdir) {
		this.outdir = outdir;
		summaryFile = outdir.resolve("batch-summary.tsv");
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Consumer#accept(java.lang.Object)
	 */
	@Override
	public void accept(final BatchJobSummary summary) {
		final TestParameters testParams = summary.getTestParams();
		final String batchOutdirName = createBatchOutdirName(testParams);
		try {
			final Path batchOutdir = Files.createDirectories(outdir.resolve(batchOutdirName));
			LOGGER.info("Will write results of testing {} to \"{}\".", testParams, batchOutdir);
			final BatchTestResultWriter testWriter = new BatchTestResultWriter(batchOutdir);
			final Tester.Result testResults = summary.getTestResults();
			testWriter.apply(testResults);
			writeSummary(summary, batchOutdir);
		} catch (final IOException e) {
			throw new UncheckedIOException(e);
		}
	}

	private void writeInitialSummaryFile() throws IOException {
		Files.createDirectories(summaryFile.getParent());
		try (final BufferedWriter summaryWriter = Files.newBufferedWriter(summaryFile, StandardOpenOption.CREATE,
				StandardOpenOption.TRUNCATE_EXISTING)) {
			summaryWriter.write(COL_HEADERS.stream().collect(ROW_CELL_JOINER));
		}
	}

	private void writeSummary(final BatchJobSummary summary, final Path batchOutdir) throws IOException {
		if (!isRunning) {
			writeInitialSummaryFile();
			isRunning = true;
		}
		try (final BufferedWriter summaryWriter = Files.newBufferedWriter(summaryFile, StandardOpenOption.APPEND)) {
			summaryWriter.newLine();
			final Stream<String> rowCellVals = createRowCellValues(summary, batchOutdir);
			summaryWriter.write(rowCellVals.collect(ROW_CELL_JOINER));
		}
	}

}
