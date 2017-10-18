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
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.EnumMap;
import java.util.EnumSet;
import java.util.List;
import java.util.Map;
import java.util.OptionalInt;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.function.Consumer;
import java.util.function.Supplier;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import se.kth.speech.coin.tangrams.CLIParameters;
import se.kth.speech.coin.tangrams.analysis.BackgroundJobs;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.cross_validation.CombiningBatchJobTester.IncompleteResults;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.cross_validation.StatisticsWriter.SummaryDatum;
import se.kth.speech.coin.tangrams.iristk.EventTimes;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 24 May 2017
 *
 */
final class CombiningBatchJobTestMultiDirWriter { // NO_UCD (use default)

	private enum Parameter implements Supplier<Option> {
		APPEND_SUMMARY("a") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("append")
						.desc("If this flag is present, the summary file is appended to rather than being truncated and written anew.")
						.build();
			}
		},
		NO_CLOBBER("nc") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("append")
						.desc("If this flag is present, existing batch result directories will not be overwritten.")
						.build();
			}
		};

		private static final Options OPTIONS = createOptions();

		private static Options createOptions() {
			final Options result = new Options();
			Arrays.stream(CLITestParameter.values()).map(CLITestParameter::get).forEach(result::addOption);
			Arrays.stream(Parameter.values()).map(Parameter::get).forEach(result::addOption);
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

	private static final Collector<CharSequence, ?, String> BATCH_DIR_METHOD_NAME_JOINER = Collectors.joining(",");

	private static final List<String> COL_HEADERS;

	private static final Logger LOGGER = LoggerFactory.getLogger(CombiningBatchJobTestMultiDirWriter.class);

	private static final String NULL_CELL_VALUE_REPR = "?";

	private static final Charset OUTPUT_ENCODING = StandardCharsets.UTF_8;

	private static final Collector<CharSequence, ?, String> ROW_CELL_JOINER = Collectors.joining("\t");

	private static final List<SummaryDatum> SUMMARY_DATA_TO_WRITE;

	private static final DateTimeFormatter TIMESTAMP_FORMATTER = EventTimes.FORMATTER;

	static {
		SUMMARY_DATA_TO_WRITE = Arrays.asList(SummaryDatum.MEAN_RANK, SummaryDatum.MRR, SummaryDatum.DIALOGUE_COUNT,
				SummaryDatum.UTTERANCES_TESTED, SummaryDatum.MEAN_UTTERANCES_PER_DIALOGUE);
		COL_HEADERS = createColHeaderList(SUMMARY_DATA_TO_WRITE);
	}

	public static void main(final String[] args) throws BatchJobTestException {
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
		final Stream<String> rowCellVals = TestParameterReporting.createTestMethodRowCellValues(testParams,
				CombiningBatchJobTestMultiDirWriter::createCleaningMethodSetValues);
		return rowCellVals.collect(BATCH_DIR_METHOD_NAME_JOINER);
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
		return downcast.stream().map(Cleaning::toString);
	}

	private static List<String> createColHeaderList(final List<SummaryDatum> summaryDataToWrite) {
		final Stream.Builder<String> resultBuilder = Stream.builder();
		resultBuilder.add("TIME");
		resultBuilder.accept("DESC");
		TestParameterReporting.createTestMethodColumnHeaders().forEachOrdered(resultBuilder);
		resultBuilder.add("OUTDIR");
		resultBuilder.add("ITER_COUNT");
		summaryDataToWrite.stream().map(SummaryDatum::toString).forEachOrdered(resultBuilder);
		return Arrays.asList(resultBuilder.build().toArray(String[]::new));
	}

	private static Stream<String> createRowCellValues(final BatchJobSummary summary, final Path outdirPath) {
		final Stream.Builder<String> resultBuilder = Stream.builder();
		resultBuilder.add(TIMESTAMP_FORMATTER.format(summary.getTestTimestamp()));
		resultBuilder.add("Success");
		TestParameterReporting.createTestMethodRowCellValues(summary.getTestParams(),
				TestParameterReporting::createCleaningMethodBooleanValues).forEachOrdered(resultBuilder);
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
		TestParameterReporting.createTestMethodRowCellValues(incompleteResults.getTestParams(),
				TestParameterReporting::createCleaningMethodBooleanValues).forEachOrdered(resultBuilder);
		final Map<SummaryDatum, Object> configSummary = new EnumMap<>(SummaryDatum.class);
		Arrays.stream(SummaryDatum.values()).forEach(datum -> configSummary.put(datum, NULL_CELL_VALUE_REPR));
		resultBuilder.add(outdirPath.toString());
		resultBuilder.add(configSummary.get(SummaryDatum.TEST_ITERATION).toString());
		SUMMARY_DATA_TO_WRITE.stream().map(configSummary::get).map(Object::toString).forEachOrdered(resultBuilder);
		return resultBuilder.build();
	}

	private static void main(final CommandLine cl) throws BatchJobTestException {
		if (cl.hasOption(CLITestParameter.HELP.optName)) {
			Parameter.printHelp();
		} else {
			final ExecutorService backgroundJobExecutor = BackgroundJobs.getBackgroundJobExecutor();
			final CombiningBatchJobTesterCLIInputFactory inputFactory = new CombiningBatchJobTesterCLIInputFactory(
					backgroundJobExecutor);
			try {
				final CombiningBatchJobTester.Input input = inputFactory.apply(cl);
				final Consumer<Tester> testerConfigurator;
				{
					final OptionalInt optIterCount = CLIParameters
							.parseIterCount((Number) cl.getParsedOptionValue(CLITestParameter.ITER_COUNT.optName));
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

				final Path outdir = ((File) cl.getParsedOptionValue(CLITestParameter.OUTPATH.optName)).toPath();
				LOGGER.info("Will write data to \"{}\".", outdir);
				final boolean appendSummary = cl.hasOption(Parameter.APPEND_SUMMARY.optName);
				LOGGER.info("Append to summary rather than truncate? {}", appendSummary);
				final boolean noClobber = cl.hasOption(Parameter.NO_CLOBBER.optName);
				LOGGER.info("Don't clobber old batch results? {}", noClobber);
				// TODO: Finish "noclobber" opt impl

				final CombiningBatchJobTestMultiDirWriter writer = new CombiningBatchJobTestMultiDirWriter(outdir,
						!appendSummary);
				try (final ClassPathXmlApplicationContext appCtx = new ClassPathXmlApplicationContext(
						"combining-batch-tester.xml", CombiningBatchJobTestMultiDirWriter.class)) {
					final CombiningBatchJobTester tester = new CombiningBatchJobTester(backgroundJobExecutor, appCtx,
							writer::write, writer::writeError, testerConfigurator, (sent, extractedPhrases) -> {
								// Do nothing
							}, (evtDiag, uttRels) -> {
								// Do nothing
							});
					tester.accept(input);
				}
				LOGGER.info("Shutting down executor service.");
				backgroundJobExecutor.shutdown();
				LOGGER.info("Successfully shut down executor service.");

			} catch (final Exception e) {
				shutdownExceptionally(backgroundJobExecutor);
				throw new BatchJobTestException(e);
			}
		}
	}

	private static void shutdownExceptionally(final ExecutorService executor) {
		LOGGER.debug("Emergency executor service shutdown.");
		executor.shutdownNow();
		LOGGER.debug("Successfully shut down executor service.");
	}

	private boolean createNewSummary;

	private final Path outdir;

	private final Path summaryFile;

	private CombiningBatchJobTestMultiDirWriter(final Path outdir, final boolean createNewSummary) {
		this.outdir = outdir;
		this.createNewSummary = createNewSummary;

		summaryFile = outdir.resolve("batch-summary.tsv");
	}

	private BufferedWriter createAppendingSummaryWriter() throws IOException {
		return Files.newBufferedWriter(summaryFile, OUTPUT_ENCODING, StandardOpenOption.APPEND);
	}

	private Path createBatchOutdir(final TestParameters testParams) {
		final String batchOutdirName = createBatchOutdirName(testParams);
		return outdir.resolve(batchOutdirName);
	}

	private void write(final BatchJobSummary summary) {
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

	private void writeError(final IncompleteResults incompleteResults, final Throwable thrown) {
		LOGGER.error(
				String.format("An error occurred while running test which was started at \"%s\".",
						TestParameterReporting.TIMESTAMP_FORMATTER.format(incompleteResults.getTestStartTime())),
				thrown);
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

	private void writeInitialSummaryFile() throws IOException {
		final Path parent = summaryFile.getParent();
		if (parent != null) {
			Files.createDirectories(parent);
		}
		try (final BufferedWriter summaryWriter = Files.newBufferedWriter(summaryFile, OUTPUT_ENCODING,
				StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)) {
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
