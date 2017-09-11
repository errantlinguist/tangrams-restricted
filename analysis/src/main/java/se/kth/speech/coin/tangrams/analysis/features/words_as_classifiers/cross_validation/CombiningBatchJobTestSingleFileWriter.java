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
import java.util.Arrays;
import java.util.EnumMap;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.OptionalInt;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ForkJoinPool;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import se.kth.speech.coin.tangrams.CLIParameters;
import se.kth.speech.coin.tangrams.analysis.DataLanguageDefaults;
import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.coin.tangrams.analysis.dialogues.UtteranceDialogueRepresentationStringFactory;
import se.kth.speech.coin.tangrams.analysis.features.weka.EntityInstanceAttributeContext;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.EventDialogueTestResults;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.cross_validation.CombiningBatchJobTester.IncompleteResults;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.cross_validation.Tester.CrossValidationTestSummary;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 24 May 2017
 *
 */
final class CombiningBatchJobTestSingleFileWriter { // NO_UCD (unused code)

	private static final List<DialogueAnalysisSummaryFactory.SummaryDatum> DEFAULT_DATA_TO_WRITE = createDefaultDatumOrderingList();

	private static final String EXTRACTION_LOG_FILE_SUFFIX = ".extraction.tsv";

	private static final Logger LOGGER = LoggerFactory.getLogger(CombiningBatchJobTestSingleFileWriter.class);

	private static final String NULL_CELL_VALUE_REPR = "?";

	private static final Options OPTIONS = createOptions();

	private static final Charset OUTPUT_ENCODING = StandardCharsets.UTF_8;

	private static final Collector<CharSequence, ?, String> ROW_CELL_JOINER = Collectors.joining("\t");

	private static final String UTT_REL_LOG_FILE_SUFFIX = ".uttrels.tsv";

	public static void main(final String[] args) throws Exception {
		final CommandLineParser parser = new DefaultParser();
		try {
			final CommandLine cl = parser.parse(OPTIONS, args);
			main(cl);
		} catch (final ParseException e) {
			System.out.println(String.format("An error occured while parsing the command-line arguments: %s", e));
			printHelp();
		}
	}

	private static ExecutorService createBackgroundJobExecutor() {
		return ForkJoinPool.commonPool();
	}

	private static List<DialogueAnalysisSummaryFactory.SummaryDatum> createDefaultDatumOrderingList() {
		final List<DialogueAnalysisSummaryFactory.SummaryDatum> result = Arrays.asList(
				DialogueAnalysisSummaryFactory.SummaryDatum.DYAD,
				DialogueAnalysisSummaryFactory.SummaryDatum.DESCRIPTION,
				DialogueAnalysisSummaryFactory.SummaryDatum.SESSION_ORDER,
				DialogueAnalysisSummaryFactory.SummaryDatum.EVENT_TIME,
				DialogueAnalysisSummaryFactory.SummaryDatum.TEST_ITER,
				// DialogueAnalysisSummaryFactory.SummaryDatum.DIALOGUE,
				// DialogueAnalysisSummaryFactory.SummaryDatum.DIALOGUE_AS_TESTED,
				// DialogueAnalysisSummaryFactory.SummaryDatum.GOLD_STD_ID,
				DialogueAnalysisSummaryFactory.SummaryDatum.RANK,
				DialogueAnalysisSummaryFactory.SummaryDatum.TESTED_UTT_COUNT,
				DialogueAnalysisSummaryFactory.SummaryDatum.TOTAL_UTT_COUNT,
				DialogueAnalysisSummaryFactory.SummaryDatum.TOKEN_COUNT);
		assert result.size() <= DialogueAnalysisSummaryFactory.SummaryDatum.values().length;
		return result;
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
		Arrays.stream(CLITestParameter.values()).map(CLITestParameter::get).forEach(result::addOption);
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

	private static void main(final CommandLine cl) throws Exception {
		if (cl.hasOption(CLITestParameter.HELP.optName)) {
			printHelp();
		} else {
			final ExecutorService backgroundJobExecutor = createBackgroundJobExecutor();
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

				final File outFile = (File) cl.getParsedOptionValue(CLITestParameter.OUTPATH.optName);

				try (PrintWriter out = CLIParameters.parseOutpath(outFile)) {
					final CombiningBatchJobTestSingleFileWriter writer = new CombiningBatchJobTestSingleFileWriter(out,
							true);

					try (BufferedWriter extrLogOut = createExtrLogFileWriter(outFile)) {
						try (BufferedWriter uttRelLogOut = createUttRelFileWriter(outFile)) {

							try (final ClassPathXmlApplicationContext appCtx = new ClassPathXmlApplicationContext(
									"combining-batch-tester.xml", CombiningBatchJobTestSingleFileWriter.class)) {
								final CombiningBatchJobTester tester = new CombiningBatchJobTester(
										backgroundJobExecutor, appCtx, writer::write, writer::writeError,
										testerConfigurator, new ExtractionLogWriter(extrLogOut),
										new UtteranceRelationLogWriter(uttRelLogOut, NULL_CELL_VALUE_REPR));
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
				throw e;
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
		final Tester.Result testResults = summary.getTestResults();
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
									sessionDialogueOrder++, diagTestResults));
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

	public void writeError(final IncompleteResults incompleteResults, final Throwable thrown) {
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
		throw new RuntimeException(thrown);
	}

}
