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

import java.io.File;
import java.io.PrintWriter;
import java.nio.file.Path;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.EnumMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.OptionalInt;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ForkJoinPool;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
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

import it.unimi.dsi.fastutil.objects.Object2IntMap;
import se.kth.speech.coin.tangrams.CLIParameters;
import se.kth.speech.coin.tangrams.analysis.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.features.weka.EntityInstanceAttributeContext;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.EventDialogueTestResults;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.cross_validation.CombiningBatchJobTester.IncompleteResults;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.cross_validation.Tester.CrossValidationTestSummary;
import se.kth.speech.coin.tangrams.iristk.EventTimes;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 24 May 2017
 *
 */
public final class CombiningBatchJobTestSingleFileWriter {

	private static final List<DialogueAnalysisSummaryFactory.SummaryDatum> DEFAULT_DATA_TO_WRITE = createDefaultDatumOrderingList();

	private static final Logger LOGGER = LoggerFactory.getLogger(CombiningBatchJobTestSingleFileWriter.class);

	private static final String NULL_CELL_VALUE_REPR = "?";

	private static final Options OPTIONS = createOptions();

	private static final Collector<CharSequence, ?, String> ROW_CELL_JOINER = Collectors.joining("\t");

	/**
	 * <strong>NOTE:</strong> This is for SPSS compability, which does not allow
	 * e.g.&nbsp;<code>"-"</code> as part of a variable name.
	 *
	 * @see <a href=
	 *      "https://www.ibm.com/support/knowledgecenter/en/SSLVMB_21.0.0/com.ibm.spss.statistics.help/syn_variables_variable_names.htm">SPSS
	 *      documentation</a>
	 */
	private static final String SUBCOL_NAME_DELIM = "#";

	private static final DateTimeFormatter TIMESTAMP_FORMATTER = EventTimes.FORMATTER;

	public static void main(final CommandLine cl) throws Exception {
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

				try (PrintWriter out = CLIParameters
						.parseOutpath((File) cl.getParsedOptionValue(CLITestParameter.OUTPATH.optName))) {
					final CombiningBatchJobTestSingleFileWriter writer = new CombiningBatchJobTestSingleFileWriter(out,
							true);
					try (final ClassPathXmlApplicationContext appCtx = new ClassPathXmlApplicationContext(
							"combining-batch-tester.xml", CombiningBatchJobTestSingleFileWriter.class)) {
						final CombiningBatchJobTester tester = new CombiningBatchJobTester(backgroundJobExecutor,
								appCtx, writer::write, writer::writeError, testerConfigurator);
						tester.accept(input);
					}
					LOGGER.info("Shutting down executor service.");
					backgroundJobExecutor.shutdown();
					LOGGER.info("Successfully shut down executor service.");

				}
			} catch (final Exception e) {
				shutdownExceptionally(backgroundJobExecutor);
				throw e;
			}
		}
	}

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

	private static Stream<String> createCleaningMethodBooleanValues(final Set<Cleaning> cleaningMethods) {
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

	private static List<DialogueAnalysisSummaryFactory.SummaryDatum> createDefaultDatumOrderingList() {
		final List<DialogueAnalysisSummaryFactory.SummaryDatum> result = Arrays.asList(
				DialogueAnalysisSummaryFactory.SummaryDatum.KEY, DialogueAnalysisSummaryFactory.SummaryDatum.TEST_ITER,
				DialogueAnalysisSummaryFactory.SummaryDatum.DESCRIPTION,
				DialogueAnalysisSummaryFactory.SummaryDatum.SESSION_ORDER,
				DialogueAnalysisSummaryFactory.SummaryDatum.EVENT_TIME,
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

	private static Options createOptions() {
		final Options result = new Options();
		Arrays.stream(CLITestParameter.values()).map(CLITestParameter::get).forEach(result::addOption);
		return result;
	}

	private static Stream<String> createTestMethodColumnHeaders() {
		final Stream.Builder<String> resultBuilder = Stream.builder();
		resultBuilder.add(UtteranceFiltering.class.getSimpleName());
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
		resultBuilder.add(testParams.getUttFiltering().toString());
		final Set<Cleaning> cleaningMethods = testParams.getCleaning();
		cleaningMethodReprFactory.apply(cleaningMethods).forEachOrdered(resultBuilder);
		resultBuilder.add(testParams.getTokenization().toString());
		resultBuilder.add(testParams.getTokenType().toString());
		resultBuilder.add(testParams.getTokenFiltering().toString());
		resultBuilder.add(testParams.getTrainingMethod().toString());
		return resultBuilder.build();
	}

	/**
	 * @return
	 */
	private static Stream<String> createTrainingDataColHeaders() {
		return EntityInstanceAttributeContext.getClassValues().stream()
				.map(classVal -> "TRAIN_INSTS" + SUBCOL_NAME_DELIM + classVal);
	}

	private static Stream<Object> createTrainingDataRowCellValues(final CrossValidationTestSummary cvTestSummary) {
		final Object2IntMap<String> trainingInstCounts = cvTestSummary.getTrainingInstanceCounts();
		return EntityInstanceAttributeContext.getClassValues().stream().map(trainingInstCounts::getInt);
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

	public CombiningBatchJobTestSingleFileWriter(final PrintWriter out, final boolean writeHeader) {
		this(out, writeHeader, DEFAULT_DATA_TO_WRITE);
	}

	public CombiningBatchJobTestSingleFileWriter(final PrintWriter out, final boolean writeHeader,
			final List<DialogueAnalysisSummaryFactory.SummaryDatum> dataToWrite) {
		this.out = out;
		this.writeHeader = writeHeader;
		this.dataToWrite = dataToWrite;
		rowDataFactory = new DialogueAnalysisSummaryFactory(dataToWrite);
	}

	public void write(final BatchJobSummary summary) {
		if (writeHeader) {
			out.println(createColHeaders(dataToWrite).collect(ROW_CELL_JOINER));
			writeHeader = false;
		}
		final String[] testParamRowCellValues = createTestParamRowCellValues(summary).toArray(String[]::new);
		final Tester.Result testResults = summary.getTestResults();
		for (final Entry<Path, List<CrossValidationTestSummary>> infileSessionResults : testResults.getSessionResults()
				.entrySet()) {
			final Path inpath = infileSessionResults.getKey();
			final List<CrossValidationTestSummary> sessionResultList = infileSessionResults.getValue();
			for (final ListIterator<CrossValidationTestSummary> sessionResultIter = sessionResultList
					.listIterator(); sessionResultIter.hasNext();) {
				final CrossValidationTestSummary cvTestSummary = sessionResultIter.next();
				final String[] trainingRowCellVals = createTrainingDataRowCellValues(cvTestSummary)
						.map(Object::toString).toArray(String[]::new);
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
		final String errorDesc = String.format("%s: %s", thrown.getClass().getName(), thrown.getLocalizedMessage());

		final Stream.Builder<String> rowCellValBuilder = Stream.builder();
		rowCellValBuilder.add(TIMESTAMP_FORMATTER.format(incompleteResults.getTestStartTime()));
		createTestMethodRowCellValues(incompleteResults.getTestParams(),
				CombiningBatchJobTestSingleFileWriter::createCleaningMethodBooleanValues)
						.forEachOrdered(rowCellValBuilder);
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
	}

	private Stream<String> createTestParamRowCellValues(final BatchJobSummary summary) {
		final Stream.Builder<String> resultBuilder = Stream.builder();
		resultBuilder.add(TIMESTAMP_FORMATTER.format(summary.getTestTimestamp()));
		createTestMethodRowCellValues(summary.getTestParams(),
				CombiningBatchJobTestSingleFileWriter::createCleaningMethodBooleanValues).forEachOrdered(resultBuilder);
		return resultBuilder.build();
	}

}
