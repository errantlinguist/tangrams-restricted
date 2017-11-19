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
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.EnumMap;
import java.util.EnumSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Random;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executor;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.annotation.Nullable;
import javax.annotation.concurrent.ThreadSafe;
import javax.xml.bind.JAXBException;

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

import com.google.common.collect.Maps;

import it.unimi.dsi.fastutil.objects.Object2IntMap;
import se.kth.speech.coin.tangrams.analysis.BackgroundJobs;
import se.kth.speech.coin.tangrams.analysis.DataLanguageDefaults;
import se.kth.speech.coin.tangrams.analysis.SessionGameManager;
import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.coin.tangrams.analysis.dialogues.UtteranceDialogueRepresentationStringFactory;
import se.kth.speech.coin.tangrams.analysis.features.weka.EntityInstanceAttributeContext;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.EventDialogueTestResults;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.cross_validation.CrossValidator.CrossValidationTestSummary;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues.MappingEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.TrainingInstancesFactory;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.WordClassificationData;
import se.kth.speech.coin.tangrams.analysis.io.SessionDataManager;
import se.kth.speech.coin.tangrams.iristk.EventTimes;
import se.kth.speech.coin.tangrams.iristk.io.LoggedEventReader;
import se.kth.speech.io.PrintWriterFetcher;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 24 May 2017
 *
 */
@ThreadSafe
final class ParameterFileReadingTestWriter { // NO_UCD (unused code)
	private static class DiscountingTestSetFactoryFactory implements TestSetFactoryFactory {

		private final Random random;

		private final Map<WordClassifierTrainingParameter, Object> trainingParams;

		private DiscountingTestSetFactoryFactory(final Map<WordClassifierTrainingParameter, Object> trainingParams,
				final Random random) {
			this.trainingParams = trainingParams;
			this.random = random;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.util.function.BiFunction#apply(java.lang.Object, java.lang.Object)
		 */
		@Override
		public Function<Map<SessionDataManager, Path>, Stream<Entry<SessionDataManager, WordClassificationData>>> apply(
				final TrainingInstancesFactory trainingInstsFactory,
				final Map<SessionDataManager, SessionGameManager> sessionGameMgrs) {
			final Function<Map<SessionDataManager, Path>, Stream<Entry<SessionDataManager, WordClassificationData>>> result;
			final int trainingSetSizeDiscountingConstant = (Integer) trainingParams
					.get(WordClassifierTrainingParameter.TRAINING_SET_SIZE_DISCOUNTING_CONSTANT);
			if (trainingSetSizeDiscountingConstant == 0) {
				result = new TestSetFactory(trainingInstsFactory, sessionGameMgrs);
			} else {
				result = new RandomDiscountingTestSetFactory(trainingInstsFactory, sessionGameMgrs::get, random,
						trainingSetSizeDiscountingConstant);
			}
			return result;
		}

	}

	private enum Parameter implements Supplier<Option> {
		HELP("?") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("help").desc("Prints this message.").build();
			}
		},
		OUTDIR("o") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("outpath").desc("The directory to write the extracted data to.")
						.hasArg().argName("path").type(File.class).required().build();
			}
		},
		TOKENIZATION_FILE_PATH("tok") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("tokenization-file").desc(
						"A path to the file mapping token sequences to the relevant referring-language tokens each represent.")
						.hasArg().argName("path").type(File.class).required().build();
			}
		},
		TRAINING("tr") {
			@Override
			public Option get() {
				final Training[] possibleVals = Training.values();
				return Option.builder(optName).longOpt("training")
						.desc("The training method to use. Possible values: " + Arrays.toString(possibleVals)).hasArg()
						.argName("name").required().build();
			}
		},
		WORD_CLASSIFIER_TRAINING_PARAMS("p") {

			@Override
			public Option get() {
				return Option.builder(optName).longOpt("tokenization-file").desc(String.format(
						"A path to the tabular file containing the different combinations of %s values to use for cross-validation.",
						WordClassifierTrainingParameter.class.getSimpleName())).hasArg().argName("path")
						.type(File.class).required().build();
			}

		};

		private static Training parseTrainingMethod(final CommandLine cl) {
			final String name = cl.getOptionValue(Parameter.TRAINING.optName);
			return Training.valueOf(name);
		}

		protected final String optName;

		private Parameter(final String optName) {
			this.optName = optName;
		}
	}

	/**
	 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
	 * @since 19 Nov 2017
	 *
	 */
	private static class ParameterCombinationTester {

		private final ApplicationContext appCtx;

		private final Executor backgroundJobExecutor;

		private final Map<WordClassifierTrainingParameter, Object> trainingParams;

		private final ParameterFileReadingTestWriter writer;

		private ParameterCombinationTester(final ParameterFileReadingTestWriter writer,
				final Map<WordClassifierTrainingParameter, Object> trainingParams, final ApplicationContext appCtx,
				final Executor backgroundJobExecutor) {
			this.writer = writer;
			this.trainingParams = trainingParams;
			this.appCtx = appCtx;
			this.backgroundJobExecutor = backgroundJobExecutor;
		}

		void accept(final UtteranceMappingBatchJobTester.Input input) {
			// Use the same Random instance for each random
			// iteration so that each iteration is at least
			// somewhat different from the others
			final Random random = new Random((Long) trainingParams.get(WordClassifierTrainingParameter.RANDOM_SEED));
			final int crossValidationIterCount = (Integer) trainingParams
					.get(WordClassifierTrainingParameter.CROSS_VALIDATION_ITERATION_COUNT);
			LOGGER.info("Will run {} cross-validation iteration(s).", crossValidationIterCount);
			for (int crossValidationIter = 1; crossValidationIter <= crossValidationIterCount; ++crossValidationIter) {
				final UtteranceMappingBatchJobTester tester = new UtteranceMappingBatchJobTester(backgroundJobExecutor,
						appCtx, writer::write, writer::writeError, UTT_REL_HANDLER, trainingParams,
						new DiscountingTestSetFactoryFactory(trainingParams, random));
				tester.accept(input);
				writer.setCrossValidationIterId(Integer.toString(crossValidationIter));
			}
			LOGGER.info("Finished performing {} cross-validations.", crossValidationIterCount);
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

	private static final Logger LOGGER = LoggerFactory.getLogger(ParameterFileReadingTestWriter.class);

	private static final String NULL_CELL_VALUE_REPR = "?";

	private static final Options OPTIONS = createOptions();

	private static final Charset OUTPUT_ENCODING = StandardCharsets.UTF_8;

	private static final Collector<CharSequence, ?, String> ROW_CELL_JOINER = Collectors.joining("\t");

	/**
	 * <strong>NOTE:</strong> This is for SPSS compatibility, which does not allow
	 * e.g.&nbsp;<code>"-"</code> as part of a variable name.
	 *
	 * @see <a href=
	 *      "https://www.ibm.com/support/knowledgecenter/en/SSLVMB_21.0.0/com.ibm.spss.statistics.help/syn_variables_variable_names.htm">SPSS
	 *      documentation</a>
	 */
	private static final String SUBCOL_NAME_DELIM = ".";

	private static final Charset TOKENIZATION_FILE_ENCODING = StandardCharsets.UTF_8;

	private static final Charset TRAINING_PARAM_INFILE_ENCODING = StandardCharsets.UTF_8;

	private static final BiConsumer<Object, Object> UTT_REL_HANDLER = (evtDiag, uttRels) -> {
		// Do nothing
	};

	private static final DateTimeFormatter TIMESTAMP_FORMATTER = EventTimes.FORMATTER;

	public static void main(final String[] args) throws IOException, CrossValidationTestException {
		final CommandLineParser parser = new DefaultParser();
		try {
			final CommandLine cl = parser.parse(OPTIONS, args);
			main(cl);
		} catch (final ParseException e) {
			System.out.println(String.format("An error occured while parsing the command-line arguments: %s", e));
			printHelp();
		}
	}

	private static Stream<String> createColHeaders(
			final List<DialogueAnalysisSummaryFactory.SummaryDatum> summaryDataToWrite) {
		final Stream.Builder<String> resultBuilder = Stream.builder();
		resultBuilder.add("TIME");
		resultBuilder.add("CROSS_VALID_ITER");
		createTestMethodColumnHeaders().forEachOrdered(resultBuilder);
		createTrainingDataColHeaders().forEachOrdered(resultBuilder);
		summaryDataToWrite.stream().map(DialogueAnalysisSummaryFactory.SummaryDatum::toString)
				.forEachOrdered(resultBuilder);
		return resultBuilder.build();
	}

	private static List<DialogueAnalysisSummaryFactory.SummaryDatum> createDefaultDatumOrderingList() {
		final Set<DialogueAnalysisSummaryFactory.SummaryDatum> excludedData = EnumSet.of(
				DialogueAnalysisSummaryFactory.SummaryDatum.DIALOGUE_AS_TESTED,
				DialogueAnalysisSummaryFactory.SummaryDatum.GOLD_STD_ID);
		return Arrays.asList(DialogueAnalysisSummaryFactory.getDefaultSummaryDatumOrdering().stream()
				.filter(datum -> !excludedData.contains(datum))
				.toArray(DialogueAnalysisSummaryFactory.SummaryDatum[]::new));
	}

	private static UtteranceMappingBatchJobTester.Input createInput(final CommandLine cl,
			final ExecutorService backgroundJobExecutor) throws IOException, ParseException {
		final List<Path> inpaths = Arrays.asList(cl.getArgList().stream().map(String::trim)
				.filter(path -> !path.isEmpty()).map(Paths::get).toArray(Path[]::new));
		if (inpaths.isEmpty()) {
			throw new MissingOptionException("No input path(s) specified.");
		} else {
			final Map<SessionDataManager, Path> allSessionData = TestSessionData.readTestSessionData(inpaths);
			final Training trainingMethod = Parameter.parseTrainingMethod(cl);
			LOGGER.info("Training method: {}", trainingMethod);

			final Path tokFilePath = ((File) cl.getParsedOptionValue(Parameter.TOKENIZATION_FILE_PATH.optName))
					.toPath();
			LOGGER.info("Reading tokenization data from \"{}\".", tokFilePath);
			final Map<List<String>, List<String>> tokenSeqTransformations = new UtteranceReferringLanguageMapParser(
					"UTTERANCE", "REFERRING_TOKENS").apply(Files.readAllLines(tokFilePath, TOKENIZATION_FILE_ENCODING));
			final MappingEventDialogueTransformer diagTransformer = new MappingEventDialogueTransformer(
					tokenSeqTransformations);
			final Future<Map<SessionDataManager, SessionGameManager>> futureSessionGameMgrs = backgroundJobExecutor
					.submit(() -> createSessionGameMgrMap(allSessionData));
			final Supplier<Map<SessionDataManager, SessionGameManager>> sessionGameMgrMapSupplier = () -> {
				try {
					return futureSessionGameMgrs.get();
				} catch (ExecutionException | InterruptedException e) {
					throw new TestException(e);
				}
			};
			return new UtteranceMappingBatchJobTester.Input(allSessionData, sessionGameMgrMapSupplier, trainingMethod,
					diagTransformer);
		}
	}

	private static Options createOptions() {
		final Options result = new Options();
		Arrays.stream(Parameter.values()).map(Parameter::get).forEach(result::addOption);
		return result;
	}

	private static Map<SessionDataManager, SessionGameManager> createSessionGameMgrMap(
			final Map<SessionDataManager, Path> allSessions) throws JAXBException, IOException {
		final Map<SessionDataManager, SessionGameManager> result = Maps.newHashMapWithExpectedSize(allSessions.size());
		final SessionGameManager.Factory sessionGameMgrFactory = new SessionGameManager.Factory(
				new LoggedEventReader(allSessions.size(), allSessions.size() * 10));
		for (final SessionDataManager sessionDataMgr : allSessions.keySet()) {
			final SessionGameManager sessionGameMgr = sessionGameMgrFactory.apply(sessionDataMgr);
			result.put(sessionDataMgr, sessionGameMgr);
		}
		return result;
	}

	private static Stream<String> createTestMethodColumnHeaders() {
		final Stream.Builder<String> resultBuilder = Stream.builder();
		resultBuilder.add(Training.class.getSimpleName());
		return resultBuilder.build();
	}

	private static Stream<String> createTestMethodRowCellValues(
			final UtteranceMappingBatchJobTester.TestParameters testParams) {
		final Stream.Builder<String> resultBuilder = Stream.builder();
		resultBuilder.add(testParams.getTrainingMethod().toString());
		return resultBuilder.build();
	}

	private static Stream<String> createTestParamRowCellValues(
			final UtteranceMappingBatchJobTester.BatchJobSummary summary, final String crossValidationIterId) {
		final Stream.Builder<String> resultBuilder = Stream.builder();
		resultBuilder.add(TIMESTAMP_FORMATTER.format(summary.getTestTimestamp()));
		resultBuilder.add(crossValidationIterId);
		createTestMethodRowCellValues(summary.getTestParams()).forEachOrdered(resultBuilder);
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

	private static void main(final CommandLine cl) throws IOException, CrossValidationTestException, ParseException {
		if (cl.hasOption(Parameter.HELP.optName)) {
			printHelp();
		} else {
			final ExecutorService backgroundJobExecutor = BackgroundJobs.fetchBackgroundJobExecutor();
			final UtteranceMappingBatchJobTester.Input input = createInput(cl, backgroundJobExecutor);

			try {
				final Path baseOutDir = ((File) cl.getParsedOptionValue(Parameter.OUTDIR.optName)).toPath();
				final List<Map<WordClassifierTrainingParameter, Object>> trainingParamMaps = readTrainingParamMaps(cl);

				// try (PrintWriter out = CLIParameters.parseOutpath(outDir, OUTPUT_ENCODING)) {
				try (final ClassPathXmlApplicationContext appCtx = new ClassPathXmlApplicationContext(
						"cross-validation.xml", ParameterFileReadingTestWriter.class)) {
					for (final Map<WordClassifierTrainingParameter, Object> trainingParamMap : trainingParamMaps) {
						final String description = (String) trainingParamMap
								.get(WordClassifierTrainingParameter.DESCRIPTION);
						final Path testOutfilePath = baseOutDir.resolve(description + ".tsv");
						LOGGER.info("Will write results of test \"{}\" to \"{}\".", description, testOutfilePath);
						final ParameterFileReadingTestWriter testParamCombinationTestWriter = new ParameterFileReadingTestWriter(
								testOutfilePath);
						final ParameterCombinationTester testParamCombinationTester = new ParameterCombinationTester(
								testParamCombinationTestWriter, trainingParamMap, appCtx, backgroundJobExecutor);
						testParamCombinationTester.accept(input);
					}

					LOGGER.info("Shutting down executor service.");
					backgroundJobExecutor.shutdown();
					LOGGER.info("Successfully shut down executor service.");

				}
			} catch (final Exception e) {
				shutdownExceptionally(backgroundJobExecutor);
				throw new CrossValidationTestException(e);
			}
		}
	}

	private static void printHelp() {
		final HelpFormatter formatter = new HelpFormatter();
		formatter.printHelp(ParameterFileReadingTestWriter.class.getSimpleName() + " INPATHS...", OPTIONS);
	}

	private static List<Map<WordClassifierTrainingParameter, Object>> readTrainingParamMaps(final CommandLine cl)
			throws ParseException, IOException {
		final Path trainingParamFilePath = ((File) cl
				.getParsedOptionValue(Parameter.WORD_CLASSIFIER_TRAINING_PARAMS.optName)).toPath();
		LOGGER.info("Reading word classifier training parameter sets at \"{}\".", trainingParamFilePath);
		final List<Map<WordClassifierTrainingParameter, Object>> result = new WordClassifierTrainingParameterTableParser()
				.apply(Files.readAllLines(trainingParamFilePath, TRAINING_PARAM_INFILE_ENCODING));
		LOGGER.info("Read {} word classifier training parameter combination(s).", result.size());

		final Stream<Object> descriptions = result.stream()
				.map(trainingParamMap -> trainingParamMap.get(WordClassifierTrainingParameter.DESCRIPTION));
		final Map<Object, Long> descriptionCounts = descriptions
				.collect(Collectors.groupingBy(Function.identity(), Collectors.counting()));
		final Object[] nonUniqueDescs = descriptionCounts.entrySet().stream()
				.filter(descriptionCount -> descriptionCount.getValue() > 1L).map(Entry::getKey).toArray(String[]::new);
		if (nonUniqueDescs.length < 1) {
			return result;
		} else {
			throw new IllegalArgumentException(String.format("Encountered non-unique value(s) for %s --- values: {}",
					WordClassifierTrainingParameter.DESCRIPTION, Arrays.toString(nonUniqueDescs)));
		}
	}

	private static void shutdownExceptionally(final ExecutorService executor) {
		LOGGER.debug("Emergency executor service shutdown.");
		executor.shutdownNow();
		LOGGER.debug("Successfully shut down executor service.");
	}

	@Nullable
	private String crossValidationIterId;

	private final List<DialogueAnalysisSummaryFactory.SummaryDatum> dataToWrite;

	private final DialogueAnalysisSummaryFactory rowDataFactory;

	private Consumer<UtteranceMappingBatchJobTester.BatchJobSummary> resultsWriter;

	private final Lock writeLock = new ReentrantLock();

	private final PrintWriterFetcher writerFetcher;

	private ParameterFileReadingTestWriter(final Path outPath) {
		this(outPath, new UtteranceDialogueRepresentationStringFactory(DataLanguageDefaults.getLocale()),
				DEFAULT_DATA_TO_WRITE);
	}

	private ParameterFileReadingTestWriter(final Path outPath,
			final Function<? super Iterator<Utterance>, String> uttDiagReprFactory,
			final List<DialogueAnalysisSummaryFactory.SummaryDatum> dataToWrite) {
		writerFetcher = new PrintWriterFetcher(outPath, OUTPUT_ENCODING);
		this.dataToWrite = dataToWrite;
		rowDataFactory = new DialogueAnalysisSummaryFactory(uttDiagReprFactory, dataToWrite);

		resultsWriter = this::writeInitialResults;
	}

	public void write(final UtteranceMappingBatchJobTester.BatchJobSummary summary) {
		resultsWriter.accept(summary);
	}

	public void writeError(final UtteranceMappingBatchJobTester.IncompleteResults incompleteResults,
			final Throwable thrown) {
		LOGGER.error(String.format("An error occurred while running test which was started at \"%s\".",
				TIMESTAMP_FORMATTER.format(incompleteResults.getTestStartTime())), thrown);
		final String errorDesc = String.format("%s: %s", thrown.getClass().getName(), thrown.getLocalizedMessage());

		final Stream.Builder<String> rowCellValBuilder = Stream.builder();
		rowCellValBuilder.add(TIMESTAMP_FORMATTER.format(incompleteResults.getTestStartTime()));
		createTestMethodRowCellValues(incompleteResults.getTestParams()).forEachOrdered(rowCellValBuilder);
		EntityInstanceAttributeContext.getClassValues().stream().map(val -> NULL_CELL_VALUE_REPR)
				.forEach(rowCellValBuilder);
		final Map<DialogueAnalysisSummaryFactory.SummaryDatum, Object> rowData = new EnumMap<>(
				DialogueAnalysisSummaryFactory.SummaryDatum.class);
		dataToWrite.forEach(datum -> rowData.put(datum, NULL_CELL_VALUE_REPR));
		rowData.put(DialogueAnalysisSummaryFactory.SummaryDatum.DESCRIPTION, errorDesc);
		final Stream<String> diagAnalysisRowCellVals = dataToWrite.stream().map(rowData::get).map(Object::toString);
		diagAnalysisRowCellVals.forEachOrdered(rowCellValBuilder);
		final String row = rowCellValBuilder.build().collect(ROW_CELL_JOINER);
		writeLock.lock();
		try {
			final PrintWriter writer = writerFetcher.get();
			writer.println(row);
		} finally {
			writeLock.unlock();
		}
		throw new TestException(thrown);
	}

	private void writeInitialResults(final UtteranceMappingBatchJobTester.BatchJobSummary summary) {
		writeLock.lock();
		try {
			final PrintWriter writer = writerFetcher.get();
			writer.println(createColHeaders(dataToWrite).collect(ROW_CELL_JOINER));
			writeResults(summary, writer);
			// No longer write the header after calling this method once
			resultsWriter = this::writeNextResults;
		} finally {
			writeLock.unlock();
		}
	}

	private void writeNextResults(final UtteranceMappingBatchJobTester.BatchJobSummary summary) {
		writeLock.lock();
		try {
			final PrintWriter writer = writerFetcher.get();
			writeResults(summary, writer);
		} finally {
			writeLock.unlock();
		}
	}

	/**
	 * <strong>NOTE:</strong> This method needs external synchronization on order to
	 * keep it from writing nonsense to the output file stream.
	 *
	 * @param summary
	 *            The {@link UtteranceMappingBatchJobTester.BatchJobSummary}
	 *            instance to write.
	 * @param writer
	 *            The {@link PrintWriter} to use for writing.
	 */
	private void writeResults(final UtteranceMappingBatchJobTester.BatchJobSummary summary, final PrintWriter writer) {
		final String[] testParamRowCellValues = createTestParamRowCellValues(summary, crossValidationIterId)
				.toArray(String[]::new);
		final List<CrossValidator.IterationResult> testResults = summary.getTestResults();
		for (final CrossValidator.IterationResult iterResult : testResults) {
			final int iterNo = iterResult.getIterNo();
			iterResult.getCvTestResults().forEach(infileSessionResults -> {
				final Path inpath = infileSessionResults.getKey();
				final CrossValidationTestSummary cvTestSummary = infileSessionResults.getValue();
				final String[] trainingRowCellVals = createTrainingDataRowCellValues(cvTestSummary)
						.map(Object::toString).toArray(String[]::new);
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
					writer.println(row);
				}
			});
		}
	}

	/**
	 * @param crossValidationIterId
	 *            the crossValidationIterId to set
	 */
	void setCrossValidationIterId(final String crossValidationIterId) {
		this.crossValidationIterId = crossValidationIterId;
	}

}
