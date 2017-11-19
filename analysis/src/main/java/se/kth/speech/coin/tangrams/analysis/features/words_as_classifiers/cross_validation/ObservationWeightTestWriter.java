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
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

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
import org.springframework.context.support.ClassPathXmlApplicationContext;

import com.google.common.collect.Maps;

import it.unimi.dsi.fastutil.objects.Object2IntMap;
import se.kth.speech.coin.tangrams.CLIParameters;
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

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 24 May 2017
 *
 */
final class ObservationWeightTestWriter { // NO_UCD (unused code)

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
		OUTFILE_NAME("n") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("outfile-name")
						.desc("The filename to write the extracted data to for each input session.").hasArg()
						.argName("name").build();
			}
		},
		OUTPATH("o") {
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
						.desc("AThe training method to use. Possible values: " + Arrays.toString(possibleVals)).hasArg()
						.argName("name").required().build();
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

	private static final Logger LOGGER = LoggerFactory.getLogger(ObservationWeightTestWriter.class);

	private static final String NULL_CELL_VALUE_REPR = "?";

	private static final Options OPTIONS = createOptions();

	private static final Charset OUTPUT_ENCODING = StandardCharsets.UTF_8;

	private static final int RANDOM_SESSION_DISCOUNTING_ITERS = 10;

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

	private static final BiConsumer<Object, Object> UTT_REL_HANDLER = (evtDiag, uttRels) -> {
		// Do nothing
	};

	static final DateTimeFormatter TIMESTAMP_FORMATTER = EventTimes.FORMATTER;

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
			final Map<List<String>, List<String>> tokenSeqTransformations = new UtteranceReferringLanguageMapReader(
					"UTTERANCE", "REFERRING_TOKENS").apply(Files.readAllLines(tokFilePath, TOKENIZATION_FILE_ENCODING));
			final MappingEventDialogueTransformer diagTransformer = new MappingEventDialogueTransformer(
					tokenSeqTransformations);
			final Future<Map<SessionDataManager, SessionGameManager>> futureSessionGameMgrs = backgroundJobExecutor
					.submit(() -> createSessionGameMgrMap(allSessionData));
			return new UtteranceMappingBatchJobTester.Input(allSessionData, futureSessionGameMgrs, trainingMethod,
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
			final UtteranceMappingBatchJobTester.BatchJobSummary summary) {
		final Stream.Builder<String> resultBuilder = Stream.builder();
		resultBuilder.add(TIMESTAMP_FORMATTER.format(summary.getTestTimestamp()));
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
				final Future<Map<SessionDataManager, SessionGameManager>> futureSessionGameMgrs = backgroundJobExecutor
						.submit(() -> createSessionGameMgrMap(input.getAllSessionData()));

				final File outFile = (File) cl.getParsedOptionValue(Parameter.OUTPATH.optName);
				try (PrintWriter out = CLIParameters.parseOutpath(outFile, OUTPUT_ENCODING)) {
					final ObservationWeightTestWriter writer = new ObservationWeightTestWriter(out, true, 1);

					try (final ClassPathXmlApplicationContext appCtx = new ClassPathXmlApplicationContext(
							"cross-validation.xml", ObservationWeightTestWriter.class)) {
						final Map<WordClassifierTrainingParameter, Object> defaultTrainingParams = WordClassifierTrainingParameter
								.createDefaultMap();

						final Map<SessionDataManager, SessionGameManager> sessionGameMgrs = futureSessionGameMgrs.get();
						// One session for testing, one for training
						final int maxTrainingSetSizeDiscountingFactor = input.getAllSessionData().size() - 2;
						for (int trainingSetSizeDiscountingConstant = 1; trainingSetSizeDiscountingConstant < maxTrainingSetSizeDiscountingFactor; ++trainingSetSizeDiscountingConstant) {
							LOGGER.info("Performing cross-validation while discounting training set size by {}.",
									trainingSetSizeDiscountingConstant);
							final Map<WordClassifierTrainingParameter, Object> trainingParams = new EnumMap<>(
									defaultTrainingParams);
							trainingParams.put(WordClassifierTrainingParameter.TRAINING_SET_SIZE_DISCOUNTING_CONSTANT,
									trainingSetSizeDiscountingConstant);
							// Use the same Random instance for each random
							// iteration so that each iteration is at least
							// somewhat different from the others
							final Random random = new Random(
									(Long) trainingParams.get(WordClassifierTrainingParameter.RANDOM_SEED));
							for (int randomDiscountingIter = 1; randomDiscountingIter <= RANDOM_SESSION_DISCOUNTING_ITERS; ++randomDiscountingIter) {
								final UtteranceMappingBatchJobTester tester = new UtteranceMappingBatchJobTester(
										backgroundJobExecutor, appCtx, sessionGameMgrs, writer::write,
										writer::writeError, UTT_REL_HANDLER, trainingParams,
										new DiscountingTestSetFactoryFactory(trainingParams, random));
								tester.accept(input);
								writer.setSessionTestIterFactor(randomDiscountingIter);
							}
							LOGGER.info(
									"Finished performing cross-validation for training set size discounting constant {}.",
									trainingSetSizeDiscountingConstant);
						}

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
		formatter.printHelp(ObservationWeightTestWriter.class.getSimpleName() + " INPATHS...", OPTIONS);
	}

	private static void shutdownExceptionally(final ExecutorService executor) {
		LOGGER.debug("Emergency executor service shutdown.");
		executor.shutdownNow();
		LOGGER.debug("Successfully shut down executor service.");
	}

	private final List<DialogueAnalysisSummaryFactory.SummaryDatum> dataToWrite;

	private final PrintWriter out;

	private final DialogueAnalysisSummaryFactory rowDataFactory;

	private int sessionTestIterFactor;

	private boolean writeHeader;

	private ObservationWeightTestWriter(final PrintWriter out, final boolean writeHeader,
			final int sessionTestIterFactor) {
		this(out, writeHeader, sessionTestIterFactor,
				new UtteranceDialogueRepresentationStringFactory(DataLanguageDefaults.getLocale()),
				DEFAULT_DATA_TO_WRITE);
	}

	private ObservationWeightTestWriter(final PrintWriter out, final boolean writeHeader,
			final int sessionTestIterFactor, final Function<? super Iterator<Utterance>, String> uttDiagReprFactory,
			final List<DialogueAnalysisSummaryFactory.SummaryDatum> dataToWrite) {
		this.out = out;
		this.writeHeader = writeHeader;
		this.sessionTestIterFactor = sessionTestIterFactor;
		this.dataToWrite = dataToWrite;
		rowDataFactory = new DialogueAnalysisSummaryFactory(uttDiagReprFactory, dataToWrite);

	}

	public void write(final UtteranceMappingBatchJobTester.BatchJobSummary summary) {
		if (writeHeader) {
			out.println(createColHeaders(dataToWrite).collect(ROW_CELL_JOINER));
			writeHeader = false;
		}
		final String[] testParamRowCellValues = createTestParamRowCellValues(summary).toArray(String[]::new);
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
					out.println(row);
				}
			});
		}
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
		out.println(row);
		throw new TestException(thrown);
	}

	/**
	 * @param sessionTestIterFactor
	 *            the sessionTestIterFactor to set
	 */
	void setSessionTestIterFactor(final int sessionTestIterFactor) {
		this.sessionTestIterFactor = sessionTestIterFactor;
	}

}
