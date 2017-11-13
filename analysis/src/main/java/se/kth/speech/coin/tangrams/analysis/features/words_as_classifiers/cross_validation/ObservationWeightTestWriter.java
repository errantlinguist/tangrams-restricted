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
import java.nio.file.Path;
import java.nio.file.Paths;
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
import java.util.concurrent.ForkJoinPool;
import java.util.function.BiConsumer;
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

import com.google.common.cache.LoadingCache;

import se.kth.speech.coin.tangrams.CLIParameters;
import se.kth.speech.coin.tangrams.analysis.BackgroundJobs;
import se.kth.speech.coin.tangrams.analysis.DataLanguageDefaults;
import se.kth.speech.coin.tangrams.analysis.SessionGameManager;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.coin.tangrams.analysis.dialogues.UtteranceDialogueRepresentationStringFactory;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.TrainingInstancesFactory;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.WordClassificationData;
import se.kth.speech.coin.tangrams.analysis.io.SessionDataManager;

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
				result = new RandomDiscountingTestSetFactory(trainingInstsFactory, sessionGameMgrs, random,
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
				return Option.builder(optName).longOpt("tokenization-file")
						.desc("A path to the file mapping token sequences to the relevant referring-language tokens each represent.")
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
		public TestException(final Throwable cause) {
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

	private static final BiConsumer<Object, Object> UTT_REL_HANDLER = (evtDiag, uttRels) -> {
		// Do nothing
	};

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

	private static List<DialogueAnalysisSummaryFactory.SummaryDatum> createDefaultDatumOrderingList() {
		final Set<DialogueAnalysisSummaryFactory.SummaryDatum> excludedData = EnumSet.of(
				DialogueAnalysisSummaryFactory.SummaryDatum.DIALOGUE,
				DialogueAnalysisSummaryFactory.SummaryDatum.DIALOGUE_AS_TESTED,
				DialogueAnalysisSummaryFactory.SummaryDatum.GOLD_STD_ID);
		return Arrays.asList(DialogueAnalysisSummaryFactory.getDefaultSummaryDatumOrdering().stream()
				.filter(datum -> !excludedData.contains(datum))
				.toArray(DialogueAnalysisSummaryFactory.SummaryDatum[]::new));
	}

	private static UtteranceMappingBatchJobTester.Input createInput(final CommandLine cl)
			throws MissingOptionException, IOException {
		final List<Path> inpaths = Arrays.asList(cl.getArgList().stream().map(String::trim)
				.filter(path -> !path.isEmpty()).map(Paths::get).toArray(Path[]::new));
		if (inpaths.isEmpty()) {
			throw new MissingOptionException("No input path(s) specified.");
		} else {
			final Map<SessionDataManager, Path> allSessionData = TestSessionData.readTestSessionData(inpaths);
			final Training trainingMethod = Parameter.parseTrainingMethod(cl);
			LOGGER.info("Training method: {}", trainingMethod);
			return new UtteranceMappingBatchJobTester.Input(allSessionData, trainingMethod);
		}
	}

	private static Options createOptions() {
		final Options result = new Options();
		Arrays.stream(Parameter.values()).map(Parameter::get).forEach(result::addOption);
		return result;
	}

	private static void main(final CommandLine cl)
			throws MissingOptionException, IOException, CrossValidationTestException {
		if (cl.hasOption(Parameter.HELP.optName)) {
			printHelp();
		} else {
			final ForkJoinPool backgroundJobExecutor = BackgroundJobs.getBackgroundJobExecutor();
			final UtteranceMappingBatchJobTester.Input input = createInput(cl);

			final Consumer<CrossValidator> testerConfigurator = crossValidator -> {
				// Do nothing
			};

			try {
				final File outFile = (File) cl.getParsedOptionValue(Parameter.OUTPATH.optName);
				try (PrintWriter out = CLIParameters.parseOutpath(outFile, OUTPUT_ENCODING)) {
					final ObservationWeightTestWriter writer = new ObservationWeightTestWriter(out, true, 1);

					try (final ClassPathXmlApplicationContext appCtx = new ClassPathXmlApplicationContext(
							"cross-validation.xml", ObservationWeightTestWriter.class)) {
						final Map<WordClassifierTrainingParameter, Object> defaultTrainingParams = WordClassifierTrainingParameter
								.createDefaultMap();

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
										backgroundJobExecutor, appCtx, writer::write, writer::writeError,
										testerConfigurator, UTT_REL_HANDLER, trainingParams,
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

	private final boolean writeHeader;

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
		// if (writeHeader) {
		// out.println(TestParameterReporting.createColHeaders(dataToWrite).collect(ROW_CELL_JOINER));
		// writeHeader = false;
		// }
		// final String[] testParamRowCellValues =
		// TestParameterReporting.createTestParamRowCellValues(summary)
		// .toArray(String[]::new);
		// final CrossValidator.Result testResults = summary.getTestResults();
		// for (final Entry<Path, List<CrossValidationTestSummary>>
		// infileSessionResults : testResults.getSessionResults()
		// .entrySet()) {
		// final Path inpath = infileSessionResults.getKey();
		// final List<CrossValidationTestSummary> sessionResultList =
		// infileSessionResults.getValue();
		// for (final ListIterator<CrossValidationTestSummary> sessionResultIter
		// = sessionResultList
		// .listIterator(); sessionResultIter.hasNext();) {
		// final CrossValidationTestSummary cvTestSummary =
		// sessionResultIter.next();
		// final String[] trainingRowCellVals = TestParameterReporting
		// .createTrainingDataRowCellValues(cvTestSummary).map(Object::toString).toArray(String[]::new);
		// // NOTE: This should remain here, after "Iterator.next()", so
		// // that the printed first iteration is "1" rather than "0"
		// final int iterNo = sessionResultIter.nextIndex() *
		// sessionTestIterFactor;
		// int sessionDialogueOrder = 1;
		// final LocalDateTime sessionStartTime =
		// cvTestSummary.getSessionStartTime();
		// for (final Entry<EventDialogue, EventDialogueTestResults>
		// diagTestResults : cvTestSummary
		// .getTestResults().getDialogueTestResults()) {
		// final Map<DialogueAnalysisSummaryFactory.SummaryDatum, Object>
		// rowData = rowDataFactory.apply(
		// new DialogueAnalysisSummaryFactory.Input(inpath, "Success", iterNo,
		// sessionDialogueOrder++,
		// diagTestResults, summary.getTestParams().getTrainingParams(),
		// sessionStartTime));
		// final Stream<String> diagAnalysisRowCellVals =
		// dataToWrite.stream().map(rowData::get)
		// .map(Object::toString);
		// final Stream.Builder<String> rowCellValBuilder = Stream.builder();
		// Arrays.stream(testParamRowCellValues).forEachOrdered(rowCellValBuilder);
		// Arrays.stream(trainingRowCellVals).forEachOrdered(rowCellValBuilder);
		// diagAnalysisRowCellVals.forEachOrdered(rowCellValBuilder);
		// final String row =
		// rowCellValBuilder.build().collect(ROW_CELL_JOINER);
		// out.println(row);
		// }
		// }
		// }
	}

	public void writeError(final UtteranceMappingBatchJobTester.IncompleteResults incompleteResults,
			final Throwable thrown) {
		// LOGGER.error(
		// String.format("An error occurred while running test which was started
		// at \"%s\".",
		// TestParameterReporting.TIMESTAMP_FORMATTER.format(incompleteResults.getTestStartTime())),
		// thrown);
		// final String errorDesc = String.format("%s: %s",
		// thrown.getClass().getName(), thrown.getLocalizedMessage());
		//
		// final Stream.Builder<String> rowCellValBuilder = Stream.builder();
		// rowCellValBuilder.add(TestParameterReporting.TIMESTAMP_FORMATTER.format(incompleteResults.getTestStartTime()));
		// TestParameterReporting.createTestMethodRowCellValues(incompleteResults.getTestParams(),
		// TestParameterReporting::createCleaningMethodBooleanValues).forEachOrdered(rowCellValBuilder);
		// EntityInstanceAttributeContext.getClassValues().stream().map(val ->
		// NULL_CELL_VALUE_REPR)
		// .forEach(rowCellValBuilder);
		// final Map<DialogueAnalysisSummaryFactory.SummaryDatum, Object>
		// rowData = new EnumMap<>(
		// DialogueAnalysisSummaryFactory.SummaryDatum.class);
		// dataToWrite.forEach(datum -> rowData.put(datum,
		// NULL_CELL_VALUE_REPR));
		// rowData.put(DialogueAnalysisSummaryFactory.SummaryDatum.DESCRIPTION,
		// errorDesc);
		// final Stream<String> diagAnalysisRowCellVals =
		// dataToWrite.stream().map(rowData::get).map(Object::toString);
		// diagAnalysisRowCellVals.forEachOrdered(rowCellValBuilder);
		// final String row =
		// rowCellValBuilder.build().collect(ROW_CELL_JOINER);
		// out.println(row);
		// throw new TestException(thrown);
	}

	/**
	 * @param sessionTestIterFactor
	 *            the sessionTestIterFactor to set
	 */
	void setSessionTestIterFactor(final int sessionTestIterFactor) {
		this.sessionTestIterFactor = sessionTestIterFactor;
	}

}
