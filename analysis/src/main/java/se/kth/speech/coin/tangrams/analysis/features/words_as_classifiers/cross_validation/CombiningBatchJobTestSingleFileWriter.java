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
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.EnumMap;
import java.util.EnumSet;
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
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
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

import it.unimi.dsi.fastutil.objects.Object2IntMap;
import se.kth.speech.coin.tangrams.CLIParameters;
import se.kth.speech.coin.tangrams.analysis.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.SessionDataManager;
import se.kth.speech.coin.tangrams.analysis.features.ClassificationException;
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

	private enum Parameter implements Supplier<Option> {
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
			formatter.printHelp(CombiningBatchJobTestSingleFileWriter.class.getSimpleName() + " INPATHS...", OPTIONS);
		}

		protected final String optName;

		private Parameter(final String optName) {
			this.optName = optName;
		}

	}

	private static final List<DialogueAnalysisSummaryFactory.SummaryDatum> DEFAULT_DATA_TO_WRITE = createDefaultDatumOrderingList();

	private static final Logger LOGGER = LoggerFactory.getLogger(CombiningBatchJobTestSingleFileWriter.class);

	private static final String NULL_CELL_VALUE_REPR = "?";

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

				try (PrintWriter out = CLIParameters
						.parseOutpath((File) cl.getParsedOptionValue(Parameter.OUTPATH.optName))) {
					final CombiningBatchJobTestSingleFileWriter writer = new CombiningBatchJobTestSingleFileWriter(out,
							true);
					try (final ClassPathXmlApplicationContext appCtx = new ClassPathXmlApplicationContext(
							"combining-batch-tester.xml", CombiningBatchJobTestSingleFileWriter.class)) {
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
