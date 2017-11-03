/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.analysis.
 *
 *  tangrams is free software: you can redistribute it and/or modify
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
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.OptionalInt;
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
import org.springframework.context.support.FileSystemXmlApplicationContext;

import se.kth.speech.coin.tangrams.CLIParameters;
import se.kth.speech.coin.tangrams.analysis.BackgroundJobs;
import se.kth.speech.coin.tangrams.analysis.DataLanguageDefaults;
import se.kth.speech.coin.tangrams.analysis.SessionGameManagerCacheSupplier;
import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.coin.tangrams.analysis.dialogues.UtteranceDialogueRepresentationStringFactory;
import se.kth.speech.coin.tangrams.analysis.features.ClassificationException;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.EventDialogueTestResults;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.cross_validation.CrossValidator.CrossValidationTestSummary;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues.DummyEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues.EventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues.UtteranceRelation;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.TrainingInstancesFactory;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since May 18, 2017
 * @see
 *      <ul>
 *      <li><a href="http://anthology.aclweb.org/W/W15/W15-0124.pdf">Casey
 *      Kennington, Livia Dia, &amp; David Schlangen. &ldquo;A Discriminative
 *      Model for Perceptually-Grounded Incremental Reference Resolution.&rdquo;
 *      In <em>Proceedings of IWCS 2015</em><a>.</li>
 *      <li><a href="http://www.aclweb.org/anthology/P15-1029">Casey Kennington,
 *      &amp; David Schlangen. &ldquo;Simple Learning and Compositional
 *      Application of Perceptually Grounded Word Meanings for Incremental
 *      Reference Resolution&rdquo;. In <em>Proceedings of the 53<sup>rd</sup>
 *      Annual Meeting of the Association for Computational Linguistics and the
 *      7<sup>th</sup> International Joint Conference on Natural Language
 *      Processing</em><a>.</li>
 *      </ul>
 *
 */
final class DialogueAnalysisWriter implements Consumer<CrossValidator.Result> { // NO_UCD
																				// (use
																				// default)

	private enum Parameter implements Supplier<Option> {
		APP_CONTEXT_DEFINITIONS("c") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("app-ctx")
						.desc("Location(s) to the Spring application context definition file(s) to load for configuration.")
						.hasArgs().argName("locator").required().build();
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
						.argName("path").type(File.class).build();
			}
		};

		private static final Options OPTIONS = createOptions();

		private static Options createOptions() {
			final Options result = new Options();
			Arrays.stream(Parameter.values()).map(Parameter::get).forEach(result::addOption);
			return result;
		}

		private static void printHelp() {
			final HelpFormatter formatter = new HelpFormatter();
			formatter.printHelp(DialogueAnalysisWriter.class.getSimpleName() + " INPATHS...", OPTIONS);
		}

		protected final String optName;

		private Parameter(final String optName) {
			this.optName = optName;
		}

	}

	private static final List<DialogueAnalysisSummaryFactory.SummaryDatum> DEFAULT_DATA_TO_WRITE = DialogueAnalysisSummaryFactory
			.getDefaultSummaryDatumOrdering();

	private static final EventDialogueTransformer EVT_DIAG_TRANSFORMER = new DummyEventDialogueTransformer();

	private static final Logger LOGGER = LoggerFactory.getLogger(DialogueAnalysisWriter.class);

	private static final Collector<CharSequence, ?, String> ROW_CELL_JOINER = Collectors.joining("\t");

	private static final Training TRAINING_METHOD = Training.ALL_NEG;

	private static final BiConsumer<EventDialogue, Iterable<UtteranceRelation>> UTT_REL_HANDLER = (evtDiag,
			uttRels) -> {
		// Do nothing
	};

	public static void main(final String[] args) throws IOException, ClassificationException {
		final CommandLineParser parser = new DefaultParser();
		try {
			final CommandLine cl = parser.parse(Parameter.OPTIONS, args);
			main(cl);
		} catch (final ParseException e) {
			System.out.println(String.format("An error occured while parsing the command-line arguments: %s", e));
			Parameter.printHelp();
		}
	}

	private static void main(final CommandLine cl) throws ParseException, IOException, ClassificationException {
		if (cl.hasOption(Parameter.HELP.optName)) {
			Parameter.printHelp();
		} else {
			final List<Path> inpaths = Arrays.asList(cl.getArgList().stream().map(String::trim)
					.filter(path -> !path.isEmpty()).map(Paths::get).toArray(Path[]::new));
			if (inpaths.isEmpty()) {
				throw new MissingOptionException("No input path(s) specified.");

			} else {
				final String[] appCtxLocs = CLIParameters
						.parseAppCtxDefPaths(cl.getOptionValues(Parameter.APP_CONTEXT_DEFINITIONS.optName)).stream()
						.toArray(String[]::new);
				final OptionalInt iterCount = CLIParameters
						.parseIterCount((Number) cl.getParsedOptionValue(Parameter.ITER_COUNT.optName));
				final ForkJoinPool backgroundJobExecutor = BackgroundJobs.getBackgroundJobExecutor();

				try (final FileSystemXmlApplicationContext appCtx = new FileSystemXmlApplicationContext(appCtxLocs)) {
					final Map<WordClassifierTrainingParameter, Object> trainingParams = WordClassifierTrainingParameter
							.createDefaultMap();
					final TrainingContext trainingCtx = new TrainingContext(EVT_DIAG_TRANSFORMER, appCtx,
							UTT_REL_HANDLER, trainingParams);
					final TrainingInstancesFactory trainingInstsFactory = TRAINING_METHOD
							.createTrainingInstsFactory(trainingCtx);
					final SessionGameManagerCacheSupplier sessionDiagMgrCacheSupplier = appCtx
							.getBean(SessionGameManagerCacheSupplier.class);
					final TestSetFactory testSetFactory = new TestSetFactory(trainingInstsFactory,
							sessionDiagMgrCacheSupplier);
					final CrossValidator crossValidator = appCtx.getBean(CrossValidator.class, testSetFactory,
							EVT_DIAG_TRANSFORMER, TRAINING_METHOD.getClassifierFactory(trainingCtx),
							backgroundJobExecutor);
					iterCount.ifPresent(crossValidator::setIterCount);
					final CrossValidator.Result testResults = crossValidator
							.apply(TestSessionData.readTestSessionData(inpaths));
					try (PrintWriter out = CLIParameters
							.parseOutpath((File) cl.getParsedOptionValue(Parameter.OUTPATH.optName))) {
						final DialogueAnalysisWriter writer = new DialogueAnalysisWriter(out,
								crossValidator.getIterCount(), trainingParams);
						writer.accept(testResults);
					}
				}
			}
		}
	}

	private final List<DialogueAnalysisSummaryFactory.SummaryDatum> dataToWrite;

	private final int maxIters;

	private final PrintWriter out;

	private final DialogueAnalysisSummaryFactory rowDataFactory;

	private final Map<WordClassifierTrainingParameter, Object> trainingParams;

	private DialogueAnalysisWriter(final PrintWriter out, final int maxIters,
			final Function<? super Iterator<Utterance>, String> uttDiagReprFactory,
			final Map<WordClassifierTrainingParameter, Object> trainingParams,
			final List<DialogueAnalysisSummaryFactory.SummaryDatum> dataToWrite) {
		this.out = out;
		this.maxIters = maxIters;
		this.dataToWrite = dataToWrite;
		this.trainingParams = trainingParams;
		rowDataFactory = new DialogueAnalysisSummaryFactory(uttDiagReprFactory, dataToWrite);
	}

	DialogueAnalysisWriter(final PrintWriter out, final int maxIters,
			final Map<WordClassifierTrainingParameter, Object> trainingParams) {
		this(out, maxIters, new UtteranceDialogueRepresentationStringFactory(DataLanguageDefaults.getLocale()),
				trainingParams, DEFAULT_DATA_TO_WRITE);
	}

	@Override
	public void accept(final CrossValidator.Result cvtestResults) {
		out.println(dataToWrite.stream().map(Enum::toString).collect(ROW_CELL_JOINER));

		for (final Entry<Path, List<CrossValidationTestSummary>> infileSessionResults : cvtestResults
				.getSessionResults().entrySet()) {
			final Path inpath = infileSessionResults.getKey();
			final List<CrossValidationTestSummary> sessionResultList = infileSessionResults.getValue();
			for (final ListIterator<CrossValidationTestSummary> sessionResultIter = sessionResultList
					.listIterator(); sessionResultIter.hasNext();) {
				final CrossValidationTestSummary cvTestSummary = sessionResultIter.next();
				// NOTE: This should remain here, after "Iterator.next()", so
				// that the printed first iteration is "1" rather than "0"
				final int iterNo = sessionResultIter.nextIndex();
				if (iterNo <= maxIters) {
					int sessionDialogueOrder = 1;
					for (final Entry<EventDialogue, EventDialogueTestResults> diagTestResults : cvTestSummary
							.getTestResults().getDialogueTestResults()) {
						final Map<DialogueAnalysisSummaryFactory.SummaryDatum, Object> rowData = rowDataFactory
								.apply(new DialogueAnalysisSummaryFactory.Input(inpath, "Success", iterNo,
										sessionDialogueOrder++, diagTestResults, trainingParams,
										cvTestSummary.getSessionStartTime()));
						final Stream<String> rowCellVals = dataToWrite.stream().map(rowData::get).map(Object::toString);
						out.print(rowCellVals.collect(ROW_CELL_JOINER));
					}
				} else {
					LOGGER.debug("Maximum iteration output reached ({}).", maxIters);
					break;
				}
			}
		}
	}

}
