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
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.Arrays;
import java.util.List;
import java.util.ListIterator;
import java.util.Map.Entry;
import java.util.concurrent.ExecutionException;
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

import iristk.system.Event;
import se.kth.speech.coin.tangrams.analysis.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.Utterance;
import se.kth.speech.coin.tangrams.analysis.UtteranceDialogueRepresentationStringFactory;
import se.kth.speech.coin.tangrams.analysis.features.ClassificationException;
import se.kth.speech.coin.tangrams.analysis.features.TrainingException;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.EventDialogueTester;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.SessionTester;

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
public final class DialogueAnalysisWriter {

	private enum Parameter implements Supplier<Option> {
		APP_CONTEXT_DEFINITIONS("a") {
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

		private static PrintWriter parseOutpath(final CommandLine cl) throws ParseException, IOException {
			final PrintWriter result;
			final File outfile = (File) cl.getParsedOptionValue(Parameter.OUTPATH.optName);
			if (outfile == null) {
				LOGGER.info("No output file path specified; Writing to standard output.");
				result = new PrintWriter(System.out);
			} else {
				LOGGER.info("Output file path is \"{}\".", outfile);
				result = new PrintWriter(Files.newBufferedWriter(outfile.toPath(), StandardOpenOption.CREATE,
						StandardOpenOption.TRUNCATE_EXISTING));
			}
			return result;
		}

		private static void printHelp() {
			final HelpFormatter formatter = new HelpFormatter();
			formatter.printHelp(DialogueAnalysisWriter.class.getSimpleName() + " INFILE", OPTIONS);
		}

		protected final String optName;

		private Parameter(final String optName) {
			this.optName = optName;
		}

	}

	private static final List<String> COL_HEADERS = Arrays.asList("INPATH", "TEST_ITER", "SESSION_ORDER", "EVENT_TIME",
			"DIALOGUE", "DIALOGUE_AS_TESTED", "GOLD_STD_ID", "RANK", "RR", "TESTED_UTT_COUNT", "TOTAL_UTT_COUNT",
			"MEAN_DIAG_UTTS_TESTED", "TOKEN_COUNT", "MEAN_TOKENS_PER_UTT");

	private static final Logger LOGGER = LoggerFactory.getLogger(DialogueAnalysisWriter.class);

	private static final Collector<CharSequence, ?, String> ROW_CELL_JOINER = Collectors.joining("\t");

	private static final UtteranceDialogueRepresentationStringFactory UTT_DIAG_REPR_FACTORY = new UtteranceDialogueRepresentationStringFactory();

	public static void main(final CommandLine cl)
			throws ParseException, TrainingException, ExecutionException, IOException, ClassificationException {
		if (cl.hasOption(Parameter.HELP.optName)) {
			Parameter.printHelp();
		} else {
			final List<Path> inpaths = Arrays.asList(cl.getArgList().stream().map(Paths::get).toArray(Path[]::new));
			if (inpaths.isEmpty()) {
				throw new MissingOptionException("No input path(s) specified.");

			} else {
				final String[] appCtxLocs = cl.getOptionValues(Parameter.APP_CONTEXT_DEFINITIONS.optName);
				try (final FileSystemXmlApplicationContext appCtx = new FileSystemXmlApplicationContext(appCtxLocs)) {
					final Tester tester = appCtx.getBean(Tester.class);
					final Tester.Result testResults = tester.apply(inpaths);
					try (PrintWriter out = Parameter.parseOutpath(cl)) {
						final DialogueAnalysisWriter writer = new DialogueAnalysisWriter(out);
						writer.write(testResults);
					}
				}
			}
		}
	}

	public static void main(final String[] args)
			throws TrainingException, ExecutionException, IOException, ClassificationException {
		final CommandLineParser parser = new DefaultParser();
		try {
			final CommandLine cl = parser.parse(Parameter.OPTIONS, args);
			main(cl);
		} catch (final ParseException e) {
			System.out.println(String.format("An error occured while parsing the command-line arguments: %s", e));
			Parameter.printHelp();
		}
	}

	private final PrintWriter out;

	public DialogueAnalysisWriter(final PrintWriter out) {
		this.out = out;
	}

	private void write(final Tester.Result cvtestResults) {
		out.println(COL_HEADERS.stream().collect(ROW_CELL_JOINER));

		for (final Entry<Path, List<SessionTester.Result>> infileSessionResults : cvtestResults.getSessionResults()
				.entrySet()) {
			final Path inpath = infileSessionResults.getKey();
			final List<SessionTester.Result> sessionResultList = infileSessionResults.getValue();
			for (final ListIterator<SessionTester.Result> sessionResultIter = sessionResultList
					.listIterator(); sessionResultIter.hasNext();) {
				final SessionTester.Result sessionResults = sessionResultIter.next();
				final int iterNo = sessionResultIter.nextIndex();
				{
					int sessionDialogueOrder = 1;
					for (final Entry<EventDialogue, EventDialogueTester.Result> diagTestResults : sessionResults
							.getDialogueTestResults()) {
						writeRow(inpath, iterNo, sessionDialogueOrder++, diagTestResults);
					}
				}
			}
		}

		// out.println("SESSION TOTALS");
		// for (final Entry<Path, SessionTester.Result> infileSessionResults :
		// cvtestResults.getSessionResults()
		// .entrySet()) {
		// final Path inpath = infileSessionResults.getKey();
		// final SessionTester.Result sessionResults =
		// infileSessionResults.getValue();
		// writeSessionSummary(inpath, sessionResults);
		// }
		// writeSummaryTotals(cvtestResults);
	}

	private void writeRow(final Object key, final Integer iterNo, final Integer sequenceOrder,
			final Entry<EventDialogue, EventDialogueTester.Result> diagTestResults) {
		final EventDialogue diag = diagTestResults.getKey();
		final String timestamp = diag.getLastEvent().map(Event::getTime).orElse("(no event found for dialogue)");
		final String uttDiagRepr = UTT_DIAG_REPR_FACTORY.apply(diag.getUtts().iterator());
		final EventDialogueTester.Result testResults = diagTestResults.getValue();
		final Stream<Utterance> uttsTested = testResults.utterancesTested();
		final String testedUttDiagRepr = UTT_DIAG_REPR_FACTORY.apply(uttsTested.iterator());

		final List<Object> cellVals = Arrays.asList(key, iterNo, sequenceOrder, timestamp, uttDiagRepr,
				testedUttDiagRepr, testResults.getGoldStandardReferentId(), testResults.rank(),
				testResults.reciprocalRank(), testResults.totalUtterancesTested(), testResults.totalUtteranceCount(),
				testResults.meanUtterancesTested(), testResults.totalTokensTested(),
				testResults.meanTokensPerTestedUtterance());
		assert cellVals.size() == COL_HEADERS.size();
		out.println(cellVals.stream().map(Object::toString).collect(ROW_CELL_JOINER));
	}

	// private void writeSessionSummary(Object key, final SessionTester.Result
	// sessionResults) {
	// final List<Object> cellVals = Arrays.asList(key, "", "", "",
	// sessionResults.meanRank(), sessionResults.meanReciprocalRank(),
	// sessionResults.meanUtterancesTestedPerDialogue(),
	// sessionResults.meanUtteranceTotalPerDialogue(),
	// sessionResults.meanUtterancesTestedPerDialogue(),
	// sessionResults.totalTokensTested(),
	// sessionResults.meanTokensPerTestedUtterance(),
	// sessionResults.getGoldStdReferentIdCounts().size());
	// assert cellVals.size() == COL_HEADERS.size();
	// out.println(cellVals.stream().map(Object::toString).collect(ROW_CELL_JOINER));
	// }

	// private void writeSummaryTotals(final CrossValidationTester.Result
	// cvtestResults) {
	// final SessionTester.Result totalSessionResults =
	// cvtestResults.totalResults();
	// final int uttsTestedCount = totalSessionResults.totalUtterancesTested();
	// final int totalDiagUtts = totalSessionResults.totalUtteranceCount();
	// final double meanDiagUttsTested = uttsTestedCount / (double)
	// totalDiagUtts;
	// final List<Object> cellVals = Arrays.asList("SUMMARY: ALL DIALOGUES AS
	// ONE SESSION", "", "", "",
	// totalSessionResults.meanRank(), totalSessionResults.meanReciprocalRank(),
	// totalSessionResults.meanUtterancesTestedPerDialogue(),
	// totalSessionResults.meanUtteranceTotalPerDialogue(), meanDiagUttsTested,
	// totalSessionResults.totalTokensTested(),
	// totalSessionResults.meanTokensPerTestedUtterance(),
	// cvtestResults.meanGoldStandardUniqueReferentIdCount());
	// assert cellVals.size() == COL_HEADERS.size();
	// out.println(cellVals.stream().map(Object::toString).collect(ROW_CELL_JOINER));
	// }

}
