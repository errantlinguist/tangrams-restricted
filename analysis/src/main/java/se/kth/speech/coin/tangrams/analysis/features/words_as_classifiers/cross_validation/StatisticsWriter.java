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
import java.util.OptionalInt;
import java.util.concurrent.ExecutionException;
import java.util.function.Consumer;
import java.util.function.Supplier;
import java.util.stream.Collector;
import java.util.stream.Collectors;

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

import se.kth.speech.coin.tangrams.analysis.features.ClassificationException;
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
public final class StatisticsWriter implements Consumer<Tester.Result> {

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
			formatter.printHelp(StatisticsWriter.class.getSimpleName() + " INPATHS...", OPTIONS);
		}

		protected final String optName;

		private Parameter(final String optName) {
			this.optName = optName;
		}

	}

	private static final List<String> COL_HEADERS = Arrays.asList("INPATH", "TEST_ITER", "MEAN_RANK", "MRR",
			"DIAG_COUNT", "UTTS_TESTED", "MEAN_UTTS_PER_DIAG");

	private static final Logger LOGGER = LoggerFactory.getLogger(StatisticsWriter.class);

	private static final Collector<CharSequence, ?, String> ROW_CELL_JOINER = Collectors.joining("\t");

	public static List<Object> createSummaryTableRow(final Object key, final Tester.Result testResults) {
		return createTableRow(key, testResults.iterCount(), testResults.totalResults());
	}

	public static void main(final CommandLine cl) throws ParseException, IOException, ClassificationException, ExecutionException {
		if (cl.hasOption(Parameter.HELP.optName)) {
			Parameter.printHelp();
		} else {
			final List<Path> inpaths = Arrays.asList(cl.getArgList().stream().map(Paths::get).toArray(Path[]::new));
			if (inpaths.isEmpty()) {
				throw new MissingOptionException("No input path(s) specified.");

			} else {
				final String[] appCtxLocs = cl.getOptionValues(Parameter.APP_CONTEXT_DEFINITIONS.optName);
				final OptionalInt iterCount = Parameter.parseIterCount(cl);
				try (final FileSystemXmlApplicationContext appCtx = new FileSystemXmlApplicationContext(appCtxLocs)) {
					final Tester tester = appCtx.getBean(Tester.class);
					iterCount.ifPresent(tester::setIterCount);
					final Tester.Result testResults = tester.apply(TestSessionData.readTestSessionData(inpaths));
					try (PrintWriter out = Parameter.parseOutpath(cl)) {
						final StatisticsWriter writer = new StatisticsWriter(out);
						writer.accept(testResults);
					}
				}
			}
		}
	}

	public static void main(final String[] args) throws IOException, ClassificationException, ExecutionException {
		final CommandLineParser parser = new DefaultParser();
		try {
			final CommandLine cl = parser.parse(Parameter.OPTIONS, args);
			main(cl);
		} catch (final ParseException e) {
			System.out.println(String.format("An error occured while parsing the command-line arguments: %s", e));
			Parameter.printHelp();
		}
	}

	private static List<Object> createTableRow(final Object key, final Integer iterNo,
			final SessionTester.Result sessionTestResults) {
		final int totalUttsTested = sessionTestResults.totalUtterancesTested();
		final int totalDiagsTested = sessionTestResults.totalDialoguesTested();
		return Arrays.asList(key, iterNo, sessionTestResults.meanRank(), sessionTestResults.meanReciprocalRank(),
				totalDiagsTested, totalUttsTested, totalUttsTested / (double) totalDiagsTested);
	}

	private final PrintWriter out;

	public StatisticsWriter(final PrintWriter out) {
		this.out = out;
	}

	@Override
	public void accept(final Tester.Result testResults) {
		out.println(COL_HEADERS.stream().collect(ROW_CELL_JOINER));

		for (final Entry<Path, List<SessionTester.Result>> infileSessionTestResults : testResults.getSessionResults()
				.entrySet()) {
			final Path infilePath = infileSessionTestResults.getKey();
			final List<SessionTester.Result> sessionTestResultList = infileSessionTestResults.getValue();
			for (final ListIterator<SessionTester.Result> sessionTestResultIter = sessionTestResultList
					.listIterator(); sessionTestResultIter.hasNext();) {
				final SessionTester.Result sessionResults = sessionTestResultIter.next();
				final int iterNo = sessionTestResultIter.nextIndex();
				final List<Object> cellVals = createTableRow(infilePath, iterNo, sessionResults);
				out.println(cellVals.stream().map(Object::toString).collect(ROW_CELL_JOINER));
			}
		}

		final List<Object> summaryVals = createSummaryTableRow("SUMMARY", testResults);
		out.print(summaryVals.stream().map(Object::toString).collect(ROW_CELL_JOINER));
	}

}