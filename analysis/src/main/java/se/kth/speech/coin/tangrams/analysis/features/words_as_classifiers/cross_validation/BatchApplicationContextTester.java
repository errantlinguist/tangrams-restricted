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
import java.nio.file.Files;
import java.nio.file.OpenOption;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.OptionalInt;
import java.util.Set;
import java.util.concurrent.ExecutionException;
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

import se.kth.speech.coin.tangrams.analysis.SessionDataManager;
import se.kth.speech.coin.tangrams.analysis.features.ClassificationException;
import se.kth.speech.io.FileNames;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 24 May 2017
 *
 */
public final class BatchApplicationContextTester {

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
						.argName("path").type(File.class).required().build();
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

		private static void printHelp() {
			final HelpFormatter formatter = new HelpFormatter();
			formatter.printHelp(BatchApplicationContextTester.class.getSimpleName() + " INPATHS...", OPTIONS);
		}

		protected final String optName;

		private Parameter(final String optName) {
			this.optName = optName;
		}

	}

	private static final List<String> COL_HEADERS = Arrays.asList("INPATH", "ITER_COUNT", "MEAN_RANK", "MRR",
			"DIAG_COUNT", "UTTS_TESTED", "MEAN_UTTS_PER_DIAG");

	private static final Logger LOGGER = LoggerFactory.getLogger(BatchApplicationContextTester.class);

	private static final Collector<CharSequence, ?, String> ROW_CELL_JOINER = Collectors.joining("\t");

	public static void main(final CommandLine cl) throws IOException, ParseException, ClassificationException, ExecutionException {
		if (cl.hasOption(Parameter.HELP.optName)) {
			Parameter.printHelp();
		} else {
			final List<Path> inpaths = Arrays.asList(cl.getArgList().stream().map(Paths::get).toArray(Path[]::new));
			if (inpaths.isEmpty()) {
				throw new MissingOptionException("No input path(s) specified.");

			} else {
				final String[] appCtxLocs = cl.getOptionValues(Parameter.APP_CONTEXT_DEFINITIONS.optName);
				final Set<Path> appCtxDefPaths = createBatchAppDefSet(appCtxLocs);
				final OptionalInt iterCount = Parameter.parseIterCount(cl);
				final Path outdir = ((File) cl.getParsedOptionValue(Parameter.OUTPATH.optName)).toPath();
				LOGGER.info("Will write data to \"{}\".", outdir);

				final Map<SessionDataManager, Path> allSessions = TestSessionData.readTestSessionData(inpaths);
				final BatchApplicationContextTester tester = new BatchApplicationContextTester(allSessions, iterCount,
						outdir);
				tester.accept(appCtxDefPaths);
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

	private static Set<Path> createBatchAppDefSet(final String[] appCtxLocs) throws IOException {
		final Set<Path> result = new HashSet<>();
		for (final String appCtxLoc : appCtxLocs) {
			final Path appCtxPath = Paths.get(appCtxLoc);
			final Iterator<Path> childPathIter = Files.walk(appCtxPath, 1).iterator();
			while (childPathIter.hasNext()) {
				final Path childPath = childPathIter.next();
				final String contentType = Files.probeContentType(childPath);
				if (contentType != null && contentType.endsWith("/xml")) {
					result.add(childPath);
				}
			}
		}
		return result;
	}

	private static String createBatchOutdirName(final Path appCtxDefPath) {
		return FileNames.splitBase(appCtxDefPath.getFileName().toString())[0];
	}

	private static void writeResults(final Tester.Result result, final Path actualOutdirPath) throws IOException {
		final Path statsFilePath = actualOutdirPath.resolve("stats.tsv");
		try (final PrintWriter out = new PrintWriter(Files.newBufferedWriter(statsFilePath, StandardOpenOption.CREATE,
				StandardOpenOption.TRUNCATE_EXISTING))) {
			final StatisticsWriter writer = new StatisticsWriter(out);
			writer.accept(result);
		}
		LOGGER.info("Wrote cross-validation statistics to \"{}\".", statsFilePath);

		final Path diagAnalysisPath = actualOutdirPath.resolve("diag-analysis-firstiter.tsv");
		try (final PrintWriter out = new PrintWriter(Files.newBufferedWriter(diagAnalysisPath,
				StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING))) {
			final DialogueAnalysisWriter writer = new DialogueAnalysisWriter(out, 1);
			writer.accept(result);
		}
		LOGGER.info("Wrote dialogue analysis to \"{}\".", diagAnalysisPath);
	}

	private final Map<SessionDataManager, Path> allSessions;

	private final OptionalInt iterCount;

	private final Path outdir;

	private final Path summaryFile;

	public BatchApplicationContextTester(final Map<SessionDataManager, Path> allSessions, final OptionalInt iterCount,
			final Path outdir) {
		this.allSessions = allSessions;
		this.iterCount = iterCount;
		this.outdir = outdir;

		summaryFile = outdir.resolve("batch-summary.tsv");
	}

	/**
	 * @param appCtxDefPaths
	 * @throws IOException
	 * @throws ExecutionException 
	 * @throws ClassificationException 
	 */
	public void accept(final Iterable<Path> appCtxDefPaths) throws IOException, ClassificationException, ExecutionException {
		try (final BufferedWriter summaryWriter = Files.newBufferedWriter(summaryFile, StandardOpenOption.CREATE,
				StandardOpenOption.TRUNCATE_EXISTING)) {
			summaryWriter.write(COL_HEADERS.stream().collect(ROW_CELL_JOINER));
		}
		for (final Path appCtxDefPath : appCtxDefPaths) {
			accept(appCtxDefPath, StandardOpenOption.APPEND);
		}
	}

	private void accept(final Path appCtxDefPath, final OpenOption... summaryFileOpenOpts) throws IOException, ClassificationException, ExecutionException {
		final String appCtxDefLoc = appCtxDefPath.toString();
		final String batchOutdirName = createBatchOutdirName(appCtxDefPath);
		final Path outdirPath = Files.createDirectories(outdir.resolve(batchOutdirName));
		LOGGER.info("Will write results of testing \"{}\" to \"{}\".", appCtxDefPath, outdirPath);

		final Tester.Result testResults;
		try (final FileSystemXmlApplicationContext appCtx = new FileSystemXmlApplicationContext(appCtxDefLoc)) {
			final Tester tester = appCtx.getBean(Tester.class);
			iterCount.ifPresent(tester::setIterCount);
			testResults = tester.apply(allSessions);
		}
		writeResults(testResults, outdirPath);
		final List<Object> configSummary = StatisticsWriter.createSummaryTableRow(appCtxDefPath.toString(),
				testResults);
		try (final BufferedWriter summaryWriter = Files.newBufferedWriter(summaryFile, summaryFileOpenOpts)) {
			summaryWriter.newLine();
			summaryWriter.write(configSummary.stream().map(Object::toString).collect(ROW_CELL_JOINER));
		}
	}

}