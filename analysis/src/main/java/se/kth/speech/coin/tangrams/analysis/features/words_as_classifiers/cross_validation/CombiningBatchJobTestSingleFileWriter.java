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
import java.util.Map;
import java.util.OptionalInt;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.Future;
import java.util.function.Consumer;
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

import com.google.common.collect.Sets;

import se.kth.speech.coin.tangrams.CLIParameters;
import se.kth.speech.coin.tangrams.analysis.SessionDataManager;
import se.kth.speech.coin.tangrams.analysis.features.ClassificationException;
import se.kth.speech.coin.tangrams.iristk.EventTimes;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 24 May 2017
 *
 */
public final class CombiningBatchJobTestSingleFileWriter implements Consumer<BatchJobSummary> {

	// private static final List<String> COL_HEADERS = Arrays.asList("INPATH",
	// "TEST_ITER", "SESSION_ORDER", "EVENT_TIME",
	// "DIALOGUE", "DIALOGUE_AS_TESTED", "GOLD_STD_ID", "RANK", "RR",
	// "TESTED_UTT_COUNT", "TOTAL_UTT_COUNT",
	// "MEAN_DIAG_UTTS_TESTED", "TOKEN_COUNT", "MEAN_TOKENS_PER_UTT");
	public enum SummaryDatum {
		SESSION_FILE, TEST_ITER, SESSION_ORDER, EVENT_TIME, DIALOGUE, DIALOGUE_AS_TESTED, GOLD_STD_ENTITY_ID, RANK, TESTED_UTT_COUNT, TOTAL_UTT_COUNT, TOKEN_COUNT, TEST_TIME;
	}

	private enum Parameter implements Supplier<Option> {
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
		UTT_PROCESSING("u") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("token-filters")
						.desc("A list of token processing option(s) to use.").hasArgs().argName("name").build();
			}
		},
		TRAINING("t") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("training").desc("A list of training method(s) to use.")
						.hasArgs().argName("name").build();
			}
		};

		private static final Options OPTIONS = createOptions();

		private static Options createOptions() {
			final Options result = new Options();
			Arrays.stream(Parameter.values()).map(Parameter::get).forEach(result::addOption);
			return result;
		}

		private static Set<Training> parseTrainingMethods(final CommandLine cl) {
			final String[] names = cl.getOptionValues(Parameter.TRAINING.optName);
			final Stream<Training> insts = names == null ? Arrays.stream(Training.values())
					: Arrays.stream(names).map(Training::valueOf);
			final Set<Training> result = EnumSet.noneOf(Training.class);
			insts.forEach(result::add);
			return result;
		}

		private static Set<UtteranceProcessingOption> parseUtteranceProcessingOptions(final CommandLine cl) {
			final String[] names = cl.getOptionValues(Parameter.UTT_PROCESSING.optName);
			final Stream<UtteranceProcessingOption> insts = names == null
					? Arrays.stream(UtteranceProcessingOption.values())
					: Arrays.stream(names).map(UtteranceProcessingOption::valueOf);
			final Set<UtteranceProcessingOption> result = EnumSet.noneOf(UtteranceProcessingOption.class);
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

	private static final Logger LOGGER = LoggerFactory.getLogger(CombiningBatchJobTestSingleFileWriter.class);

	private static final Collector<CharSequence, ?, String> METHOD_KEY_NAME_JOINER = Collectors.joining("_");

	private static final Collector<CharSequence, ?, String> ROW_CELL_JOINER = Collectors.joining("\t");

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
				final PrintWriter out = CLIParameters
						.parseOutpath((File) cl.getParsedOptionValue(Parameter.OUTPATH.optName));
				final Set<UtteranceProcessingOption> uttProcessingOpts = Parameter.parseUtteranceProcessingOptions(cl);
				LOGGER.info("Utterance processing options: {}", uttProcessingOpts);
				final Set<Training> trainingMethods = Parameter.parseTrainingMethods(cl);
				LOGGER.info("Training methods: {}", trainingMethods);
				final ExecutorService backgroundJobExecutor = createBackgroundJobExecutor();
				final Future<Set<Set<UtteranceProcessingOption>>> futureUttProcessingOptSets = backgroundJobExecutor
						.submit(() -> Sets.powerSet(uttProcessingOpts));
				try {
					final Future<Map<SessionDataManager, Path>> allSessionDataFuture = backgroundJobExecutor
							.submit(() -> TestSessionData.readTestSessionData(inpaths));
					final CombiningBatchJobTestSingleFileWriter writer = new CombiningBatchJobTestSingleFileWriter(out);
					try (final ClassPathXmlApplicationContext appCtx = new ClassPathXmlApplicationContext(
							"combining-batch-tester.xml", CombiningBatchJobTestSingleFileWriter.class)) {
						final CombiningBatchJobTester tester = new CombiningBatchJobTester(backgroundJobExecutor,
								appCtx, writer, testerConfigurator);
						final Set<Set<UtteranceProcessingOption>> uttProcessingOptSets = futureUttProcessingOptSets
								.get();
						LOGGER.info("Testing {} combination(s) of utterance processing options.", uttProcessingOptSets.size());
						final CombiningBatchJobTester.Input input = new CombiningBatchJobTester.Input(
								uttProcessingOptSets, trainingMethods, allSessionDataFuture.get());
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

	private static String createBatchOutdirName(final TestParameters testParams) {
		final Stream<String> rowCellVals = createTestMethodRowCellValues(testParams);
		return rowCellVals.collect(METHOD_KEY_NAME_JOINER);
	}

	private static List<String> createColHeaderList(final List<SummaryDatum> summaryDataToWrite) {
		final Stream.Builder<String> resultBuilder = Stream.builder();
		resultBuilder.add("TIME");
		createTestMethodColumnHeaders().forEachOrdered(resultBuilder);
		resultBuilder.add("OUTDIR");
		resultBuilder.add("ITER_COUNT");
		summaryDataToWrite.stream().map(SummaryDatum::toString).forEachOrdered(resultBuilder);
		return Arrays.asList(resultBuilder.build().toArray(String[]::new));
	}

	private static Map<SummaryDatum, Object> createRowCellValueMap(final BatchJobSummary summary) {
		final Map<SummaryDatum, Object> result = new EnumMap<>(SummaryDatum.class);
		final TestParameters testParams = summary.getTestParams();
		final Tester.Result testResults = summary.getTestResults();

		assert result.size() == SummaryDatum.values().length;
		return result;
	}

	private static Stream<String> createTestMethodColumnHeaders() {
		return Stream.of(UtteranceProcessingOption.class.getSimpleName(), Training.class.getSimpleName());
	}

	private static Stream<String> createTestMethodRowCellValues(final TestParameters testParams) {
		return Stream.of(testParams.getUttProcessingMethodDesc(), testParams.getTrainingMethod().getAbbreviation());
	}

	private static void shutdownExceptionally(final ExecutorService executor) {
		LOGGER.debug("Emergency executor service shutdown.");
		executor.shutdownNow();
		LOGGER.debug("Successfully shut down executor service.");
	}

	private final PrintWriter out;

	public CombiningBatchJobTestSingleFileWriter(final PrintWriter out) {
		this.out = out;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Consumer#accept(java.lang.Object)
	 */
	@Override
	public void accept(final BatchJobSummary summary) {
		final TestParameters testParams = summary.getTestParams();
		final Tester.Result testResults = summary.getTestResults();
	}

}
