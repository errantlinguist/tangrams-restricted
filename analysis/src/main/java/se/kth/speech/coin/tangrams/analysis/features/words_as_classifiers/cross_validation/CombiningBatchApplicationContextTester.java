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
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.Arrays;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.OptionalInt;
import java.util.Properties;
import java.util.Random;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.function.BiFunction;
import java.util.function.Function;
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
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import edu.stanford.nlp.pipeline.Annotator;
import edu.stanford.nlp.pipeline.StanfordCoreNLP;
import edu.stanford.nlp.trees.CollinsHeadFinder;
import se.kth.speech.coin.tangrams.analysis.SessionDataManager;
import se.kth.speech.coin.tangrams.analysis.SessionEventDialogueManagerCacheSupplier;
import se.kth.speech.coin.tangrams.analysis.features.ClassificationException;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.CachingEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.ChainedEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.DummyEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.EventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.InstructorUtteranceFilteringEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.TokenFilteringEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.TokenizingEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.OnePositiveMaximumNegativeInstancesFactory;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.OnePositiveOneNegativeInstanceFactory;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.TrainingInstancesFactory;
import se.kth.speech.io.FileNames;
import se.kth.speech.nlp.EnglishLocationalPrepositions;
import se.kth.speech.nlp.PhrasalHeadFilteringPredicate;
import se.kth.speech.nlp.SnowballPorter2EnglishStopwordSetFactory;
import se.kth.speech.nlp.SnowballPorter2EnglishStopwords;
import se.kth.speech.nlp.StanfordCoreNLPLemmatizer;
import se.kth.speech.nlp.StanfordCoreNLPParsingTokenizer;
import se.kth.speech.nlp.StanfordCoreNLPTokenizer;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 24 May 2017
 *
 */
public final class CombiningBatchApplicationContextTester {

	public static final class Input {

		private final StanfordCoreNLP stanfordPipeline;

		private final TokenFiltering tokenFilterFactory;

		private final Tokenization tokenizerFactory;

		private final UtteranceFiltering uttChooserFactory;

		public Input(final StanfordCoreNLP stanfordPipeline, final UtteranceFiltering uttChooserFactory,
				final Tokenization tokenizerFactory, final TokenFiltering tokenFilterFactory) {
			this.stanfordPipeline = stanfordPipeline;
			this.uttChooserFactory = uttChooserFactory;
			this.tokenizerFactory = tokenizerFactory;
			this.tokenFilterFactory = tokenFilterFactory;

		}
	}

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
			formatter.printHelp(CombiningBatchApplicationContextTester.class.getSimpleName() + " INPATHS...", OPTIONS);
		}

		protected final String optName;

		private Parameter(final String optName) {
			this.optName = optName;
		}

	}

	private enum TokenFiltering implements Supplier<EventDialogueTransformer> {
		NO_FILTER(DUMMY_EVT_DIAG_TRANSFORMER), STOPWORDS_FILLERS(createStopwordFilteringTransformer());

		private final EventDialogueTransformer held;

		private TokenFiltering(final EventDialogueTransformer held) {
			this.held = held;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.util.function.Supplier#get()
		 */
		@Override
		public EventDialogueTransformer get() {
			return held;
		}

	}

	private enum Tokenization implements Function<Annotator, TokenizingEventDialogueTransformer> {
		BASIC_TOKENIZER {

			/*
			 * (non-Javadoc)
			 *
			 * @see java.util.function.Function#apply(java.lang.Object)
			 */
			@Override
			public TokenizingEventDialogueTransformer apply(final Annotator annotator) {
				return new TokenizingEventDialogueTransformer(new StanfordCoreNLPTokenizer(annotator));
			}
		},
		LEMMATIZER {

			/*
			 * (non-Javadoc)
			 *
			 * @see java.util.function.Function#apply(java.lang.Object)
			 */
			@Override
			public TokenizingEventDialogueTransformer apply(final Annotator annotator) {
				return new TokenizingEventDialogueTransformer(new StanfordCoreNLPLemmatizer(annotator));
			}
		},
		PARSING_TOKENIZER {

			/*
			 * (non-Javadoc)
			 *
			 * @see java.util.function.Function#apply(java.lang.Object)
			 */
			@Override
			public TokenizingEventDialogueTransformer apply(final Annotator annotator) {
				final Map<String, Set<List<String>>> labelHeadBlacklists = Collections.singletonMap("PP",
						EnglishLocationalPrepositions.loadSet());
				final PhrasalHeadFilteringPredicate pred = new PhrasalHeadFilteringPredicate(labelHeadBlacklists,
						new CollinsHeadFinder());
				return new TokenizingEventDialogueTransformer(new StanfordCoreNLPParsingTokenizer(annotator, pred));
			}
		};
	}

	private enum TrainingMethod
			implements BiFunction<EventDialogueTransformer, ApplicationContext, TrainingInstancesFactory> {
		ALL_NEG {
			@Override
			public OnePositiveMaximumNegativeInstancesFactory apply(final EventDialogueTransformer diagTransformer,
					final ApplicationContext appCtx) {
				final OnePositiveMaximumNegativeInstancesFactory result = appCtx
						.getBean(OnePositiveMaximumNegativeInstancesFactory.class, diagTransformer);
				return result;
			}
		},
		ONE_NEG {

			@Override
			public OnePositiveOneNegativeInstanceFactory apply(final EventDialogueTransformer diagTransformer,
					final ApplicationContext appCtx) {
				final OnePositiveOneNegativeInstanceFactory result = appCtx
						.getBean(OnePositiveOneNegativeInstanceFactory.class, diagTransformer, RND);
				return result;
			}

		};
	}

	private enum UtteranceFiltering implements Supplier<EventDialogueTransformer> {
		ALL_UTTS(DUMMY_EVT_DIAG_TRANSFORMER), INSTRUCTOR_UTTS(
				new InstructorUtteranceFilteringEventDialogueTransformer());

		private final EventDialogueTransformer held;

		private UtteranceFiltering(final EventDialogueTransformer held) {
			this.held = held;
		}

		@Override
		public EventDialogueTransformer get() {
			return held;
		}
	}

	private static final List<String> COL_HEADERS = Arrays.asList("INPATH", "ITER_COUNT", "MEAN_RANK", "MRR",
			"DIAG_COUNT", "UTTS_TESTED", "MEAN_UTTS_PER_DIAG");

	private static final DummyEventDialogueTransformer DUMMY_EVT_DIAG_TRANSFORMER = new DummyEventDialogueTransformer();

	private static final Logger LOGGER = LoggerFactory.getLogger(CombiningBatchApplicationContextTester.class);

	private static final Random RND = new Random(1);

	private static final Collector<CharSequence, ?, String> ROW_CELL_JOINER = Collectors.joining("\t");

	public static void main(final CommandLine cl)
			throws IOException, ParseException, ClassificationException, ExecutionException {
		if (cl.hasOption(Parameter.HELP.optName)) {
			Parameter.printHelp();
		} else {
			final List<Path> inpaths = Arrays.asList(cl.getArgList().stream().map(Paths::get).toArray(Path[]::new));
			if (inpaths.isEmpty()) {
				throw new MissingOptionException("No input path(s) specified.");

			} else {
				final OptionalInt iterCount = Parameter.parseIterCount(cl);
				final Path outdir = ((File) cl.getParsedOptionValue(Parameter.OUTPATH.optName)).toPath();
				LOGGER.info("Will write data to \"{}\".", outdir);

				final Map<SessionDataManager, Path> allSessions = TestSessionData.readTestSessionData(inpaths);
				// final CombiningBatchApplicationContextTester tester = new
				// CombiningBatchApplicationContextTester(
				// allSessions, iterCount, outdir);

				final StanfordCoreNLP stanfordPipeline = new StanfordCoreNLP(createAnnotationPipelineProps());

				try (final ClassPathXmlApplicationContext appCtx = new ClassPathXmlApplicationContext("context.xml",
						CombiningBatchApplicationContextTester.class)) {
					final SessionEventDialogueManagerCacheSupplier sessionDiagMgrCacheSupplier = appCtx
							.getBean(SessionEventDialogueManagerCacheSupplier.class);

					for (final UtteranceFiltering uttFilteringMethod : UtteranceFiltering.values()) {
						final EventDialogueTransformer uttFilter = uttFilteringMethod.get();
						for (final Tokenization tokenizationMethod : Tokenization.values()) {
							final TokenizingEventDialogueTransformer tokenizer = tokenizationMethod
									.apply(stanfordPipeline);

							for (final TokenFiltering tokenFilteringMethod : TokenFiltering.values()) {
								final EventDialogueTransformer tokenFilter = tokenFilteringMethod.get();

								final List<EventDialogueTransformer> diagTransformers = Arrays.asList(uttFilter,
										tokenizer, tokenFilter);
								final CachingEventDialogueTransformer cachingDiagTransformer = new CachingEventDialogueTransformer(
										new ChainedEventDialogueTransformer(diagTransformers));
								final Tester tester = appCtx.getBean(Tester.class, cachingDiagTransformer);

								for (final TrainingMethod trainingMethod : TrainingMethod.values()) {
									final TrainingInstancesFactory trainingInstsFactory = trainingMethod
											.apply(cachingDiagTransformer, appCtx);
									final TestSetFactory testSetFactory = new TestSetFactory(trainingInstsFactory,
											sessionDiagMgrCacheSupplier);

								}

							}

						}
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

	private static Properties createAnnotationPipelineProps() {
		final Properties result = new Properties();
		// https://stanfordnlp.github.io/CoreNLP/api.html
		// https://stanfordnlp.github.io/CoreNLP/annotators.html
		result.setProperty("annotators", "tokenize,ssplit,pos,lemma,parse");
		// https://stanfordnlp.github.io/CoreNLP/parse.html
		result.setProperty("parse.model", "edu/stanford/nlp/models/lexparser/englishPCFG.caseless.ser.gz");
		// https://stanfordnlp.github.io/CoreNLP/pos.html
		result.setProperty("pos.model",
				"edu/stanford/nlp/models/pos-tagger/english-caseless-left3words-distsim.tagger");
		// https://stanfordnlp.github.io/CoreNLP/ssplit.html
		result.setProperty("ssplit.isOneSentence", "true");
		// https://stanfordnlp.github.io/CoreNLP/tokenize.html
		result.setProperty("tokenize.language", "en");
		return result;
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

	private static TokenFilteringEventDialogueTransformer createStopwordFilteringTransformer() {
		final SnowballPorter2EnglishStopwordSetFactory factory = new SnowballPorter2EnglishStopwordSetFactory();
		factory.setVariantsToUnify(EnumSet.of(SnowballPorter2EnglishStopwords.Variant.CANONICAL,
				SnowballPorter2EnglishStopwords.Variant.FILLERS));
		try {
			return new TokenFilteringEventDialogueTransformer(factory.getObject());
		} catch (final IOException e) {
			throw new UncheckedIOException(e);
		}
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

	public CombiningBatchApplicationContextTester(final Map<SessionDataManager, Path> allSessions,
			final OptionalInt iterCount, final Path outdir) {
		this.allSessions = allSessions;
		this.iterCount = iterCount;
		this.outdir = outdir;

		summaryFile = outdir.resolve("batch-summary.tsv");
	}

}
