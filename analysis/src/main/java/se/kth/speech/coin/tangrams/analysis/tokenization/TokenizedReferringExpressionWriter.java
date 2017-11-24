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
package se.kth.speech.coin.tangrams.analysis.tokenization;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.Future;
import java.util.function.BiConsumer;
import java.util.function.Supplier;
import java.util.regex.Pattern;
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

import edu.stanford.nlp.trees.Tree;
import edu.stanford.nlp.util.CoreMap;
import se.kth.speech.MapCollectors;
import se.kth.speech.coin.tangrams.analysis.UtteranceTabularDataWriter;
import se.kth.speech.coin.tangrams.analysis.dialogues.transformation.ChainedEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.io.SessionDataManager;
import se.kth.speech.nlp.stanford.AnnotationCacheFactory;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 12 Nov 2017
 *
 */
final class TokenizedReferringExpressionWriter { // NO_UCD (unused code)

	private enum Parameter implements Supplier<Option> {
		CLEANING("c") {
			@Override
			public Option get() {
				final Cleaning[] possibleVals = Cleaning.values();
				return Option.builder(optName).longOpt("cleaning")
						.desc(String.format(
								"A list of cleaning method(s) to use. Possible values: %s; Default values: %s",
								Arrays.toString(possibleVals), DEFAULT_CLEANING_METHOD_SET))
						.hasArg().argName("names").build();
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
		TOKEN_FILTER("tf") {
			@Override
			public Option get() {
				final TokenFiltering[] possibleVals = TokenFiltering.values();
				return Option.builder(optName).longOpt("token-filters")
						.desc(String.format("The token filtering method to use. Possible values: %s; Default value: %s",
								Arrays.toString(possibleVals), DEFAULT_TOKEN_FILTER))
						.hasArg().argName("name").build();
			}
		},
		TOKEN_TYPE("tt") {
			@Override
			public Option get() {
				final TokenType[] possibleVals = TokenType.values();
				return Option.builder(optName).longOpt("token-types")
						.desc(String.format("The token type to use. Possible values: %s; Default value: %s",
								Arrays.toString(possibleVals), DEFAULT_TOKEN_TYPE))
						.hasArg().argName("name").build();
			}
		},
		TOKENIZER("tok") {
			@Override
			public Option get() {
				final Tokenization[] possibleVals = Tokenization.values();
				return Option.builder(optName).longOpt("tokenizers")
						.desc(String.format("The tokenization method to use. Possible values: %s; Default value: %s",
								Arrays.toString(possibleVals), DEFAULT_TOKENIZATION_METHOD))
						.hasArg().argName("name").build();
			}
		};

		private static final Set<Cleaning> DEFAULT_CLEANING_METHOD_SET = EnumSet.allOf(Cleaning.class);

		private static final TokenFiltering DEFAULT_TOKEN_FILTER = TokenFiltering.STOPWORDS;

		private static final TokenType DEFAULT_TOKEN_TYPE = TokenType.INFLECTED;

		private static final Tokenization DEFAULT_TOKENIZATION_METHOD = Tokenization.STANFORD_NPS_WITHOUT_PPS;

		private static final Pattern MULTI_OPT_VALUE_DELIMITER = Pattern.compile("\\s+");

		private static Set<Cleaning> parseCleaningMethods(final CommandLine cl) {
			final Set<Cleaning> result;
			final String optValue = cl.getOptionValue(Parameter.CLEANING.optName);
			if (optValue == null) {
				result = DEFAULT_CLEANING_METHOD_SET;
			} else {
				result = EnumSet.noneOf(Cleaning.class);
				final Stream<Cleaning> insts = MULTI_OPT_VALUE_DELIMITER.splitAsStream(optValue).map(Cleaning::valueOf);
				insts.forEach(result::add);
			}
			return result;
		}

		private static TokenFiltering parseTokenFilteringMethod(final CommandLine cl) {
			final String name = cl.getOptionValue(Parameter.TOKEN_FILTER.optName);
			return name == null ? DEFAULT_TOKEN_FILTER : TokenFiltering.valueOf(name);
		}

		private static Tokenization parseTokenizationMethod(final CommandLine cl) {
			final String name = cl.getOptionValue(Parameter.TOKENIZER.optName);
			return name == null ? DEFAULT_TOKENIZATION_METHOD : Tokenization.valueOf(name);
		}

		private static TokenType parseTokenType(final CommandLine cl) {
			final String name = cl.getOptionValue(Parameter.TOKEN_TYPE.optName);
			return name == null ? DEFAULT_TOKEN_TYPE : TokenType.valueOf(name);
		}

		protected final String optName;

		private Parameter(final String optName) {
			this.optName = optName;
		}
	}

	/**
	 * Parsing can take up huge amounts of memory, so it's single-threaded and
	 * thus the caches are created designed for single-threaded operation.
	 */
	private static final AnnotationCacheFactory ANNOTATION_CACHE_FACTORY = new AnnotationCacheFactory(1);

	private static final String DEFAULT_OUTFILE_NAME = "utt-referring-tokens.tsv";

	private static final BiConsumer<CoreMap, List<Tree>> EXTRACTED_PHRASE_HANDLER = (sent, extractedPhrases) -> {
		// Do nothing
	};

	private static final Logger LOGGER = LoggerFactory.getLogger(TokenizedReferringExpressionWriter.class);

	private static final Options OPTIONS = createOptions();

	public static void main(final CommandLine cl)
			throws ParseException, IOException, JAXBException, InterruptedException, ExecutionException {
		final SortedSet<Path> inpaths = cl.getArgList().stream().map(String::trim).filter(path -> !path.isEmpty())
				.map(Paths::get).collect(Collectors.toCollection(() -> new TreeSet<>()));
		if (inpaths.isEmpty()) {
			throw new MissingOptionException("No input path(s) specified.");
		} else {
			LOGGER.info("Reading session data underneath {}.", inpaths);
			final Future<Map<SessionDataManager, Path>> sessionDataFuture = ForkJoinPool.commonPool()
					.submit(() -> readTestSessionData(inpaths));
			final Path outDir = ((File) cl.getParsedOptionValue(Parameter.OUTPATH.optName)).toPath().toAbsolutePath();
			LOGGER.info("Will write output underneath directory \"{}\".", outDir);
			final String outfileName = cl.getOptionValue(Parameter.OUTFILE_NAME.optName, DEFAULT_OUTFILE_NAME);
			LOGGER.info("Will name output files \"{}\".", outfileName);
			final Set<Cleaning> cleaningMethodSet = Parameter.parseCleaningMethods(cl);
			LOGGER.info("Cleaning method set: {}", cleaningMethodSet);
			final TokenFiltering tokenFilteringMethod = Parameter.parseTokenFilteringMethod(cl);
			LOGGER.info("Token filtering method: {}", tokenFilteringMethod);
			final Tokenization tokenizationMethod = Parameter.parseTokenizationMethod(cl);
			LOGGER.info("Tokenization method: {}", tokenizationMethod);
			final TokenType tokenType = Parameter.parseTokenType(cl);
			LOGGER.info("Token type: {}", tokenType);
			final Tokenization.Context tokenizationContext = new Tokenization.Context(cleaningMethodSet, tokenType,
					EXTRACTED_PHRASE_HANDLER, ANNOTATION_CACHE_FACTORY);
			final ChainedEventDialogueTransformer diagTransformer = new ChainedEventDialogueTransformer(
					Arrays.asList(tokenizationMethod.apply(tokenizationContext), tokenFilteringMethod.get()));
			final UtteranceTabularDataWriter writer = new UtteranceTabularDataWriter(outDir, outfileName,
					diagTransformer);
			writer.accept(sessionDataFuture);
		}
	}

	public static void main(final String[] args)
			throws IOException, JAXBException, InterruptedException, ExecutionException {
		final CommandLineParser parser = new DefaultParser();
		try {
			final CommandLine cl = parser.parse(OPTIONS, args);
			main(cl);
		} catch (final ParseException e) {
			System.out.println(String.format("An error occured while parsing the command-line arguments: %s", e));
			printHelp();
		}
	}

	private static Options createOptions() {
		final Options result = new Options();
		Arrays.stream(Parameter.values()).map(Parameter::get).forEach(result::addOption);
		return result;
	}

	private static void printHelp() {
		final HelpFormatter formatter = new HelpFormatter();
		formatter.printHelp(TokenizedReferringExpressionWriter.class.getSimpleName() + " INPATHS...", OPTIONS);
	}

	private static Map<SessionDataManager, Path> readTestSessionData(final Iterable<Path> inpaths) throws IOException {
		final Map<Path, SessionDataManager> infileSessionData = SessionDataManager.createFileSessionDataMap(inpaths);
		final Map<SessionDataManager, Path> result = infileSessionData.entrySet().stream()
				.collect(Collectors.toMap(Entry::getValue, Entry::getKey, MapCollectors.throwingMerger(),
						() -> new HashMap<>(infileSessionData.size() + 1, 1.0f)));
		infileSessionData.forEach((infile, sessionData) -> result.put(sessionData, infile));
		return result;
	}

	private TokenizedReferringExpressionWriter() {
	}

}
