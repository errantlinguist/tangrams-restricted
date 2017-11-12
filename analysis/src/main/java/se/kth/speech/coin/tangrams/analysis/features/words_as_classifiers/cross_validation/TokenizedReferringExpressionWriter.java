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
import java.util.EnumSet;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.Supplier;
import java.util.regex.Pattern;
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

import edu.stanford.nlp.ling.CoreAnnotations.TextAnnotation;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.trees.Tree;
import edu.stanford.nlp.util.CoreMap;
import se.kth.speech.coin.tangrams.CLIParameters;
import se.kth.speech.coin.tangrams.analysis.SessionGame;
import se.kth.speech.coin.tangrams.analysis.SessionGameManager;
import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues.ChainedEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues.EventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.io.SessionDataManager;
import se.kth.speech.coin.tangrams.iristk.io.LoggedEventReader;
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
						.desc("A list of cleaning method(s) to use. Possible values: " + Arrays.toString(possibleVals))
						.hasArg().argName("name").build();
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
				return Option.builder(optName).longOpt("outpath").desc("The directory to write the extracted data to.")
						.hasArg().argName("path").type(File.class).required().build();
			}
		},
		TOKEN_FILTER("tf") {
			@Override
			public Option get() {
				final TokenFiltering[] possibleVals = TokenFiltering.values();
				return Option.builder(optName).longOpt("token-filters")
						.desc("The filtering method to use. Possible values: " + Arrays.toString(possibleVals)).hasArg()
						.argName("name").build();
			}
		},
		TOKENIZER("tok") {
			@Override
			public Option get() {
				final Tokenization[] possibleVals = Tokenization.values();
				return Option.builder(optName).longOpt("tokenizers")
						.desc("The tokenization method to use. Possible values: " + Arrays.toString(possibleVals))
						.hasArg().argName("name").build();
			}
		};

		private static final Pattern MULTI_OPT_VALUE_DELIMITER = Pattern.compile("\\s+");

		private static Set<Cleaning> parseCleaningMethods(final CommandLine cl) {
			final String[] names = parseOptEnumValueNames(cl, Parameter.CLEANING.optName);
			final Stream<Cleaning> insts = names == null ? Stream.empty()
					: Arrays.stream(names).map(String::trim).filter(str -> !str.isEmpty()).map(Cleaning::valueOf);
			final EnumSet<Cleaning> result = EnumSet.noneOf(Cleaning.class);
			insts.forEach(result::add);
			return result;
		}

		private static String[] parseOptEnumValueNames(final CommandLine cl, final String optName) {
			final String val = cl.getOptionValue(optName);
			return val == null ? null : MULTI_OPT_VALUE_DELIMITER.split(val);
		}

		static TokenFiltering parseTokenFilteringMethod(final CommandLine cl) {
			final String name = cl.getOptionValue(Parameter.TOKEN_FILTER.optName);
			return TokenFiltering.valueOf(name);
		}

		static Tokenization parseTokenizationMethod(final CommandLine cl) {
			final String name = cl.getOptionValue(Parameter.TOKENIZER.optName);
			return Tokenization.valueOf(name);
		}

		protected final String optName;

		private Parameter(final String optName) {
			this.optName = optName;
		}
	}

	private static class UtteranceReferringExpressionWriter implements BiConsumer<CoreMap, List<Tree>> {

		@Override
		public void accept(final CoreMap sent, final List<Tree> extractedPhrases) {
			final String sentText = sent.get(TextAnnotation.class);

			for (final Tree extractedPhrase : extractedPhrases) {
				final List<CoreLabel> phraseLabels = extractedPhrase.taggedLabeledYield();
				for (final CoreLabel phraseLabel : phraseLabels) {
					final int beginPos = phraseLabel.beginPosition();
					final int endPos = phraseLabel.endPosition();
					final String word = phraseLabel.word();
					final int idx = phraseLabel.index();
					LOGGER.info("Word: {}; start: {}, end: {}; idx: {}", word, beginPos, endPos, idx);
					assert sentText.substring(beginPos, endPos).equals(word);
				}
			}

		}

	}

	/**
	 * Parsing can take up huge amounts of memory, so it's single-threaded and thus
	 * the caches are created designed for single-threaded operation.
	 */
	private static final AnnotationCacheFactory ANNOTATION_CACHE_FACTORY = new AnnotationCacheFactory(1);

	private static final Logger LOGGER = LoggerFactory.getLogger(TokenizedReferringExpressionWriter.class);

	private static final Options OPTIONS = createOptions();

	private static final Charset OUTPUT_ENCODING = StandardCharsets.UTF_8;

	private static final Collector<CharSequence, ?, String> ROW_CELL_JOINER = Collectors.joining("\t");

	public static void main(final String[] args) throws BatchJobTestException, IOException, JAXBException {
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
		final Set<Parameter> params = EnumSet.of(Parameter.CLEANING, Parameter.HELP, Parameter.OUTPATH,
				Parameter.TOKEN_FILTER, Parameter.TOKENIZER);
		params.stream().map(Parameter::get).forEach(result::addOption);
		return result;
	}

	private static void main(final CommandLine cl)
			throws BatchJobTestException, ParseException, IOException, JAXBException {
		if (cl.hasOption(Parameter.HELP.optName)) {
			printHelp();
		} else {
			final List<Path> inpaths = Arrays.asList(cl.getArgList().stream().map(String::trim)
					.filter(path -> !path.isEmpty()).map(Paths::get).toArray(Path[]::new));
			if (inpaths.isEmpty()) {
				throw new MissingOptionException("No input path(s) specified.");
			} else {
				final Map<SessionDataManager, Path> allSessionData = TestSessionData.readTestSessionData(inpaths);
				final Set<Cleaning> cleaningMethodSet = Parameter.parseCleaningMethods(cl);
				if (cleaningMethodSet.isEmpty()) {
					throw new IllegalArgumentException("No cleaning method set(s) specified.");
				}
				LOGGER.info("Cleaning method set: {}", cleaningMethodSet);
				final TokenFiltering tokenFilteringMethod = Parameter.parseTokenFilteringMethod(cl);
				LOGGER.info("Token filtering method: {}", tokenFilteringMethod);
				final Tokenization tokenizationMethod = Parameter.parseTokenizationMethod(cl);
				LOGGER.info("Tokenization method: {}", tokenizationMethod);
				final EventDialogueTransformer tokenFilter = tokenFilteringMethod.get();
				final LoggedEventReader eventReader = new LoggedEventReader(allSessionData.size(),
						allSessionData.size() * 10);
				for (final Entry<SessionDataManager, Path> sessionDataPath : allSessionData.entrySet()) {
					final SessionDataManager sessionDataMgr = sessionDataPath.getKey();
					final Path sessionDir = sessionDataPath.getValue();

					final TokenizationContext tokenizationContext = new TokenizationContext(cleaningMethodSet,
							TokenType.INFLECTED, new UtteranceReferringExpressionWriter(), ANNOTATION_CACHE_FACTORY);
					final ChainedEventDialogueTransformer diagTransformer = new ChainedEventDialogueTransformer(
							Arrays.asList(tokenizationMethod.apply(tokenizationContext), tokenFilter));

					final SessionGameManager sessionGameMgr = new SessionGameManager(sessionDataMgr, eventReader);
					final SessionGame sessionGame = sessionGameMgr.getCanonicalGame();
					for (final ListIterator<EventDialogue> evtDiagIter = sessionGame.getEventDialogues()
							.listIterator(); evtDiagIter.hasNext();) {
						final EventDialogue evtDiag = evtDiagIter.next();
						// Round ID is 1-indexed
						final int roundId = evtDiagIter.nextIndex();
						final EventDialogue transformedDiag = diagTransformer.apply(evtDiag);
					}
				}

				final File outFile = (File) cl.getParsedOptionValue(Parameter.OUTPATH.optName);
				try (PrintWriter out = CLIParameters.parseOutpath(outFile, OUTPUT_ENCODING)) {

				}
			}

		}
	}

	private static void printHelp() {
		final HelpFormatter formatter = new HelpFormatter();
		formatter.printHelp(TokenizedReferringExpressionWriter.class.getSimpleName() + " INPATHS...", OPTIONS);
	}

	private final PrintWriter out;

	private final boolean writeHeader;

	private TokenizedReferringExpressionWriter(final PrintWriter out, final boolean writeHeader) {
		this.out = out;
		this.writeHeader = writeHeader;
	}

}
