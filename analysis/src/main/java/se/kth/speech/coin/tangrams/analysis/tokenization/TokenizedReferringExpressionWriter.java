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
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.Function;
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

import com.google.common.collect.BiMap;
import com.google.common.collect.Maps;

import edu.stanford.nlp.trees.Tree;
import edu.stanford.nlp.util.CoreMap;
import se.kth.speech.CommonPaths;
import se.kth.speech.MapCollectors;
import se.kth.speech.coin.tangrams.analysis.SessionGame;
import se.kth.speech.coin.tangrams.analysis.SessionGameManager;
import se.kth.speech.coin.tangrams.analysis.UtteranceSpeakerParticipantIdMapFactory;
import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues.ChainedEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues.EventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.io.SessionDataManager;
import se.kth.speech.coin.tangrams.game.PlayerRole;
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
						.hasArg().argName("names").required().build();
			}
		},
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
		TOKEN_FILTER("tf") {
			@Override
			public Option get() {
				final TokenFiltering[] possibleVals = TokenFiltering.values();
				return Option.builder(optName).longOpt("token-filters")
						.desc("The filtering method to use. Possible values: " + Arrays.toString(possibleVals)).hasArg()
						.argName("name").required().build();
			}
		},
		TOKEN_TYPE("tt") {
			@Override
			public Option get() {
				final TokenType[] possibleVals = TokenType.values();
				return Option.builder(optName).longOpt("token-types")
						.desc("The token type to use. Possible values: " + Arrays.toString(possibleVals)).hasArg()
						.argName("name").required().build();
			}
		},
		TOKENIZER("tok") {
			@Override
			public Option get() {
				final Tokenization[] possibleVals = Tokenization.values();
				return Option.builder(optName).longOpt("tokenizers")
						.desc("The tokenization method to use. Possible values: " + Arrays.toString(possibleVals))
						.hasArg().argName("name").required().build();
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

		private static TokenFiltering parseTokenFilteringMethod(final CommandLine cl) {
			final String name = cl.getOptionValue(Parameter.TOKEN_FILTER.optName);
			return TokenFiltering.valueOf(name);
		}

		private static Tokenization parseTokenizationMethod(final CommandLine cl) {
			final String name = cl.getOptionValue(Parameter.TOKENIZER.optName);
			return Tokenization.valueOf(name);
		}

		private static TokenType parseTokenType(final CommandLine cl) {
			final String name = cl.getOptionValue(Parameter.TOKEN_TYPE.optName);
			return TokenType.valueOf(name);
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

	private static final String DEFAULT_OUTFILE_NAME = "extracted-referring-tokens.tsv";

	private static final BiConsumer<CoreMap, List<Tree>> EXTRACTED_PHRASE_HANDLER = (sent, extractedPhrases) -> {
		// Do nothing
	};

	private static final Logger LOGGER = LoggerFactory.getLogger(TokenizedReferringExpressionWriter.class);

	private static final Options OPTIONS = createOptions();

	private static final String OUTFILE_HEADER;

	private static final Charset OUTPUT_ENCODING = StandardCharsets.UTF_8;

	private static final UtteranceSpeakerParticipantIdMapFactory PLAYER_PARTICIPANT_ID_MAPPER = new UtteranceSpeakerParticipantIdMapFactory();

	private static final Collector<CharSequence, ?, String> ROW_CELL_JOINER;

	private static final Collector<CharSequence, ?, String> TOKEN_JOINER = Collectors.joining(" ");

	static {
		ROW_CELL_JOINER = Collectors.joining("\t");
		final List<String> colHeaders = Arrays.asList("ROUND", "SPEAKER", "START_TIME", "END_TIME", "UTTERANCE",
				"REFERRING_TOKENS");
		OUTFILE_HEADER = colHeaders.stream().collect(ROW_CELL_JOINER);
	}

	public static void main(final String[] args) throws IOException, JAXBException {
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

	private static void main(final CommandLine cl) throws ParseException, IOException, JAXBException {
		if (cl.hasOption(Parameter.HELP.optName)) {
			printHelp();
		} else {
			final List<Path> inpaths = Arrays.asList(cl.getArgList().stream().map(String::trim)
					.filter(path -> !path.isEmpty()).map(Paths::get).toArray(Path[]::new));
			if (inpaths.isEmpty()) {
				throw new MissingOptionException("No input path(s) specified.");
			} else {
				final Map<SessionDataManager, Path> allSessionData = readTestSessionData(inpaths);
				final LoggedEventReader eventReader = new LoggedEventReader(allSessionData.size(),
						allSessionData.size() * 10);

				final Set<Cleaning> cleaningMethodSet = Parameter.parseCleaningMethods(cl);
				if (cleaningMethodSet.isEmpty()) {
					throw new IllegalArgumentException("No cleaning method set(s) specified.");
				}
				LOGGER.info("Cleaning method set: {}", cleaningMethodSet);
				final TokenFiltering tokenFilteringMethod = Parameter.parseTokenFilteringMethod(cl);
				LOGGER.info("Token filtering method: {}", tokenFilteringMethod);
				final Tokenization tokenizationMethod = Parameter.parseTokenizationMethod(cl);
				LOGGER.info("Tokenization method: {}", tokenizationMethod);
				final TokenType tokenType = Parameter.parseTokenType(cl);
				LOGGER.info("Token type: {}", tokenType);

				final EventDialogueTransformer tokenFilter = tokenFilteringMethod.get();
				final Path outDir = ((File) cl.getParsedOptionValue(Parameter.OUTPATH.optName)).toPath()
						.toAbsolutePath();
				LOGGER.info("Will write output underneath directory \"{}\".", outDir);
				final String outfileName = cl.getOptionValue(Parameter.OUTFILE_NAME.optName, DEFAULT_OUTFILE_NAME);
				LOGGER.info("Will name output files \"{}\".", outfileName);

				final Path sessionPrefixPath = CommonPaths.findCommonPrefixPath(allSessionData.values().stream());
				LOGGER.info("Found a common path of \"{}\" for all input sessions.", sessionPrefixPath);

				for (final Entry<SessionDataManager, Path> sessionDataPath : allSessionData.entrySet()) {
					final SessionDataManager sessionDataMgr = sessionDataPath.getKey();
					final Path sessionPropsFilePath = sessionDataPath.getValue().toAbsolutePath();
					final Path sessionDir = sessionPropsFilePath.getParent();
					assert sessionDir != null;
					final Path relativeSessionDir = sessionPrefixPath.relativize(sessionDir);
					final Path sessionOutputDir = Files.createDirectories(outDir.resolve(relativeSessionDir));

					final Tokenization.Context tokenizationContext = new Tokenization.Context(cleaningMethodSet,
							tokenType, EXTRACTED_PHRASE_HANDLER, ANNOTATION_CACHE_FACTORY);
					final ChainedEventDialogueTransformer diagTransformer = new ChainedEventDialogueTransformer(
							Arrays.asList(tokenizationMethod.apply(tokenizationContext), tokenFilter));

					final SessionGameManager sessionGameMgr = new SessionGameManager(sessionDataMgr, eventReader);
					final SessionGame sessionGame = sessionGameMgr.getCanonicalGame();
					final BiMap<PlayerRole, String> playerRoles = sessionGame.getHistory().getInitialState()
							.getPlayerRoles();
					final BiMap<String, String> playerParticipantIds = PLAYER_PARTICIPANT_ID_MAPPER.apply(playerRoles);
					final List<EventDialogue> evtDiags = sessionGame.getEventDialogues();
					// Trim initial pre-game dialogue if present
					final List<EventDialogue> evtDiagsToPrint = evtDiags.get(0).getFirstEvent().isPresent() ? evtDiags : evtDiags.subList(1, evtDiags.size());

					final List<String> rows = new ArrayList<>(evtDiagsToPrint.size() * 4 + 1);
					rows.add(OUTFILE_HEADER);
					for (final ListIterator<EventDialogue> evtDiagIter = evtDiagsToPrint.listIterator(); evtDiagIter
							.hasNext();) {
						final EventDialogue evtDiag = evtDiagIter.next();
						final List<Utterance> origUtts = evtDiag.getUtterances();
						final Map<String, Utterance> origUttsById = origUtts.stream()
								.collect(Collectors.toMap(Utterance::getSegmentId, Function.identity(),
										MapCollectors.throwingMerger(),
										() -> Maps.newHashMapWithExpectedSize(origUtts.size())));
						// Round ID is 1-indexed
						final int roundId = evtDiagIter.nextIndex();
						final EventDialogue transformedDiag = diagTransformer.apply(evtDiag);
						for (final Utterance utterance : transformedDiag.getUtterances()) {
							final String segId = utterance.getSegmentId();
							final Utterance origUtt = origUttsById.get(segId);
							final String speakerId = utterance.getSpeakerId();
							assert origUtt.getSpeakerId().equals(speakerId);
							final String participantId = playerParticipantIds.get(speakerId);
							assert origUtt.getStartTime() == utterance.getStartTime();
							assert origUtt.getEndTime() == utterance.getEndTime();
							final List<Object> rowCells = Arrays.asList(roundId, participantId,
									utterance.getStartTime(), utterance.getEndTime(),
									origUtt.getTokens().stream().collect(TOKEN_JOINER),
									utterance.getTokens().stream().collect(TOKEN_JOINER));
							rows.add(rowCells.stream().map(Object::toString).collect(ROW_CELL_JOINER));
						}
					}

					final Path outfile = sessionOutputDir.resolve(outfileName);
					LOGGER.info("Writing data extracted from \"{}\" to \"{}\".", sessionPropsFilePath, outfile);
					Files.write(outfile, rows, OUTPUT_ENCODING, StandardOpenOption.CREATE,
							StandardOpenOption.TRUNCATE_EXISTING);
				}
			}

		}
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
