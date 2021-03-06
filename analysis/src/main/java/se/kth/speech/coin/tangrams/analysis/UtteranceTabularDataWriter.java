/*
 *  This file is part of Tangrams-restricted.
 *
 *  Tangrams-restricted is free software: you can redistribute it and/or modify
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
package se.kth.speech.coin.tangrams.analysis;

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
import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.Future;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;
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

import se.kth.speech.CommonPaths;
import se.kth.speech.MapCollectors;
import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.coin.tangrams.analysis.io.SessionDataManager;
import se.kth.speech.coin.tangrams.game.PlayerRole;
import se.kth.speech.coin.tangrams.iristk.io.LoggedEventReader;
import se.kth.speech.higgins._2005.annotation.Annotation;
import se.kth.speech.higgins._2005.annotation.Annotation.Segments.Segment;
import se.kth.speech.higgins.io.HatIO;

/**
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since 24 Nov 2017
 *
 */
public final class UtteranceTabularDataWriter { // NO_UCD (unused code)

	private enum Parameter implements Supplier<Option> {
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
		};

		protected final String optName;

		private Parameter(final String optName) {
			this.optName = optName;
		}
	}

	private static class TabularDataFactory {

		private static final List<String> COL_NAMES = Arrays.asList("ROUND", "SPEAKER", "DIALOGUE_ROLE", "START_TIME",
				"END_TIME", "TOKENS");

		private static final Collector<CharSequence, ?, String> ROW_CELL_JOINER = Collectors.joining("\t");

		private static final Collector<CharSequence, ?, String> TOKEN_JOINER = Collectors.joining(" ");

		private static String formatMinutes(final double timeInSecs) {
			final double fractionalMins = timeInSecs / 60;
			final double wholeMins = Math.floor(fractionalMins);
			final double remainingSecs = timeInSecs - wholeMins * 60;
			return String.format("%02.0f:%02.3f", wholeMins, remainingSecs);

		}

		private final Map<? super String, Utterance> origUttsBySegmentId;

		private final Map<? super String, PlayerRole> playerInitialRoles;

		private final Map<? super String, String> playerParticipantIds;

		private final Predicate<? super Utterance> uttRowFilter;

		private TabularDataFactory(final Map<? super String, String> playerParticipantIds,
				final Map<? super String, PlayerRole> playerInitialRoles,
				final Map<? super String, Utterance> origUttsBySegmentId,
				final Predicate<? super Utterance> uttRowFilter) {
			this.playerParticipantIds = playerParticipantIds;
			this.playerInitialRoles = playerInitialRoles;
			this.origUttsBySegmentId = origUttsBySegmentId;
			this.uttRowFilter = uttRowFilter;
		}

		private void addUtt(final Utterance origUtt, final Collection<? super String> uttRows, final int roundId,
				final EventDialogue evtDiag,
				final BiFunction<? super Utterance, ? super EventDialogue, DialogueRole> uttDiagRoleFactory) {
			final String segId = origUtt.getSegmentId();
			final List<Object> rowCells = new ArrayList<>(COL_NAMES.size());
			rowCells.add(roundId);

			final String speakerId = origUtt.getSpeakerId();
			final String participantId = playerParticipantIds.get(speakerId);
			rowCells.add(participantId);
			final DialogueRole diagRole = uttDiagRoleFactory.apply(origUtt, evtDiag);
			rowCells.add(diagRole);

			final float startTime = origUtt.getStartTime();
			rowCells.add(startTime);
			final float endTime = origUtt.getEndTime();
			rowCells.add(endTime);

			final Utterance rawUtt = origUttsBySegmentId.get(segId);
			final String rawUttRepr = rawUtt.getTokens().stream().collect(TOKEN_JOINER);
			rowCells.add(rawUttRepr);

			LOGGER.debug("Raw utt: \"{}\"; Orig utt: \"{}\"; Transformed utt: \"{}\"", rawUttRepr,
					origUtt.getTokens().stream().collect(TOKEN_JOINER));
			uttRows.add(rowCells.stream().map(Object::toString).collect(ROW_CELL_JOINER));
		}

		private void addUtteranceDataRows(final int roundId, final EventDialogue evtDiag,
				final Collection<? super String> uttRows,
				final BiFunction<? super Utterance, ? super EventDialogue, DialogueRole> uttDiagRoleFactory) {
			final List<Utterance> origUtts = evtDiag.getUtterances();
			LOGGER.debug("Writing rows for round ID {}.", roundId);

			final Stream<Utterance> filteredOrigUtts = origUtts.stream().filter(uttRowFilter);
			final Iterator<Utterance> filteredOrigUttIter = filteredOrigUtts.iterator();
			if (filteredOrigUttIter.hasNext()) {
				final Utterance firstOrigUtt = filteredOrigUttIter.next();
				final DialogueRole firstOrigUttDiagRole = uttDiagRoleFactory.apply(firstOrigUtt, evtDiag);
				if (!DialogueRole.INSTRUCTOR.equals(firstOrigUttDiagRole)) {
					LOGGER.warn("First utterance of round {} is not from the instructor: Start time: {}; \"{}\"",
							roundId, formatMinutes(firstOrigUtt.getStartTime()),
							firstOrigUtt.getTokens().stream().collect(TOKEN_JOINER));
				}
				addUtt(firstOrigUtt, uttRows, roundId, evtDiag, uttDiagRoleFactory);
				while (filteredOrigUttIter.hasNext()) {
					final Utterance nextOrigUtt = filteredOrigUttIter.next();
					addUtt(nextOrigUtt, uttRows, roundId, evtDiag, uttDiagRoleFactory);
				}
			}
		}

		private List<String> apply(final Iterator<EventDialogue> evtDiagsToPrint, final int expectedUtteranceCount) {
			final List<String> result = new ArrayList<>(expectedUtteranceCount + 1);
			result.add(COL_NAMES.stream().collect(ROW_CELL_JOINER));

			final EventDialogue firstDiag = evtDiagsToPrint.next();
			int roundId;
			final BiFunction<? super Utterance, ? super EventDialogue, DialogueRole> firstUttDiagRoleFactory;
			if (firstDiag.getFirstEvent().isPresent()) {
				// The first event dialogue represents a round in the game
				// session
				roundId = 1;
				firstUttDiagRoleFactory = DialogueRole::get;
			} else {
				// Use 0 index for pre-game dialogue, i.e. an EventDialogue with
				// no actual game event(s)
				roundId = 0;
				firstUttDiagRoleFactory = this::getInitialPlayerDiagRole;
			}
			addUtteranceDataRows(roundId, firstDiag, result, firstUttDiagRoleFactory);
			while (evtDiagsToPrint.hasNext()) {
				final EventDialogue evtDiag = evtDiagsToPrint.next();
				addUtteranceDataRows(++roundId, evtDiag, result, DialogueRole::get);
			}
			return result;
		}

		private List<String> apply(final List<EventDialogue> evtDiagsToPrint) {
			return apply(evtDiagsToPrint.iterator(), evtDiagsToPrint.size() * 4 + 1);
		}

		private DialogueRole getInitialPlayerDiagRole(final String playerId) {
			return DialogueRole.get(playerInitialRoles.get(playerId));
		}

		private DialogueRole getInitialPlayerDiagRole(final Utterance utt, final EventDialogue evtDiag) {
			return getInitialPlayerDiagRole(utt.getSpeakerId());
		}

	}

	private static final String DEFAULT_OUTFILE_NAME = "utts.tsv";

	/**
	 * Print all utts by default
	 */
	private static final Predicate<Utterance> DEFAULT_UTT_FILTER = transformedUtt -> true;

	private static final Logger LOGGER = LoggerFactory.getLogger(UtteranceTabularDataWriter.class);

	private static final Options OPTIONS = createOptions();

	private static final Charset OUTPUT_ENCODING = StandardCharsets.UTF_8;

	private static final UtteranceSpeakerParticipantIdMapFactory PLAYER_PARTICIPANT_ID_MAPPER = new UtteranceSpeakerParticipantIdMapFactory();

	/**
	 * A factory for creating unfiltered {@link Utterance} instances,
	 * i.e.&nbsp;still containing possible metalanguage tokens.
	 */
	private static final SegmentUtteranceFactory SEG_UTT_FACTORY = new SegmentUtteranceFactory();

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
			final UtteranceTabularDataWriter writer = new UtteranceTabularDataWriter(outDir, outfileName);
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
		formatter.printHelp(UtteranceTabularDataWriter.class.getName() + " INPATHS...", OPTIONS);
	}

	private static Map<SessionDataManager, Path> readTestSessionData(final Iterable<Path> inpaths) throws IOException {
		final Map<Path, SessionDataManager> infileSessionData = SessionDataManager.createFileSessionDataMap(inpaths);
		final Map<SessionDataManager, Path> result = infileSessionData.entrySet().stream()
				.collect(Collectors.toMap(Entry::getValue, Entry::getKey, MapCollectors.throwingMerger(),
						() -> Maps.newHashMapWithExpectedSize(infileSessionData.size())));
		infileSessionData.forEach((infile, sessionData) -> result.put(sessionData, infile));
		return result;
	}

	private static Stream<Utterance> readUtterances(final SessionDataManager sessionDataMgr)
			throws JAXBException, IOException {
		final Path hatInfilePath = sessionDataMgr.getHATFilePath();
		final Annotation uttAnnots = HatIO.readAnnotation(hatInfilePath);
		final List<Segment> segs = uttAnnots.getSegments().getSegment();
		return segs.stream().flatMap(seg -> SEG_UTT_FACTORY.create(seg).stream());
	}

	private final Path outDir;

	private final String outfileName;

	private final Predicate<? super Utterance> uttRowFilter;

	public UtteranceTabularDataWriter(final Path outDir, final String outfileName) {
		this(outDir, outfileName, DEFAULT_UTT_FILTER);
	}

	public UtteranceTabularDataWriter(final Path outDir, final String outfileName,
			final Predicate<? super Utterance> uttRowFilter) {
		this.outDir = outDir;
		this.outfileName = outfileName;
		this.uttRowFilter = uttRowFilter;
	}

	public void accept(final Future<Map<SessionDataManager, Path>> sessionDataFuture)
			throws InterruptedException, ExecutionException, IOException, JAXBException {
		final Map<SessionDataManager, Path> allSessionData = sessionDataFuture.get();
		final Path sessionPrefixPath = CommonPaths.findCommonPrefixPath(allSessionData.values().stream());
		LOGGER.info("Found a common path of \"{}\" for all input sessions.", sessionPrefixPath);

		final SessionGameManager.Factory sessionGameMgrFactory = new SessionGameManager.Factory(
				new LoggedEventReader(allSessionData.size(), allSessionData.size() * 10));
		for (final Iterator<Entry<SessionDataManager, Path>> sessionDataPathIter = allSessionData.entrySet().stream()
				.sorted(Comparator.comparing(Entry::getValue)).iterator(); sessionDataPathIter.hasNext();) {
			final Entry<SessionDataManager, Path> sessionDataPath = sessionDataPathIter.next();
			final SessionDataManager sessionDataMgr = sessionDataPath.getKey();
			final Path sessionPropsFilePath = sessionDataPath.getValue().toAbsolutePath();
			final Path sessionDir = sessionPropsFilePath.getParent();
			assert sessionDir != null;
			final Path relativeSessionDir = sessionPrefixPath.relativize(sessionDir);
			final Path sessionOutputDir = Files.createDirectories(outDir.resolve(relativeSessionDir));

			final Stream<Utterance> utts = readUtterances(sessionDataMgr);
			final Map<String, Utterance> uttsBySegmentId = utts
					.collect(Collectors.toMap(Utterance::getSegmentId, Function.identity()));

			final SessionGameManager sessionGameMgr = sessionGameMgrFactory.apply(sessionDataMgr);
			final SessionGame sessionGame = sessionGameMgr.getCanonicalGame();
			final BiMap<PlayerRole, String> playerRoles = sessionGame.getHistory().getInitialState().getPlayerRoles();
			final BiMap<String, String> playerParticipantIds = PLAYER_PARTICIPANT_ID_MAPPER.apply(playerRoles);
			final List<EventDialogue> evtDiags = sessionGame.getEventDialogues();
			final List<String> rows = new TabularDataFactory(playerParticipantIds, playerRoles.inverse(),
					uttsBySegmentId, uttRowFilter).apply(evtDiags);
			final Path outfile = sessionOutputDir.resolve(outfileName);
			LOGGER.info("Writing data extracted from \"{}\" to \"{}\".", sessionPropsFilePath, outfile);
			Files.write(outfile, rows, OUTPUT_ENCODING, StandardOpenOption.CREATE,
					StandardOpenOption.TRUNCATE_EXISTING);
		}
		LOGGER.info("Finished writing data for {} session(s).", allSessionData.size());
	}
}
