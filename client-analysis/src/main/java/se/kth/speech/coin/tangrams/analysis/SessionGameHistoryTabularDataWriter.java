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

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.charset.Charset;
import java.nio.file.FileVisitOption;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.time.Duration;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.EnumMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NavigableMap;
import java.util.NavigableSet;
import java.util.Objects;
import java.util.Optional;
import java.util.OptionalInt;
import java.util.Properties;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.function.BiFunction;
import java.util.function.Supplier;
import java.util.regex.Pattern;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import javax.xml.bind.JAXBException;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.MissingOptionException;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.LogCommand;
import org.eclipse.jgit.api.errors.GitAPIException;
import org.eclipse.jgit.api.errors.NoHeadException;
import org.eclipse.jgit.errors.AmbiguousObjectException;
import org.eclipse.jgit.errors.IncorrectObjectTypeException;
import org.eclipse.jgit.errors.RevisionSyntaxException;
import org.eclipse.jgit.lib.AbbreviatedObjectId;
import org.eclipse.jgit.lib.Constants;
import org.eclipse.jgit.lib.ObjectId;
import org.eclipse.jgit.lib.ObjectLoader;
import org.eclipse.jgit.lib.ObjectReader;
import org.eclipse.jgit.lib.PersonIdent;
import org.eclipse.jgit.lib.Repository;
import org.eclipse.jgit.revwalk.RevCommit;
import org.eclipse.jgit.revwalk.filter.CommitTimeRevFilter;
import org.eclipse.jgit.revwalk.filter.RevFilter;
import org.eclipse.jgit.storage.file.FileRepositoryBuilder;
import org.eclipse.jgit.treewalk.TreeWalk;
import org.eclipse.jgit.treewalk.filter.PathFilter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.google.common.collect.BiMap;
import com.google.common.collect.Table;

import it.unimi.dsi.fastutil.ints.IntOpenHashSet;
import it.unimi.dsi.fastutil.ints.IntSet;
import se.kth.speech.ObservationOrderComparator;
import se.kth.speech.TimestampArithmetic;
import se.kth.speech.coin.tangrams.TangramsClient;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeature;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeatureExtractionContextFactory;
import se.kth.speech.coin.tangrams.analysis.io.SessionDataManager;
import se.kth.speech.coin.tangrams.game.Selection;
import se.kth.speech.coin.tangrams.iristk.EventTypeMatcher;
import se.kth.speech.coin.tangrams.iristk.GameEvent;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;
import se.kth.speech.coin.tangrams.iristk.io.LoggedEventReader;
import se.kth.speech.coin.tangrams.view.InteractiveGameBoardPanel;

/**
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since 23 Aug 2017
 *
 */
public final class SessionGameHistoryTabularDataWriter { // NO_UCD (unused code)

	public enum EventDatum implements BiFunction<EventContext, String, String> {
		ENTITY {

			@Override
			public String apply(final EventContext eventCtx, final String nullValueRepr) {
				// 1-indexed
				return Integer.toString(eventCtx.getEntityId() + 1);
			}

		},
		EVENT {

			@Override
			public String apply(final EventContext eventCtx, final String nullValueRepr) {
				return Integer.toString(eventCtx.getEventId());
			}

		},
		NAME {

			private final Pattern tangramsActionEventNamePrefixPattern = Pattern.compile("tangrams\\.action\\.");

			@Override
			public String apply(final EventContext eventCtx, final String nullValueRepr) {
				final GameEvent event = eventCtx.getEvent();
				final String eventName = event.getName();
				return tangramsActionEventNamePrefixPattern.matcher(eventName).replaceFirst("");
			}
		},
		REFERENT {

			@Override
			public String apply(final EventContext eventCtx, final String nullValueRepr) {
				final GameContext gameCtx = eventCtx.getGameContext();
				final OptionalInt optReferentEntityId = gameCtx.findLastSelectedEntityId();
				final Boolean isReferent;
				if (optReferentEntityId.isPresent()) {
					final int referentEntityId = optReferentEntityId.getAsInt();
					final int entityId = eventCtx.getEntityId();
					isReferent = Objects.equals(referentEntityId, entityId);
				} else {
					isReferent = Boolean.FALSE;
				}
				return isReferent.toString();
			}

		},
		ROUND {

			@Override
			public String apply(final EventContext eventCtx, final String nullValueRepr) {
				return Integer.toString(eventCtx.getGameRoundId());
			}

		},
		SCORE {

			@Override
			public String apply(final EventContext eventCtx, final String nullValueRepr) {
				return Integer.toString(eventCtx.getScore());
			}

		},
		SELECTED {

			private final EventTypeMatcher selectionEventMatcher = new EventTypeMatcher(
					GameManagementEvent.SELECTION_REQUEST, GameManagementEvent.SELECTION_REJECTION);

			@Override
			public String apply(final EventContext eventCtx, final String nullValueRepr) {
				final GameEvent event = eventCtx.getEvent();

				final boolean isSelected;
				if (selectionEventMatcher.test(event)) {
					final Selection selection = (Selection) event.getGameAttrs()
							.get(GameManagementEvent.Attribute.SELECTION);
					final int selectedEntityId = selection.getPieceId();
					final int entityId = eventCtx.getEntityId();
					isSelected = selectedEntityId == entityId;
				} else {
					isSelected = false;
				}
				return Boolean.toString(isSelected);
			}

		},
		SUBMITTER {
			@Override
			public String apply(final EventContext eventCtx, final String nullValueRepr) {
				final GameEvent event = eventCtx.getEvent();
				final Object attrVal = event.getGameAttrs().get(GameManagementEvent.Attribute.PLAYER_ID);
				return Objects.toString(attrVal, nullValueRepr);
			}
		},
		TIME {
			@Override
			public String apply(final EventContext eventCtx, final String nullValueRepr) {
				return eventCtx.getOffsetSecs().toPlainString();
			}
		};

		private static final List<EventDatum> CANONICAL_ORDERING;

		static {
			CANONICAL_ORDERING = Collections.unmodifiableList(Arrays.asList(EventDatum.EVENT, EventDatum.ROUND,
					EventDatum.SCORE, EventDatum.TIME, EventDatum.NAME, EventDatum.SUBMITTER, EventDatum.ENTITY,
					EventDatum.REFERENT, EventDatum.SELECTED));
			assert CANONICAL_ORDERING.size() == EventDatum.values().length;
		}

	}

	private enum SessionMetadatum {
		END_SCORE, ENTITY_COUNT, EVENT_COUNT, EXPERIMENT_VERSION, GAME_DURATION, GAME_ID, INITIAL_INSTRUCTOR_ID, MOVE_DELAY, ROUND_COUNT, START_TIME;

	}

	private static class SessionMetadatumNameComparator implements Comparator<String> {

		private static SessionMetadatum parseNullableMetadatum(final String name) {
			SessionMetadatum result = null;
			try {
				result = SessionMetadatum.valueOf(name);
			} catch (final IllegalArgumentException e) {
				LOGGER.debug(String.format("Unable to parse \"%s\" as an instance of %s; Returning null.", name,
						SessionMetadatum.class), e);
			}
			return result;
		}

		private final Comparator<String> rowObservationOrderComparator;

		private SessionMetadatumNameComparator(final int expectedRowCount) {
			rowObservationOrderComparator = new ObservationOrderComparator<>(expectedRowCount);
		}

		@Override
		public int compare(final String o1, final String o2) {
			int result;

			final SessionMetadatum m1 = parseNullableMetadatum(o1);
			final SessionMetadatum m2 = parseNullableMetadatum(o2);
			if (m1 == null) {
				if (m2 == null) {
					result = rowObservationOrderComparator.compare(o1, o2);
				} else {
					result = 1;
				}
			} else if (m2 == null) {
				result = -1;
			} else {
				result = m1.compareTo(m2);
			}

			return result;
		}

	}

	private enum Parameter implements Supplier<Option> {
		GIT_REPO_PATH("g") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("git-repository").hasArg().argName("path").type(File.class)
						.desc("Specifies the git repository to use for automatically determining metadata.").build();
			}
		},
		HELP("?") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("help").desc("Prints this message.").build();
			}
		};

		private static final Options OPTIONS = createOptions();

		private static Options createOptions() {
			final Options result = new Options();
			Arrays.stream(Parameter.values()).map(Parameter::get).forEach(result::addOption);
			return result;
		}

		private static File parseGitRepoPath(final CommandLine cl) throws ParseException, URISyntaxException {
			final File result;

			final File suppliedGitRepo = (File) cl.getParsedOptionValue(Parameter.GIT_REPO_PATH.optName);
			if (suppliedGitRepo == null) {
				final URL clientSourceLoc = TangramsClient.class.getProtectionDomain().getCodeSource().getLocation();
				result = new File(clientSourceLoc.toURI());
			} else {
				result = suppliedGitRepo;
			}

			return result;
		}

		private static void printHelp() {
			final HelpFormatter formatter = new HelpFormatter();
			formatter.printHelp(SessionGameHistoryTabularDataWriter.class.getName() + " INPATHS...", OPTIONS);
		}

		protected final String optName;

		private Parameter(final String optName) {
			this.optName = optName;
		}

	}
	
	public static final Charset OUTPUT_CHARSET = LoggedEventReader.CHARSET;

	private static final int ESTIMATED_EVENT_METADATUM_COUNT = SessionMetadatum.values().length + 8;

	private static final long EXPECTED_MAXIMUM_UNIQUE_EXPERIMENT_VERSIONS = 24;

	private static final ZoneId EXPERIMENT_VERSION_TIMEZONE = ZoneId.of("UTC");

	private static final GameManagementEvent GAME_ROUND_DELIMITING_EVENT_TYPE = GameManagementEvent.NEXT_TURN_REQUEST;

	private static final Logger LOGGER = LoggerFactory.getLogger(SessionGameHistoryTabularDataWriter.class);

	private static final String NULL_VALUE_REPR = "?";

	private static final ZoneId ORIGINAL_EXPERIMENT_TIMEZONE = ZoneId.of("Europe/Stockholm");

	private static final DateTimeFormatter OUTPUT_DATETIME_FORMATTER = DateTimeFormatter.ISO_OFFSET_DATE_TIME;

	private static final Comparator<RevCommit> REV_TIME_COMPARATOR = Comparator
			.comparing(commit -> commit.getAuthorIdent().getWhen());

	/**
	 * <strong>NOTE:</strong> This string is fragile and must be automatically
	 * updated in cases of e.g.&nbsp;refactoring.
	 */
	private static final String SOURCE_FILE_LOCATOR_PATH = "client/src/main/resources/"
			+ InteractiveGameBoardPanel.class.getPackage().getName().replace('.', '/') + '/'
			+ InteractiveGameBoardPanel.class.getSimpleName() + ".properties";

	private static final Collector<CharSequence, ?, String> TABLE_ROW_CELL_JOINER;

	private static final String TABLE_STR_REPR_COL_DELIM;

	static {
		TABLE_STR_REPR_COL_DELIM = "\t";
		TABLE_ROW_CELL_JOINER = Collectors.joining(TABLE_STR_REPR_COL_DELIM);
	}

	public static void main(final String[] args) throws URISyntaxException, IOException, JAXBException {
		final CommandLineParser parser = new DefaultParser();
		try {
			final CommandLine cl = parser.parse(Parameter.OPTIONS, args);
			main(cl);
		} catch (final ParseException e) {
			System.out.println(String.format("An error occured while parsing the command-line arguments: %s", e));
			Parameter.printHelp();
		}
	}

	private static String createCommitDesc(final RevCommit commit, final Git gitCommandFactory) throws IOException {
		final PersonIdent authIdent = commit.getAuthorIdent();
		final Date date = authIdent.getWhen();
		// final TimeZone timeZone = authIdent.getTimeZone();
		final ZonedDateTime zonedDateTime = ZonedDateTime.ofInstant(date.toInstant(), EXPERIMENT_VERSION_TIMEZONE);
		final ObjectId commitId = commit.getId();
		final ObjectReader objReader = gitCommandFactory.getRepository().newObjectReader();
		final AbbreviatedObjectId abbvCommitId = objReader.abbreviate(commitId);
		return String.format("%s, git commit %s", OUTPUT_DATETIME_FORMATTER.format(zonedDateTime), abbvCommitId.name());
	}

	private static Git createGitCommandFactory(final File inpath) throws IOException {
		final FileRepositoryBuilder builder = new FileRepositoryBuilder();
		builder.findGitDir(inpath);
		final File gitDir = builder.getGitDir();
		LOGGER.info("Using \"{}\" as git repository.", gitDir);
		final Repository repository = builder.build();
		// Get the central git object from the repository
		return new Git(repository);
	}

	private static LogCommand createLatestCommitLogCommand(final ZonedDateTime zonedGameStart,
			final Git gitCommandFactory)
			throws RevisionSyntaxException, AmbiguousObjectException, IncorrectObjectTypeException, IOException {
		final LogCommand result = gitCommandFactory.log();
		final ObjectId head = gitCommandFactory.getRepository().resolve(Constants.HEAD);
		result.add(head);
		// https://stackoverflow.com/a/45588376/1391325
		// https://stackoverflow.com/a/23885950/1391325
		final RevFilter beforeExpTime = CommitTimeRevFilter.before(Date.from(zonedGameStart.toInstant()));
		result.setRevFilter(beforeExpTime);
		result.setMaxCount(1);
		return result;
	}

	private static Optional<RevCommit> findLatestCommit(final ZonedDateTime zonedGameStart, final Git gitCommandFactory)
			throws NoHeadException, GitAPIException, RevisionSyntaxException, AmbiguousObjectException,
			IncorrectObjectTypeException, IOException {
		final LogCommand logCommand = createLatestCommitLogCommand(zonedGameStart, gitCommandFactory);
		return StreamSupport.stream(logCommand.call().spliterator(), false).max(REV_TIME_COMPARATOR);
	}

	private static OptionalInt findMoveSubmissionWaitTime(final RevCommit commit, final Git gitCommandFactory)
			throws IncorrectObjectTypeException, IOException {
		final IntSet waitTimesMills = new IntOpenHashSet(1);

		try (TreeWalk walk = new TreeWalk(gitCommandFactory.getRepository())) {
			walk.addTree(commit.getTree());
			walk.setFilter(PathFilter.create(SOURCE_FILE_LOCATOR_PATH));
			walk.setRecursive(true);
			final ObjectReader objReader = walk.getObjectReader();

			while (walk.next()) {
				final ObjectId objId = walk.getObjectId(0);
				final ObjectLoader objLoader = objReader.open(objId);
				final Properties props = new Properties();
				try (InputStream inStream = objLoader.openStream()) {
					props.load(inStream);
				}
				final int waitTimeMills = Integer.parseInt(
						props.getProperty(InteractiveGameBoardPanel.Property.MOVE_SUBMISSION_WAIT_TIME.getPropName()));
				waitTimesMills.add(waitTimeMills);
			}
		}

		final OptionalInt result;
		switch (waitTimesMills.size()) {
		case 0: {
			result = OptionalInt.empty();
			break;
		}
		case 1: {
			result = OptionalInt.of(waitTimesMills.iterator().nextInt());
			break;
		}
		default: {
			throw new AssertionError(String
					.format("Found more than one move submission wait time while walking commit: %s", waitTimesMills));
		}
		}
		return result;
	}

	public static void main(final CommandLine cl)
			throws ParseException, URISyntaxException, IOException, JAXBException {
		if (cl.hasOption(Parameter.HELP.optName)) {
			Parameter.printHelp();
		} else {
			final Path[] inpaths = cl.getArgList().stream()
					.map(Paths::get).toArray(Path[]::new);
			if (inpaths.length < 1) {
				throw new MissingOptionException("No input file(s) specified.");

			} else {
				final File gitRepoPath = Parameter.parseGitRepoPath(cl);
				LOGGER.info("Finding git repository for path \"{}\".", gitRepoPath);
				final Git gitCommandFactory = createGitCommandFactory(gitRepoPath);
				final SessionGameHistoryTabularDataWriter writer = new SessionGameHistoryTabularDataWriter(
						EventDatum.CANONICAL_ORDERING, NULL_VALUE_REPR, gitCommandFactory);

				final NavigableSet<Path> infiles = new TreeSet<>();
				for (final Path inpath : inpaths) {
					LOGGER.info("Looking for session data underneath \"{}\".", inpath);
					Files.walk(inpath, FileVisitOption.FOLLOW_LINKS).filter(Files::isRegularFile)
							.filter(filePath -> filePath.getFileName().toString().endsWith(".properties"))
							.forEach(infiles::add);
				}

				final SessionGameManager.Factory sessionGameMgrFactory = new SessionGameManager.Factory(
						new LoggedEventReader(infiles.size(), infiles.size() * 20), 2000);
				for (final Path infile : infiles) {
					writer.accept(infile, sessionGameMgrFactory);
				}
			}
		}
	}

	private static void persistSessionMetadata(final Map<SessionMetadatum, String> metadataValues,
			final Comparator<? super String> metadatumNameComparator, final Path outfilePath) throws IOException {
		// NOTE: This is not atomic: The OS could write to the file between its
		// reading and rewriting
		final NavigableMap<String, String> unifiedMetadata = readMetadata(outfilePath, metadatumNameComparator);
		metadataValues.forEach((metadatum, value) -> {
			unifiedMetadata.put(metadatum.toString(), value);
		});

		{
			final Stream<Stream<String>> metadataRows = unifiedMetadata.entrySet().stream()
					.map(entry -> new String[] { entry.getKey(), entry.getValue() }).map(Arrays::stream);
			final Stream<String> metadataFileRows = metadataRows.map(stream -> stream.collect(TABLE_ROW_CELL_JOINER));
			Files.write(outfilePath, (Iterable<String>) metadataFileRows::iterator, OUTPUT_CHARSET,
					StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING);
		}
	}

	private static NavigableMap<String, String> readMetadata(final Path infilePath,
			final Comparator<? super String> metadatumNameComparator) throws IOException {
		final NavigableMap<String, String> result = new TreeMap<>(metadatumNameComparator);
		try (BufferedReader metadataRowReader = Files.newBufferedReader(infilePath, OUTPUT_CHARSET)) {
			for (String row = metadataRowReader.readLine(); row != null; row = metadataRowReader.readLine()) {
				final String[] rowCells = row.split(TABLE_STR_REPR_COL_DELIM);
				if (rowCells.length != 2) {
					throw new IllegalArgumentException(
							String.format("Row is not a valid metadatum-value pair: %s", Arrays.toString(rowCells)));
				} else {
					final String metadatumCell = rowCells[0];
					final String value = rowCells[1];
					final String extantValue = result.put(metadatumCell, value);
					if (extantValue != null) {
						throw new IllegalArgumentException(
								String.format("More than one row found for metadatum \"%s\".", metadatumCell));
					}
				}

			}
		} catch (final NoSuchFileException e) {
			LOGGER.debug("No already-persisted metadata found at \"{}\".", infilePath);
		}
		return result;
	}

	private static int updateScore(final GameManagementEvent nextEventType, final int currentScore) {
		final int result;

		switch (nextEventType) {
		case COMPLETED_TURN_REQUEST: {
			result = currentScore + 1;
			break;
		}
		case SELECTION_REJECTION: {
			result = currentScore - 2;
			break;
		}
		default: {
			result = currentScore;
			break;
		}
		}

		return result;
	}

	private final LoadingCache<RevCommit, Map<SessionMetadatum, String>> commitMetadata;

	private final LoadingCache<ZonedDateTime, RevCommit> dateLatestCommits;

	private final LoadingCache<EventContext, String[]> eventDataRowCellValues;

	private final List<EventDatum> eventDataToDescribe;

	private SessionGameHistoryTabularDataWriter(final List<EventDatum> eventDataToDescribe,
			final String nullCellValueRepr, final Git gitCommandFactory) {
		this.eventDataToDescribe = eventDataToDescribe;
		final int concurrencyLevel = 1;
		eventDataRowCellValues = CacheBuilder.newBuilder().concurrencyLevel(concurrencyLevel).initialCapacity(96)
				.maximumSize(144).build(new CacheLoader<EventContext, String[]>() {

					@Override
					public String[] load(final EventContext eventCtx) {
						return eventDataToDescribe.stream().map(datum -> datum.apply(eventCtx, nullCellValueRepr))
								.toArray(String[]::new);
					}
				});

		dateLatestCommits = CacheBuilder.newBuilder().concurrencyLevel(concurrencyLevel)
				.maximumSize(EXPECTED_MAXIMUM_UNIQUE_EXPERIMENT_VERSIONS)
				.build(new CacheLoader<ZonedDateTime, RevCommit>() {

					@Override
					public RevCommit load(final ZonedDateTime zonedGameStart)
							throws RevisionSyntaxException, NoHeadException, AmbiguousObjectException,
							IncorrectObjectTypeException, GitAPIException, IOException {
						return findLatestCommit(zonedGameStart, gitCommandFactory).get();
					}

				});
		commitMetadata = CacheBuilder.newBuilder().concurrencyLevel(concurrencyLevel)
				.maximumSize(EXPECTED_MAXIMUM_UNIQUE_EXPERIMENT_VERSIONS)
				.build(new CacheLoader<RevCommit, Map<SessionMetadatum, String>>() {

					@Override
					public Map<SessionMetadatum, String> load(final RevCommit commit) throws IOException {
						final Map<SessionMetadatum, String> result = new EnumMap<>(SessionMetadatum.class);
						final String commitDesc = createCommitDesc(commit, gitCommandFactory);
						result.put(SessionMetadatum.EXPERIMENT_VERSION, commitDesc);

						final int moveWaitTime = findMoveSubmissionWaitTime(commit, gitCommandFactory).orElse(0);
						LOGGER.debug("Found a wait time of {}ms for experiment version \"{}\".", moveWaitTime,
								commitDesc);
						result.put(SessionMetadatum.MOVE_DELAY, Integer.toString(moveWaitTime));
						return result;
					}

				});
	}

	private void accept(final Path infile, final SessionGameManager.Factory sessionGameMgrFactory)
			throws IOException, JAXBException {
		final SessionDataManager infileSessionData = SessionDataManager.create(infile);
		final SessionGameManager sessionDiagMgr = sessionGameMgrFactory.apply(infileSessionData);
		final SessionGame canonicalGame = sessionDiagMgr.getCanonicalGame();

		final Path infileParentDir = infile.getParent();

		final GameHistory history = canonicalGame.getHistory();
		final EntityFeatureExtractionContextFactory extractionContextFactory = new EntityFeatureExtractionContextFactory();
		final EntityFeatureVectorDescriptionFactory entityFeatureVectorDescFactory = new EntityFeatureVectorDescriptionFactory(
				new EntityFeature.Extractor(), EntityFeature.getCanonicalOrdering(), extractionContextFactory,
				NULL_VALUE_REPR);
		final int entityCount = history.getEntityCount();

		int eventId = 0;
		int gameRoundId = 0;
		int gameScore = 0;
		LocalDateTime maxEventTime = LocalDateTime.MIN;
		{
			final List<GameEvent> events = Arrays.asList(history.getEventSequence().toArray(GameEvent[]::new));
			final List<Stream<String>> eventRows = new ArrayList<>(events.size() * entityCount);
			for (final ListIterator<GameEvent> eventIter = events.listIterator(); eventIter.hasNext();) {
				final GameEvent event = eventIter.next();
				// Event ID is 1-indexed
				eventId = eventIter.nextIndex();
				final GameManagementEvent eventType = GameManagementEvent.getEventType(event.getName());
				if (GAME_ROUND_DELIMITING_EVENT_TYPE.equals(eventType)) {
					gameRoundId++;
				}
				gameScore = updateScore(eventType, gameScore);
				final LocalDateTime eventTime = event.getTime();
				maxEventTime = Collections.max(Arrays.asList(eventTime, maxEventTime));
				final GameContext gameCtx = new GameContext(history, eventTime);
				// Create one row for each entity
				for (int entityId = 0; entityId < entityCount; ++entityId) {
					eventRows.add(createRowCellValues(entityFeatureVectorDescFactory,
							new EventContext(eventId, event, gameRoundId, gameCtx, entityId, gameScore)));
				}
			}
			final Stream<String> fileRows = Stream
					.concat(Stream.of(createColumnNames(entityFeatureVectorDescFactory)), eventRows.stream())
					.map(cells -> cells.collect(TABLE_ROW_CELL_JOINER));
			{
				final String outfileName = createEventOutfileName();
				final Path outfilePath = infileParentDir == null ? Paths.get(outfileName)
						: infileParentDir.resolve(outfileName);
				LOGGER.info("Writing tabular event data to \"{}\".", outfilePath);
				Files.write(outfilePath, (Iterable<String>) fileRows::iterator, OUTPUT_CHARSET,
						StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING);
			}
		}

		final BiMap<String, String> playerSourceIds = infileSessionData.getPlayerData().getPlayerSourceIds();
		final Entry<BiMap<String, String>, String> sourceParticipantIds = new SourceParticipantIdMapFactory()
				.apply(playerSourceIds, canonicalGame);
		{
			final Map<SessionMetadatum, String> metadataValues = createSessionMetadataReprMap(canonicalGame,
					sourceParticipantIds.getValue(), gameScore, entityCount, eventId, gameRoundId,
					history.getStartTime(), maxEventTime);
			{
				final String outfileName = createSessionMetadataOutfileName();
				final Path outfilePath = infileParentDir == null ? Paths.get(outfileName)
						: infileParentDir.resolve(outfileName);
				LOGGER.info("Writing session metadata to \"{}\".", outfilePath);
				persistSessionMetadata(metadataValues, new SessionMetadatumNameComparator(ESTIMATED_EVENT_METADATUM_COUNT),
						outfilePath);
			}
		}

		{

			final Table<ParticipantMetadatum, String, String> metadataValues = ParticipantMetadataTabularDataWriter
					.createParticipantMetadataReprTable(canonicalGame, playerSourceIds,
							sourceParticipantIds.getKey().inverse());
			{
				final String outfileName = createParticipantMetadataOutfileName();
				final Path outfilePath = infileParentDir == null ? Paths.get(outfileName)
						: infileParentDir.resolve(outfileName);
				LOGGER.info("Writing participant metadata to \"{}\".", outfilePath);
				final ParticipantMetadataTabularDataWriter participantMetadataWriter = new ParticipantMetadataTabularDataWriter(
						TABLE_STR_REPR_COL_DELIM, OUTPUT_CHARSET);
				participantMetadataWriter.persistParticipantMetadata(metadataValues, outfilePath);
			}
		}
	}

	private Stream<String> createColumnNames(
			final EntityFeatureVectorDescriptionFactory entityFeatureVectorDescFactory) {
		final Stream.Builder<String> resultBuilder = Stream.builder();
		final Stream<String> eventDatumNames = eventDataToDescribe.stream().map(EventDatum::toString);
		eventDatumNames.forEachOrdered(resultBuilder);
		final Stream<String> featureVectorColumnNames = entityFeatureVectorDescFactory.getEntityFeaturesToDescribe()
				.stream().map(EntityFeature::toString);
		featureVectorColumnNames.forEachOrdered(resultBuilder);
		return resultBuilder.build();
	}

	private String createSessionMetadataOutfileName() {
		return "session-metadata.tsv";
	}

	private Map<SessionMetadatum, String> createSessionMetadataReprMap(final SessionGame canonicalGame,
			final String initialInstructorId, final int gameScore, final int entityCount, final int eventCount,
			final int roundCount, final LocalDateTime startTime, final LocalDateTime maxEventTime) {
		assert roundCount <= eventCount;
		assert startTime.isBefore(maxEventTime);

		final Map<SessionMetadatum, String> result = new EnumMap<>(SessionMetadatum.class);

		result.put(SessionMetadatum.END_SCORE, Integer.toString(gameScore));
		result.put(SessionMetadatum.ENTITY_COUNT, Integer.toString(entityCount));
		result.put(SessionMetadatum.EVENT_COUNT, Integer.toString(eventCount));
		{
			final BigDecimal durationInSecs = TimestampArithmetic
					.toDecimalSeconds(Duration.between(startTime, maxEventTime));
			result.put(SessionMetadatum.GAME_DURATION, durationInSecs.toString());
		}
		result.put(SessionMetadatum.GAME_ID, canonicalGame.getGameId());
		result.put(SessionMetadatum.INITIAL_INSTRUCTOR_ID, initialInstructorId);
		result.put(SessionMetadatum.ROUND_COUNT, Integer.toString(roundCount));
		final ZonedDateTime zonedGameStart = startTime.atZone(ORIGINAL_EXPERIMENT_TIMEZONE);
		result.put(SessionMetadatum.START_TIME, OUTPUT_DATETIME_FORMATTER.format(zonedGameStart));

		final RevCommit lastCommit = dateLatestCommits.getUnchecked(zonedGameStart);
		final Map<SessionMetadatum, String> commitMetadatumMap = commitMetadata.getUnchecked(lastCommit);
		result.putAll(commitMetadatumMap);

		assert result.size() == SessionMetadatum.values().length;
		return result;
	}

	private String createEventOutfileName() {
		return "events.tsv";
	}

	private String createParticipantMetadataOutfileName() {
		return "participant-metadata.tsv";
	}

	private Stream<String> createRowCellValues(
			final EntityFeatureVectorDescriptionFactory entityFeatureVectorDescFactory, final EventContext eventCtx) {
		final Stream.Builder<String> resultBuilder = Stream.builder();
		Arrays.stream(eventDataRowCellValues.getUnchecked(eventCtx)).forEachOrdered(resultBuilder);

		final Stream<String> entityFeatureVectorReprs = entityFeatureVectorDescFactory
				.createFeatureValueReprs(eventCtx.getGameContext(), eventCtx.getEntityId());
		entityFeatureVectorReprs.forEachOrdered(resultBuilder);

		return resultBuilder.build();
	}
}
