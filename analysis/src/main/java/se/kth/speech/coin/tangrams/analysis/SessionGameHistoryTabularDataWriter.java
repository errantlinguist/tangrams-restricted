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
package se.kth.speech.coin.tangrams.analysis;

import java.io.BufferedReader;
import java.io.IOException;
import java.math.BigDecimal;
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
import java.util.EnumMap;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NavigableMap;
import java.util.Objects;
import java.util.Optional;
import java.util.TreeMap;
import java.util.function.BiFunction;
import java.util.regex.Pattern;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.xml.bind.JAXBException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.google.common.collect.ArrayTable;
import com.google.common.collect.BiMap;
import com.google.common.collect.Maps;
import com.google.common.collect.RowSortedTable;
import com.google.common.collect.Table;
import com.google.common.collect.TreeBasedTable;

import iristk.system.Event;
import se.kth.speech.TimestampArithmetic;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeature;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeatureExtractionContextFactory;
import se.kth.speech.coin.tangrams.analysis.features.ImageEdgeCounter;
import se.kth.speech.coin.tangrams.analysis.io.SessionDataManager;
import se.kth.speech.coin.tangrams.game.PlayerRole;
import se.kth.speech.coin.tangrams.iristk.EventTimes;
import se.kth.speech.coin.tangrams.iristk.EventTypeMatcher;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;
import se.kth.speech.coin.tangrams.iristk.events.Selection;
import se.kth.speech.coin.tangrams.iristk.io.LoggedEvents;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 23 Aug 2017
 *
 */
final class SessionGameHistoryTabularDataWriter { // NO_UCD (unused code)

	private enum EventDatum implements BiFunction<EventContext, String, String> {
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
				final Event event = eventCtx.getEvent();
				final String eventName = event.getName();
				return tangramsActionEventNamePrefixPattern.matcher(eventName).replaceFirst("");
			}
		},
		REFERENT {

			@Override
			public String apply(final EventContext eventCtx, final String nullValueRepr) {
				final GameContext gameCtx = eventCtx.getGameContext();
				final Optional<Integer> optReferentEntityId = gameCtx.findLastSelectedEntityId();
				final Boolean isReferent = optReferentEntityId.map(referentEntityId -> {
					final int entityId = eventCtx.getEntityId();
					return Objects.equals(referentEntityId, entityId);
				}).orElse(Boolean.FALSE);
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
				final Event event = eventCtx.getEvent();

				final boolean isSelected;
				if (selectionEventMatcher.test(event)) {
					final Selection selection = (Selection) event
							.get(GameManagementEvent.Attribute.SELECTION.toString());
					final Integer selectedEntityId = selection.getPieceId();
					final int entityId = eventCtx.getEntityId();
					isSelected = selectedEntityId.equals(entityId);
				} else {
					isSelected = false;
				}
				return Boolean.toString(isSelected);
			}

		},
		SUBMITTER {
			@Override
			public String apply(final EventContext eventCtx, final String nullValueRepr) {
				final Event event = eventCtx.getEvent();
				return event.getString(GameManagementEvent.Attribute.PLAYER_ID.toString(), nullValueRepr);
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

	private enum EventMetadatum {
		END_SCORE {
			@Override
			protected Integer parseValue(final String value) {
				return Integer.valueOf(value);
			}
		},
		ENTITY_COUNT {
			@Override
			protected Integer parseValue(final String value) {
				return Integer.valueOf(value);
			}
		},
		EVENT_COUNT {
			@Override
			protected Integer parseValue(final String value) {
				return Integer.valueOf(value);
			}
		},
		GAME_DURATION {
			@Override
			protected BigDecimal parseValue(final String value) {
				return new BigDecimal(value);
			}
		},
		GAME_ID {
			@Override
			protected String parseValue(final String value) {
				return value;
			}
		},
		INITIAL_INSTRUCTOR_ID {
			@Override
			protected String parseValue(final String value) {
				return value;
			}
		},
		ROUND_COUNT {
			@Override
			protected Integer parseValue(final String value) {
				return Integer.valueOf(value);
			}
		},
		START_TIME {
			@Override
			protected ZonedDateTime parseValue(final String value) {
				return OUTPUT_DATETIME_FORMATTER.parse(value, ZonedDateTime::from);
			}
		};

		protected abstract Object parseValue(String value);
	}

	private enum ParticipantMetadatum {
		INITIAL_ROLE, SOURCE_ID
	}

	private static class ParticipantMetadatumNameComparator implements Comparator<String> {

		private final Comparator<String> headerRowNameComparator;

		private ParticipantMetadatumNameComparator(final String headerRowName) {
			headerRowNameComparator = Comparator.comparing(name -> !headerRowName.equals(name));
		}

		@Override
		public int compare(final String o1, final String o2) {
			int result = headerRowNameComparator.compare(o1, o2);
			if (result == 0) {
				final ParticipantMetadatum m1 = parseNullableMetadatum(o1);
				final ParticipantMetadatum m2 = parseNullableMetadatum(o2);
				if (m1 == null) {
					if (m2 == null) {
						result = o1.compareTo(o2);
					} else {
						result = 1;
					}
				} else if (m2 == null) {
					result = -1;
				} else {
					result = m1.compareTo(m2);
				}

			}

			return result;
		}

		private ParticipantMetadatum parseNullableMetadatum(final String name) {
			ParticipantMetadatum result = null;
			try {
				result = ParticipantMetadatum.valueOf(name);
			} catch (final IllegalArgumentException e) {
				LOGGER.debug(String.format("Unable to parse \"%s\" as an instance of %s; Returning null.", name,
						ParticipantMetadatum.class), e);
			}
			return result;
		}

	}

	private static final int ESTIMATED_PARTICIPANT_METADATUM_COUNT = 16;

	private static final Comparator<String> EVENT_METADATUM_NAME_COMPARATOR = new Comparator<String>() {

		@Override
		public int compare(final String o1, final String o2) {
			int result;

			final EventMetadatum m1 = parseNullableMetadatum(o1);
			final EventMetadatum m2 = parseNullableMetadatum(o2);
			if (m1 == null) {
				if (m2 == null) {
					result = o1.compareTo(o2);
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

		private EventMetadatum parseNullableMetadatum(final String name) {
			EventMetadatum result = null;
			try {
				result = EventMetadatum.valueOf(name);
			} catch (final IllegalArgumentException e) {
				LOGGER.debug(String.format("Unable to parse \"%s\" as an instance of %s; Returning null.", name,
						EventMetadatum.class), e);
			}
			return result;
		}

	};

	private static final GameManagementEvent GAME_ROUND_DELIMITING_EVENT_TYPE = GameManagementEvent.NEXT_TURN_REQUEST;

	private static final Logger LOGGER = LoggerFactory.getLogger(SessionGameHistoryTabularDataWriter.class);

	private static final String NULL_VALUE_REPR;

	private static final ZoneId ORIGINAL_EXPERIMENT_TIMEZONE = ZoneId.of("Europe/Stockholm");

	private static final Charset OUTPUT_CHARSET = LoggedEvents.CHARSET;

	private static final DateTimeFormatter OUTPUT_DATETIME_FORMATTER = DateTimeFormatter.ISO_OFFSET_DATE_TIME;

	private static final Comparator<String> PARTICIPANT_ID_ORDERING_COMPARATOR = Comparator.naturalOrder();

	private static final String PARTICIPANT_METADATA_HEADER_ROW_NAME = "PARTICIPANT_ID";;

	private static final Collector<CharSequence, ?, String> TABLE_ROW_CELL_JOINER;

	private static final Pattern TABLE_STRING_REPR_COL_DELIMITER_PATTERN;

	static {
		final String tableStrReprColDelim = "\t";
		TABLE_STRING_REPR_COL_DELIMITER_PATTERN = Pattern.compile(tableStrReprColDelim);
		TABLE_ROW_CELL_JOINER = Collectors.joining(tableStrReprColDelim);

		NULL_VALUE_REPR = "?";
	}

	public static void main(final String[] args) throws IOException, JAXBException {
		final Path[] inpaths = Arrays.stream(args).map(String::trim).filter(path -> !path.isEmpty()).map(Paths::get)
				.toArray(Path[]::new);
		if (inpaths.length < 1) {
			throw new IllegalArgumentException(
					String.format("Usage: %s INPATHS...", SessionGameHistoryTabularDataWriter.class.getSimpleName()));
		} else {
			final SessionGameHistoryTabularDataWriter writer = new SessionGameHistoryTabularDataWriter(
					EventDatum.CANONICAL_ORDERING, NULL_VALUE_REPR);
			writer.accept(inpaths);
		}
	}

	private static Map<EventMetadatum, String> createEventMetadataReprMap(final SessionGame canonicalGame,
			final String initialInstructorId, final int gameScore, final int entityCount, final int eventCount,
			final int roundCount, final LocalDateTime startTime, final LocalDateTime maxEventTime) {
		assert roundCount <= eventCount;
		assert startTime.isBefore(maxEventTime);

		final Map<EventMetadatum, String> result = new EnumMap<>(EventMetadatum.class);

		result.put(EventMetadatum.END_SCORE, Integer.toString(gameScore));
		result.put(EventMetadatum.ENTITY_COUNT, Integer.toString(entityCount));
		result.put(EventMetadatum.EVENT_COUNT, Integer.toString(eventCount));
		{
			final BigDecimal durationInSecs = TimestampArithmetic
					.toDecimalSeconds(Duration.between(startTime, maxEventTime));
			result.put(EventMetadatum.GAME_DURATION, durationInSecs.toString());
		}
		result.put(EventMetadatum.GAME_ID, canonicalGame.getGameId());
		result.put(EventMetadatum.INITIAL_INSTRUCTOR_ID, initialInstructorId);
		result.put(EventMetadatum.ROUND_COUNT, Integer.toString(roundCount));
		{
			final ZonedDateTime zonedGameStart = startTime.atZone(ORIGINAL_EXPERIMENT_TIMEZONE);
			result.put(EventMetadatum.START_TIME, OUTPUT_DATETIME_FORMATTER.format(zonedGameStart));
		}
		assert result.size() == EventMetadatum.values().length;
		return result;
	}

	private static Table<ParticipantMetadatum, String, String> createParticipantMetadataReprTable(
			final SessionGame canonicalGame, final BiMap<String, String> playerSourceIds,
			final BiMap<String, String> participantSourceIds) {
		final List<String> sortedParticipantIds = new ArrayList<>(participantSourceIds.keySet());
		sortedParticipantIds.sort(PARTICIPANT_ID_ORDERING_COMPARATOR);
		final Table<ParticipantMetadatum, String, String> result = ArrayTable
				.create(Arrays.asList(ParticipantMetadatum.values()), sortedParticipantIds);

		{
			final BiMap<String, PlayerRole> playerRoles = canonicalGame.getHistory().getInitialState().getPlayerRoles()
					.inverse();
			final BiMap<String, String> sourceParticipantIds = participantSourceIds.inverse();
			playerRoles.forEach((playerId, role) -> {
				final String sourceId = playerSourceIds.get(playerId);
				final String participantId = sourceParticipantIds.get(sourceId);
				result.put(ParticipantMetadatum.INITIAL_ROLE, participantId, role.toString());
			});
		}

		for (final Entry<String, String> participantSourceId : participantSourceIds.entrySet()) {
			result.put(ParticipantMetadatum.SOURCE_ID, participantSourceId.getKey(), participantSourceId.getValue());
		}
		assert result.rowKeySet().size() == ParticipantMetadatum.values().length;
		assert result.columnKeySet().size() == sortedParticipantIds.size();
		return result;
	}

	private static void persistEventMetadata(final Map<EventMetadatum, String> metadataValues,
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

	private static void persistParticipantMetadata(
			final Table<ParticipantMetadatum, String, String> metadataParticipantValues, final Path outfilePath)
			throws IOException {
		// NOTE: This is not atomic: The OS could write to the file between its
		// reading and rewriting
		final RowSortedTable<String, String, String> unifiedMetadata = readParticipantMetadata(outfilePath);
		for (final Entry<ParticipantMetadatum, Map<String, String>> metadatumParticipantValues : metadataParticipantValues
				.rowMap().entrySet()) {
			final String metadatumName = metadatumParticipantValues.getKey().toString();
			for (final Entry<String, String> participantValue : metadatumParticipantValues.getValue().entrySet()) {
				unifiedMetadata.put(metadatumName, participantValue.getKey(), participantValue.getValue());
			}
		}

		for (final Entry<String, Map<String, String>> participantRowValues : unifiedMetadata.columnMap().entrySet()) {
			final String participantId = participantRowValues.getKey();
			final Map<String, String> rowValues = participantRowValues.getValue();
			// Put the participant ID as a cell value into the table so that it
			// gets printed on its own row
			rowValues.put(PARTICIPANT_METADATA_HEADER_ROW_NAME, participantId);
		}

		{
			final Stream<List<String>> metadataRows = unifiedMetadata.rowMap().entrySet().stream().map(entry -> {
				final String metadatumName = entry.getKey();
				final Map<String, String> participantValues = entry.getValue();
				final List<String> metadataRow = new ArrayList<>(participantValues.size() + 1);
				metadataRow.add(metadatumName);
				final Stream<Entry<String, String>> sortedParticipantValues = participantValues.entrySet().stream()
						.sorted(Comparator.comparing(Entry::getKey, PARTICIPANT_ID_ORDERING_COMPARATOR));
				sortedParticipantValues.forEach(participantValue -> {
					final String value = participantValue.getValue();
					metadataRow.add(value);
				});
				return metadataRow;
			});
			final Stream<String> metadataFileRows = metadataRows.map(List::stream)
					.map(stream -> stream.collect(TABLE_ROW_CELL_JOINER));
			Files.write(outfilePath, (Iterable<String>) metadataFileRows::iterator, OUTPUT_CHARSET,
					StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING);
		}
	}

	private static Map<String, List<String>> readHeadedRowMap(final Path infilePath, final int expectedMetadatumCount)
			throws IOException {
		final Map<String, List<String>> result = Maps.newHashMapWithExpectedSize(expectedMetadatumCount);
		try (BufferedReader metadataRowReader = Files.newBufferedReader(infilePath, OUTPUT_CHARSET)) {
			for (String row = metadataRowReader.readLine(); row != null; row = metadataRowReader.readLine()) {
				final List<String> rowCells = Arrays.asList(TABLE_STRING_REPR_COL_DELIMITER_PATTERN.split(row));
				final int currentRowLength = rowCells.size();
				if (currentRowLength < 2) {
					LOGGER.warn("Row has only {} value(s): {}", currentRowLength, rowCells);
				}
				final String metadatumCell = rowCells.get(0);
				final List<String> values = rowCells.subList(1, currentRowLength);
				final List<String> extantValues = result.put(metadatumCell, values);
				if (extantValues != null) {
					throw new IllegalArgumentException(
							String.format("More than one row found for metadatum \"%s\".", metadatumCell));
				}

			}
		} catch (final NoSuchFileException e) {
			LOGGER.debug("No already-persisted metadata found at \"{}\".", infilePath);
		}
		return result;
	}

	private static NavigableMap<String, String> readMetadata(final Path infilePath,
			final Comparator<? super String> metadatumNameComparator) throws IOException {
		final NavigableMap<String, String> result = new TreeMap<>(metadatumNameComparator);
		try (BufferedReader metadataRowReader = Files.newBufferedReader(infilePath, OUTPUT_CHARSET)) {
			for (String row = metadataRowReader.readLine(); row != null; row = metadataRowReader.readLine()) {
				final String[] rowCells = TABLE_STRING_REPR_COL_DELIMITER_PATTERN.split(row);
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

	private static RowSortedTable<String, String, String> readParticipantMetadata(final Path infilePath)
			throws IOException {
		final Map<String, List<String>> metadatumRows = readHeadedRowMap(infilePath,
				ESTIMATED_PARTICIPANT_METADATUM_COUNT);
		final List<String> extantParticipantIds = metadatumRows.getOrDefault(PARTICIPANT_METADATA_HEADER_ROW_NAME,
				Collections.emptyList());
		final Comparator<String> participantMetadatumNameComparator = new ParticipantMetadatumNameComparator(
				PARTICIPANT_METADATA_HEADER_ROW_NAME);
		final RowSortedTable<String, String, String> result = TreeBasedTable.create(participantMetadatumNameComparator,
				PARTICIPANT_ID_ORDERING_COMPARATOR);
		final Stream<Entry<String, List<String>>> nonHeaderRows = metadatumRows.entrySet().stream()
				.filter(metadatumRow -> !metadatumRow.getKey().equals(PARTICIPANT_METADATA_HEADER_ROW_NAME));
		nonHeaderRows.forEach(metadatumRow -> {
			final String metadatumName = metadatumRow.getKey();
			final int participantCount = extantParticipantIds.size();
			final List<String> paddedParticipantValues = new ArrayList<>(participantCount);
			paddedParticipantValues.addAll(metadatumRow.getValue());
			assert paddedParticipantValues.size() <= participantCount;
			while (paddedParticipantValues.size() < participantCount) {
				paddedParticipantValues.add("");
			}

			final Iterator<String> extantParticipantIdIter = extantParticipantIds.iterator();
			final Iterator<String> participantValueIter = paddedParticipantValues.iterator();
			while (extantParticipantIdIter.hasNext()) {
				final String extantParticipantId = extantParticipantIdIter.next();
				final String participantValue = participantValueIter.next();
				result.put(metadatumName, extantParticipantId, participantValue);
			}
		});
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

	private final LoadingCache<EventContext, String[]> eventDataRowCellValues;

	private final List<EventDatum> eventDataToDescribe;

	private final String eventOutfileNamePrefix;

	private SessionGameHistoryTabularDataWriter(final List<EventDatum> eventDataToDescribe,
			final String nullCellValueRepr) {
		this.eventDataToDescribe = eventDataToDescribe;
		eventOutfileNamePrefix = "events";
		eventDataRowCellValues = CacheBuilder.newBuilder().concurrencyLevel(1).initialCapacity(96).maximumSize(144)
				.build(new CacheLoader<EventContext, String[]>() {

					@Override
					public String[] load(final EventContext eventCtx) {
						return eventDataToDescribe.stream().map(datum -> datum.apply(eventCtx, nullCellValueRepr))
								.toArray(String[]::new);
					}
				});
	}

	private void accept(final Path[] inpaths) throws IOException, JAXBException {
		for (final Path inpath : inpaths) {
			LOGGER.info("Looking for session data underneath \"{}\".", inpath);
			final Path[] infiles = Files.walk(inpath, FileVisitOption.FOLLOW_LINKS).filter(Files::isRegularFile)
					.filter(filePath -> filePath.getFileName().toString().endsWith(".properties")).toArray(Path[]::new);
			for (final Path infile : infiles) {
				final SessionDataManager infileSessionData = SessionDataManager.create(infile);
				final SessionGameManager sessionDiagMgr = new SessionGameManager(infileSessionData);
				final SessionGame canonicalGame = sessionDiagMgr.getCanonicalGame();

				final Path infileParentDir = infile.getParent();

				final GameHistory history = canonicalGame.getHistory();
				final EntityFeatureExtractionContextFactory extractionContextFactory = new EntityFeatureExtractionContextFactory(
						new GameContextModelFactory(2), new ImageEdgeCounter());
				final EntityFeatureVectorDescriptionFactory entityFeatureVectorDescFactory = new EntityFeatureVectorDescriptionFactory(
						new EntityFeature.Extractor(), EntityFeature.getCanonicalOrdering(), extractionContextFactory,
						NULL_VALUE_REPR);
				final int entityCount = history.getEntityCount();

				int eventId = 0;
				int gameRoundId = 0;
				int gameScore = 0;
				LocalDateTime maxEventTime = LocalDateTime.MIN;
				{
					final List<Event> events = Arrays.asList(history.getEventSequence().toArray(Event[]::new));
					final List<Stream<String>> eventRows = new ArrayList<>(events.size() * entityCount);
					for (final ListIterator<Event> eventIter = events.listIterator(); eventIter.hasNext();) {
						final Event event = eventIter.next();
						// Event ID is 1-indexed
						eventId = eventIter.nextIndex();
						final GameManagementEvent eventType = GameManagementEvent.getEventType(event);
						if (GAME_ROUND_DELIMITING_EVENT_TYPE.equals(eventType)) {
							gameRoundId++;
						}
						gameScore = updateScore(eventType, gameScore);
						final LocalDateTime eventTime = EventTimes.parseEventTime(event.getTime());
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
					final Map<EventMetadatum, String> metadataValues = createEventMetadataReprMap(canonicalGame,
							sourceParticipantIds.getValue(), gameScore, entityCount, eventId, gameRoundId,
							history.getStartTime(), maxEventTime);
					{
						final String outfileName = createEventMetadataOutfileName();
						final Path outfilePath = infileParentDir == null ? Paths.get(outfileName)
								: infileParentDir.resolve(outfileName);
						LOGGER.info("Writing event metadata to \"{}\".", outfilePath);
						persistEventMetadata(metadataValues, EVENT_METADATUM_NAME_COMPARATOR, outfilePath);
					}
				}

				{
					final Table<ParticipantMetadatum, String, String> metadataValues = createParticipantMetadataReprTable(
							canonicalGame, playerSourceIds, sourceParticipantIds.getKey().inverse());
					{
						final String outfileName = createParticipantMetadataOutfileName();
						final Path outfilePath = infileParentDir == null ? Paths.get(outfileName)
								: infileParentDir.resolve(outfileName);
						LOGGER.info("Writing participant metadata to \"{}\".", outfilePath);
						persistParticipantMetadata(metadataValues, outfilePath);
					}
				}
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

	private String createEventMetadataOutfileName() {
		return eventOutfileNamePrefix + "-metadata.tsv";
	}

	private String createEventOutfileName() {
		return eventOutfileNamePrefix + ".tsv";
	}

	private String createParticipantMetadataOutfileName() {
		return "participant-metadata.tsv";
	}

	private Stream<String> createRowCellValues(
			final EntityFeatureVectorDescriptionFactory entityFeatureVectorDescFactory, final EventContext eventCtx) {
		// final Event event = eventCtx.getEvent();
		// LOGGER.debug("Processing event with name \"{}\".", event.getName());
		final Stream.Builder<String> resultBuilder = Stream.builder();
		Arrays.stream(eventDataRowCellValues.getUnchecked(eventCtx)).forEachOrdered(resultBuilder);

		final Stream<String> entityFeatureVectorReprs = entityFeatureVectorDescFactory
				.createFeatureValueReprs(eventCtx.getGameContext(), eventCtx.getEntityId());
		entityFeatureVectorReprs.forEachOrdered(resultBuilder);

		return resultBuilder.build();
	}
}
