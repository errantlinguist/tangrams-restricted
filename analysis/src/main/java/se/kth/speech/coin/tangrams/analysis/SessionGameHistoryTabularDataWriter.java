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
import java.io.FileNotFoundException;
import java.io.IOException;
import java.math.BigDecimal;
import java.nio.charset.Charset;
import java.nio.file.FileVisitOption;
import java.nio.file.Files;
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

import com.eclipsesource.json.Json;
import com.eclipsesource.json.JsonObject;
import com.eclipsesource.json.JsonValue;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;

import iristk.system.Event;
import se.kth.speech.TimestampArithmetic;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeature;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeatureExtractionContextFactory;
import se.kth.speech.coin.tangrams.analysis.features.ImageEdgeCounter;
import se.kth.speech.coin.tangrams.analysis.io.SessionDataManager;
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

	private enum Metadatum {
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
		SOURCE_PARTICIPANT_IDS {
			@Override
			protected JsonObject parseValue(final String value) {
				final JsonValue parsedVal = Json.parse(value);
				return (JsonObject) parsedVal;
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

	private static final GameManagementEvent GAME_ROUND_DELIMITING_EVENT_TYPE = GameManagementEvent.NEXT_TURN_REQUEST;

	private static final Logger LOGGER = LoggerFactory.getLogger(SessionGameHistoryTabularDataWriter.class);

	private static final Comparator<String> METADATUM_NAME_COMPARATOR = new Comparator<String>() {

		@Override
		public int compare(final String o1, final String o2) {
			int result;

			final Metadatum m1 = parseNullableMetadatum(o1);
			final Metadatum m2 = parseNullableMetadatum(o2);
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

		private Metadatum parseNullableMetadatum(final String name) {
			Metadatum result = null;
			try {
				result = Metadatum.valueOf(name);
			} catch (final IllegalArgumentException e) {
				LOGGER.debug(String.format("Unable to parse \"%s\" as an instance of %s; Returning null.", name,
						Metadatum.class), e);
			}
			return result;
		}

	};

	private static final String NULL_VALUE_REPR;

	private static final ZoneId ORIGINAL_EXPERIMENT_TIMEZONE = ZoneId.of("Europe/Stockholm");

	private static final Charset OUTPUT_CHARSET = LoggedEvents.CHARSET;

	private static final DateTimeFormatter OUTPUT_DATETIME_FORMATTER = DateTimeFormatter.ISO_OFFSET_DATE_TIME;

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
					EventDatum.CANONICAL_ORDERING, "events", NULL_VALUE_REPR);
			writer.accept(inpaths);
		}
	}

	// private static Optional<Entry<Metadatum, Object>> parseMetadatumRow(final
	// String[] rowCells) {
	// Optional<Entry<Metadatum, Object>> result;
	// if (rowCells.length != 2) {
	// throw new IllegalArgumentException(
	// String.format("Row %s is not a valid metadatum-value pair.",
	// Arrays.toString(rowCells)));
	// } else {
	// result = Optional.empty();
	// final String metadatumCell = rowCells[0];
	// try {
	// final Metadatum metadatum = Metadatum.valueOf(metadatumCell);
	// final Object value = metadatum.parseValue(rowCells[1]);
	// result = Optional.of(Pair.of(metadatum, value));
	// } catch (final IllegalArgumentException e) {
	// LOGGER.info("Row key \"{}\" is not a known constant metadatum;
	// Ignoring.", metadatumCell);
	// }
	// }
	// return result;
	// }

	private static JsonObject createJsonMapObject(final Iterable<Entry<String, String>> entries) {
		final JsonObject result = new JsonObject();
		for (final Entry<String, String> entry : entries) {
			result.add(entry.getKey(), entry.getValue());
		}
		return result;
	}

	private static void persistMetadata(final Map<Metadatum, String> metadataValues, final Path outfilePath)
			throws IOException {
		// NOTE: This is not atomic; The OS could write to the file between its
		// reading and rewriting
		final NavigableMap<String, String> unifiedMetadata = readMetadata(outfilePath);
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

	private static NavigableMap<String, String> readMetadata(final Path infilePath) throws IOException {
		// final Map<String, String> result =
		// Maps.newHashMapWithExpectedSize(Math.max(Metadatum.values().length,
		// 16));
		final NavigableMap<String, String> result = new TreeMap<>(METADATUM_NAME_COMPARATOR);
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
		} catch (final FileNotFoundException e) {
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

	private final LoadingCache<EventContext, String[]> eventDataRowCellValues;

	private final List<EventDatum> eventDataToDescribe;

	private final String outfileNamePrefix;

	private SessionGameHistoryTabularDataWriter(final List<EventDatum> eventDataToDescribe,
			final String outfileNamePrefix, final String nullCellValueRepr) {
		this.eventDataToDescribe = eventDataToDescribe;
		this.outfileNamePrefix = outfileNamePrefix;
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
						final String outfileName = createEventsOutfileName();
						final Path outfilePath = infileParentDir == null ? Paths.get(outfileName)
								: infileParentDir.resolve(outfileName);
						LOGGER.info("Writing tabular event data to \"{}\".", outfilePath);
						Files.write(outfilePath, (Iterable<String>) fileRows::iterator, OUTPUT_CHARSET,
								StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING);
					}
				}

				{
					final Map<Metadatum, String> metadataValues = new EnumMap<>(Metadatum.class);
					metadataValues.put(Metadatum.END_SCORE, Integer.toString(gameScore));
					metadataValues.put(Metadatum.ENTITY_COUNT, Integer.toString(entityCount));
					metadataValues.put(Metadatum.EVENT_COUNT, Integer.toString(eventId));
					{
						final BigDecimal durationInSecs = TimestampArithmetic
								.toDecimalSeconds(Duration.between(history.getStartTime(), maxEventTime));
						metadataValues.put(Metadatum.GAME_DURATION, durationInSecs.toString());
					}
					metadataValues.put(Metadatum.GAME_ID, canonicalGame.getGameId());
					{
						final Entry<Map<String, String>, String> sourceParticipantIds = new SourceParticipantIdMapFactory()
								.apply(infileSessionData, sessionDiagMgr);
						metadataValues.put(Metadatum.SOURCE_PARTICIPANT_IDS,
								createJsonMapObject(sourceParticipantIds.getKey().entrySet()).toString());
						metadataValues.put(Metadatum.INITIAL_INSTRUCTOR_ID, sourceParticipantIds.getValue());

					}
					metadataValues.put(Metadatum.ROUND_COUNT, Integer.toString(gameRoundId));
					{
						final ZonedDateTime zonedGameStart = history.getStartTime()
								.atZone(ORIGINAL_EXPERIMENT_TIMEZONE);
						metadataValues.put(Metadatum.START_TIME, OUTPUT_DATETIME_FORMATTER.format(zonedGameStart));
					}
					assert metadataValues.size() == Metadatum.values().length;

					{
						final String outfileName = createEventsMetadataOutfileName();
						final Path outfilePath = infileParentDir == null ? Paths.get(outfileName)
								: infileParentDir.resolve(outfileName);
						LOGGER.info("Writing metadata to \"{}\".", outfilePath);
						persistMetadata(metadataValues, outfilePath);
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

	private String createEventsMetadataOutfileName() {
		return outfileNamePrefix + "-metadata.tsv";
	}

	private String createEventsOutfileName() {
		return outfileNamePrefix + ".tsv";
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
