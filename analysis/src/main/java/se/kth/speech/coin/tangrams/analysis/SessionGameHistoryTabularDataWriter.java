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

import java.io.IOException;
import java.math.BigDecimal;
import java.nio.file.FileVisitOption;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.time.Duration;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.BiFunction;
import java.util.regex.Pattern;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.xml.bind.JAXBException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import iristk.system.Event;
import se.kth.speech.TimestampArithmetic;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeature;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeatureExtractionContextFactory;
import se.kth.speech.coin.tangrams.analysis.features.ImageEdgeCounter;
import se.kth.speech.coin.tangrams.analysis.io.SessionDataManager;
import se.kth.speech.coin.tangrams.iristk.EventTimes;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;
import se.kth.speech.coin.tangrams.iristk.io.LoggedEvents;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 23 Aug 2017
 *
 */
final class SessionGameHistoryTabularDataWriter { // NO_UCD (unused code)

	private static class EventContext {

		private final Event event;

		private final GameHistory history;

		private EventContext(final Event event, final GameHistory history) {
			this.event = event;
			this.history = history;
		}

		/**
		 * @return the event
		 */
		private Event getEvent() {
			return event;
		}

		private GameHistory getHistory() {
			return history;
		}
	}

	private enum EventDatum implements BiFunction<EventContext, String, String> {
		NAME {

			private final Pattern tangramsActionEventNamePrefixPattern = Pattern.compile("tangrams\\.action\\.");

			@Override
			public String apply(final EventContext eventCtx, final String nullValueRepr) {
				final String result;
				final Event event = eventCtx.getEvent();
				final String eventName = event.getName();
				if (eventName == null) {
					result = nullValueRepr;
				} else {
					result = tangramsActionEventNamePrefixPattern.matcher(eventName).replaceFirst("");
				}
				return result;
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
				final String result;
				final Event event = eventCtx.getEvent();
				final String eventTimestamp = event.getTime();
				if (eventTimestamp == null) {
					result = nullValueRepr;
				} else {
					final LocalDateTime eventTime = EventTimes.parseEventTime(eventTimestamp);
					final LocalDateTime gameStartTime = eventCtx.getHistory().getStartTime();
					final Duration offset = Duration.between(gameStartTime, eventTime);
					final BigDecimal offsetSecs = TimestampArithmetic.toDecimalSeconds(offset);
					result = offsetSecs.toPlainString();
				}
				return result;
			}
		};

	}

	private enum Metadatum {
		GAME_ID, START_TIME;
	}

	private static final Logger LOGGER = LoggerFactory.getLogger(SessionGameHistoryTabularDataWriter.class);

	private static final Collector<CharSequence, ?, String> TABLE_ROW_CELL_JOINER;

	private static final String TABLE_STRING_REPR_COL_DELIMITER;

	static {
		TABLE_STRING_REPR_COL_DELIMITER = "\t";
		TABLE_ROW_CELL_JOINER = Collectors.joining(TABLE_STRING_REPR_COL_DELIMITER);
	}

	public static void main(final String[] args) throws IOException, JAXBException {
		final Path[] inpaths = Arrays.stream(args).map(String::trim).filter(path -> !path.isEmpty()).map(Paths::get)
				.toArray(Path[]::new);
		if (inpaths.length < 1) {
			throw new IllegalArgumentException(
					String.format("Usage: %s INPATHS...", SessionGameHistoryTabularDataWriter.class.getSimpleName()));
		} else {
			final EntityFeatureExtractionContextFactory extractionContextFactory = new EntityFeatureExtractionContextFactory(
					new GameContextModelFactory(2), new ImageEdgeCounter());
			final String nullValueRepr = "";
			final EntityFeatureVectorDescriptionFactory entityFeatureVectorDescFactory = new EntityFeatureVectorDescriptionFactory(
					new EntityFeature.Extractor(), EntityFeature.getDefaultOrdering(), extractionContextFactory,
					nullValueRepr);
			final SessionGameHistoryTabularDataWriter writer = new SessionGameHistoryTabularDataWriter(
					entityFeatureVectorDescFactory, Arrays.asList(EventDatum.values()), "events", nullValueRepr);
			writer.accept(inpaths);
		}
	}

	private final List<String> dataColumnNames;

	private final EntityFeatureVectorDescriptionFactory entityFeatureVectorDescFactory;

	private final List<EventDatum> eventDataToDescribe;

	private final String nullCellValueRepr;

	private final String outfileNamePrefix;

	private SessionGameHistoryTabularDataWriter(
			final EntityFeatureVectorDescriptionFactory entityFeatureVectorDescFactory,
			final List<EventDatum> eventDataToDescribe, final String outfileNamePrefix,
			final String nullCellValueRepr) {
		this.entityFeatureVectorDescFactory = entityFeatureVectorDescFactory;
		this.eventDataToDescribe = eventDataToDescribe;
		this.outfileNamePrefix = outfileNamePrefix;
		this.nullCellValueRepr = nullCellValueRepr;

		dataColumnNames = Arrays.asList(createColumnNames().toArray(String[]::new));
	}

	private void accept(final Path[] inpaths) throws IOException, JAXBException {
		for (final Path inpath : inpaths) {
			LOGGER.info("Looking for session data underneath \"{}\".", inpath);
			final Path[] infiles = Files.walk(inpath, FileVisitOption.FOLLOW_LINKS).filter(Files::isRegularFile)
					.filter(filePath -> filePath.getFileName().toString().endsWith(".properties")).toArray(Path[]::new);
			for (final Path infile : infiles) {
				final SessionDataManager infileSessionData = SessionDataManager.create(infile);
				final Path eventLogPath = infileSessionData.getCanonicalEventLogPath();
				LOGGER.info("Reading events from \"{}\".", eventLogPath);
				final Map<String, GameHistory> gameHistories;
				try (Stream<Event> eventStream = LoggedEvents.readLoggedEvents(eventLogPath)) {
					gameHistories = LoggedEvents.createGameHistoryMap(eventStream);
				}

				final Path infileParentDir = infile.getParent();

				final Entry<String, GameHistory> gameIdHistory = GameHistory.ensureSingleGame(gameHistories);
				final GameHistory history = gameIdHistory.getValue();

				{
					final Stream<Event> events = history.getEventSequence();
					final Stream<Stream<String>> eventRows = events
							.map(event -> createRowCellValues(new EventContext(event, history)));
					final Stream<String> fileRows = Stream.concat(Stream.of(dataColumnNames.stream()), eventRows)
							.map(cells -> cells.collect(TABLE_ROW_CELL_JOINER));
					{
						final String outfileName = createEventsOutfileName();
						final Path outfilePath = infileParentDir == null ? Paths.get(outfileName)
								: infileParentDir.resolve(outfileName);
						LOGGER.info("Writing tabular event data to \"{}\".", outfilePath);
						Files.write(outfilePath, (Iterable<String>) fileRows::iterator, LoggedEvents.CHARSET,
								StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING);
					}
				}

				{
					final Map<Metadatum, Object> metadataValues = new EnumMap<>(Metadatum.class);
					final String gameId = gameIdHistory.getKey();
					metadataValues.put(Metadatum.GAME_ID, gameId);
					metadataValues.put(Metadatum.START_TIME, history.getStartTime());
					assert metadataValues.size() == Metadatum.values().length;
					{
						final String outfileName = createEventsMetadataOutfileName();
						final Path outfilePath = infileParentDir == null ? Paths.get(outfileName)
								: infileParentDir.resolve(outfileName);
						LOGGER.info("Writing metadata to \"{}\".", outfilePath);
						final Stream<Stream<Object>> metadataRows = metadataValues.entrySet().stream()
								.map(entry -> new Object[] { entry.getKey(), entry.getValue() }).map(Arrays::stream);
						final Stream<String> metadataFileRows = metadataRows
								.map(stream -> stream.map(Object::toString).collect(TABLE_ROW_CELL_JOINER));
						Files.write(outfilePath, (Iterable<String>) metadataFileRows::iterator, LoggedEvents.CHARSET,
								StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING);
					}
				}
			}
		}
	}

	private Stream<String> createColumnNames() {
		final Stream.Builder<String> resultBuilder = Stream.builder();
		final Stream<String> eventDatumNames = eventDataToDescribe.stream().map(EventDatum::toString);
		eventDatumNames.forEachOrdered(resultBuilder);
		final Stream<String> featureVectorColumnNames = entityFeatureVectorDescFactory.getEntityFeaturesToDescribe()
				.stream().map(EntityFeature::toString);
		featureVectorColumnNames.forEachOrdered(resultBuilder);
		return resultBuilder.build();
	}

	private Map<EventDatum, String> createEventDataMap(final EventContext eventCtx) {
		final Map<EventDatum, String> result = new EnumMap<>(EventDatum.class);
		eventDataToDescribe.stream().forEach(datum -> {
			final String datumValue = datum.apply(eventCtx, nullCellValueRepr);
			result.put(datum, datumValue);
		});
		assert result.size() == eventDataToDescribe.size();
		return result;
	}

	private String createEventsMetadataOutfileName() {
		return outfileNamePrefix + "-metadata.tsv";
	}

	private String createEventsOutfileName() {
		return outfileNamePrefix + ".tsv";
	}

	private Stream<String> createRowCellValues(final EventContext eventCtx) {
		final Stream<String> result;

		final Event event = eventCtx.getEvent();
		LOGGER.debug("Processing event with name \"{}\".", event.getName());
		final Stream.Builder<String> resultBuilder = Stream.builder();
		final Map<EventDatum, String> eventData = createEventDataMap(eventCtx);
		eventData.values().forEach(resultBuilder);

		final LocalDateTime eventTime = EventTimes.parseEventTime(event.getTime());
		final GameContext gameContext = new GameContext(eventCtx.getHistory(), eventTime);
		final Stream<String> entityFeatureVectorReprs = entityFeatureVectorDescFactory
				.createFeatureValueReprs(gameContext);
		entityFeatureVectorReprs.forEachOrdered(resultBuilder);

		result = resultBuilder.build();

		return result;
	}
}
