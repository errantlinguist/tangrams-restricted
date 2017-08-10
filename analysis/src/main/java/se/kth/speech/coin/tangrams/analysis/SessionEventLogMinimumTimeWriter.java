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
import java.nio.file.FileVisitOption;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NavigableSet;
import java.util.TreeSet;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.eclipsesource.json.JsonObject;
import com.google.common.collect.Maps;

import iristk.system.Event;
import se.kth.speech.EqualityMap;
import se.kth.speech.coin.tangrams.analysis.io.SessionDataManager;
import se.kth.speech.coin.tangrams.iristk.EventTimes;
import se.kth.speech.coin.tangrams.iristk.io.LoggedEvents;

/**
 * A class used for re-writing log times based on the earliest time when the
 * events were written to the log on any sender's machine.
 *
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 7 Aug 2017
 *
 */
final class SessionEventLogMinimumTimeWriter {

	private static final int EXPECTED_AVERAGE_EVENT_COUNT = 150;

	private static final Logger LOGGER = LoggerFactory.getLogger(SessionEventLogMinimumTimeWriter.class);

	public static void main(final String[] args) throws IOException {
		final Path[] inpaths = Arrays.stream(args).map(String::trim).filter(path -> !path.isEmpty()).map(Paths::get)
				.toArray(Path[]::new);
		if (inpaths.length < 1) {
			throw new IllegalArgumentException(
					String.format("Usage: %s INPATHS...", SessionEventLogMinimumTimeWriter.class.getSimpleName()));
		} else {
			final SessionEventLogMinimumTimeWriter writer = new SessionEventLogMinimumTimeWriter(
					EXPECTED_AVERAGE_EVENT_COUNT);
			for (final Path inpath : inpaths) {
				LOGGER.info("Will read batch job data from \"{}\".", inpath);
				writer.accept(inpath);
			}
		}

	}

	private static Path createOutfilePath(final Path infilePath) {
		final Path indirPath = infilePath.getParent();
		Path result;
		final String outfileName = "events-minimumlogtimes.txt";
		if (indirPath == null) {
			result = Paths.get(outfileName);
		} else {
			result = indirPath.resolve(outfileName);
		}
		return result;
	}

	private final Map<Event, Event> eventsWithoutTime;

	private final Map<Event, LocalDateTime> eventTimes;

	private SessionEventLogMinimumTimeWriter(final int expectedEventCount) {
		eventsWithoutTime = new EqualityMap<>(expectedEventCount);
		eventTimes = new EqualityMap<>(expectedEventCount);
	}

	private void accept(final Path inpath) throws IOException {
		final Path[] infilePaths = Files.walk(inpath, FileVisitOption.FOLLOW_LINKS).filter(Files::isRegularFile)
				.filter(filePath -> filePath.getFileName().toString().endsWith(".properties")).toArray(Path[]::new);
		for (final Path infilePath : infilePaths) {
			LOGGER.info("Reading batch job properties from \"{}\".", infilePath);
			processSession(infilePath, createOutfilePath(infilePath));
		}
	}

	private LocalDateTime fetchEventTime(final Event event) {
		return eventTimes.computeIfAbsent(event, evt -> {
			final String timestamp = evt.getTime();
			LOGGER.debug("Parsing timestamp \"{}\".", timestamp);
			return EventTimes.parseEventTime(timestamp);
		});
	}

	private Event fetchEventWithoutTime(final Event event) {
		return eventsWithoutTime.computeIfAbsent(event, evt -> {
			final Event eventWithoutTime = (Event) event.deepClone();
			eventWithoutTime.setTime(null);
			return eventWithoutTime;
		});
	}

	private void processSession(final Path infilePath, final Path outfilePath) throws IOException {
		final SessionDataManager sessionData = SessionDataManager.create(infilePath);
		final Map<String, Path> playerEventLogs = sessionData.getPlayerData().getPlayerEventLogs();
		final Collection<Path> eventLogPaths = playerEventLogs.values();
		final Map<Path, NavigableSet<Event>> eventLogEvents = Maps.newHashMapWithExpectedSize(eventLogPaths.size());
		for (final Path eventLogPath : eventLogPaths) {
			LOGGER.debug("Reading event log at \"{}\".", eventLogPath);
			try (final Stream<Event> events = LoggedEvents.readLoggedEvents(eventLogPath)) {
				final NavigableSet<Event> eventsByTime = events.collect(
						Collectors.toCollection(() -> new TreeSet<>(Comparator.comparing(this::fetchEventTime))));
				eventLogEvents.put(eventLogPath, eventsByTime);
			}
		}
		LOGGER.info("Read {} log file(s).", eventLogEvents.size());

		final Map<Event, NavigableSet<LocalDateTime>> analogousEventTimes = new EqualityMap<>(
				EXPECTED_AVERAGE_EVENT_COUNT);
		eventLogEvents.values().stream().flatMap(Collection::stream).forEach(event -> {
			final Event eventWithoutTime = fetchEventWithoutTime(event);
			final LocalDateTime eventTime = fetchEventTime(event);
			analogousEventTimes.computeIfAbsent(eventWithoutTime, key -> new TreeSet<>()).add(eventTime);
		});
		LOGGER.info("Processed {} unique event(s).", analogousEventTimes.size());

		final Stream<Entry<Event, NavigableSet<LocalDateTime>>> sortedAnalogousEventTimes = analogousEventTimes
				.entrySet().stream().sorted(Comparator.comparing(entry -> {
					final NavigableSet<LocalDateTime> times = entry.getValue();
					return times.first();
				}));
		final Stream<String> jsonLines = sortedAnalogousEventTimes.map(eventTimes -> {
			final Event eventWithTime = (Event) eventTimes.getKey().deepClone();
			eventWithTime.setTime(EventTimes.FORMATTER.format(eventTimes.getValue().first()));
			return eventWithTime;
		}).map(Event::toJSON).map(JsonObject::toString);
		LOGGER.info("Writing to \"{}\".", outfilePath);
		// https://stackoverflow.com/a/20130475/1391325
		Files.write(outfilePath, (Iterable<String>) jsonLines::iterator, LoggedEvents.CHARSET,
				StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING);
	}
}
