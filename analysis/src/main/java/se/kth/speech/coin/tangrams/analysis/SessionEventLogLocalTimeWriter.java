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
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NavigableSet;
import java.util.Objects;
import java.util.TreeSet;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.xml.bind.JAXBException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.eclipsesource.json.JsonObject;
import com.google.common.collect.Maps;

import iristk.system.Event;
import se.kth.speech.EqualityMap;
import se.kth.speech.coin.tangrams.iristk.EventTimes;
import se.kth.speech.coin.tangrams.iristk.io.LoggedEvents;

/**
 * A class used for re-writing log times based on when the events were written
 * to the log on the sender's local machine rather than when they were written
 * to the log on receiver's local machine.
 *
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 7 Aug 2017
 *
 */
final class SessionEventLogLocalTimeWriter {

	// private static class EventTimeDatum {
	//
	// private final Map<Event, NavigableSet<LocalDateTime>>
	// analogousEventTimes;
	//
	// private final Map<Path, NavigableSet<Event>> fileEventsOrderedByTime;
	//
	// private EventTimeDatum(final int fileCount) {
	// fileEventsOrderedByTime = Maps.newHashMapWithExpectedSize(fileCount);
	// analogousEventTimes = new EqualityMap<>(EXPECTED_AVERAGE_EVENT_COUNT);
	// }
	//
	// /*
	// * (non-Javadoc)
	// *
	// * @see java.lang.Object#toString()
	// */
	// @Override
	// public String toString() {
	// final StringBuilder builder = new StringBuilder();
	// builder.append("EventTimeDatum [analogousEventTimes=");
	// builder.append(analogousEventTimes);
	// builder.append(", fileEventsOrderedByTime=");
	// builder.append(fileEventsOrderedByTime);
	// builder.append("]");
	// return builder.toString();
	// }
	//
	// private void add(final Path file, final Event event) {
	// LOGGER.debug("Adding \"{}\".", event);
	// final NavigableSet<Event> eventsOrderedByTime =
	// fileEventsOrderedByTime.computeIfAbsent(file,
	// key -> new
	// TreeSet<>(Comparator.comparing(SessionEventLogLocalTimeWriter::fetchEventTime)));
	// eventsOrderedByTime.add(event);
	//
	// {
	// final LocalDateTime eventTime = fetchEventTime(event);
	// final NavigableSet<LocalDateTime> times = analogousEventTimes
	// .computeIfAbsent(fetchEventWithoutTime(event), key -> new TreeSet<>());
	// times.add(eventTime);
	// }
	// }
	//
	// }

	private static final int EXPECTED_AVERAGE_EVENT_COUNT;

	private static final Logger LOGGER = LoggerFactory.getLogger(SessionEventLogLocalTimeWriter.class);

	static {
		EXPECTED_AVERAGE_EVENT_COUNT = 150;
	}

	public static void main(final String[] args) throws JAXBException, IOException {
		final Path[] inpaths = Arrays.stream(args).map(String::trim).filter(path -> !path.isEmpty()).map(Paths::get)
				.toArray(Path[]::new);
		if (inpaths.length < 1) {
			throw new IllegalArgumentException(
					String.format("Usage: %s INPATHS...", SessionEventLogLocalTimeWriter.class.getSimpleName()));
		} else {
			final SessionEventLogLocalTimeWriter writer = new SessionEventLogLocalTimeWriter(
					EXPECTED_AVERAGE_EVENT_COUNT);
			for (final Path inpath : inpaths) {
				LOGGER.info("Will read batch job data from \"{}\".", inpath);
				writer.processSessions(inpath);
			}
		}

	}

	private static int compareSets(final Collection<LocalDateTime> first, final Collection<LocalDateTime> second) {
		if (first.size() < second.size()) {
			return compareSets(second, first);
		} else {
			final Iterator<LocalDateTime> firstIter = first.iterator();
			final Iterator<LocalDateTime> secondIter = second.iterator();
			int result = 0;
			while (firstIter.hasNext()) {
				final LocalDateTime firstTime = firstIter.next();
				if (secondIter.hasNext()) {
					final LocalDateTime secondTime = secondIter.next();
					final int intermediateResult = firstTime.compareTo(secondTime);
					if (result == 0) {
						result = intermediateResult;
					} else if (result < 0 && !(intermediateResult <= 0)) {
						throw new IllegalArgumentException("Events have different order in lists.");
					} else if (result > 0 && !(intermediateResult >= 0)) {
						throw new IllegalArgumentException("Events have different order in lists.");
					}
				}
			}
			return result;
		}
	}

	private final Map<Event, Event> eventsWithoutTime;

	// private static EventTimeDatum createEventTimeData(final Collection<Path>
	// playerEventLogs) throws IOException {
	// final EventTimeDatum result = new EventTimeDatum(playerEventLogs.size());
	// for (final Path eventLogPath : playerEventLogs) {
	// LOGGER.debug("Reading event log at \"{}\".", eventLogPath);
	// try (Stream<Event> loggedEvents =
	// LoggedEvents.readLoggedEvents(eventLogPath)) {
	// loggedEvents.forEach(event -> result.add(eventLogPath, event));
	// }
	// }
	// return result;
	// }

	private final Map<Event, LocalDateTime> eventTimes;

	private SessionEventLogLocalTimeWriter(final int expectedEventCount) {
		eventsWithoutTime = new HashMap<>(expectedEventCount);
		eventTimes = new HashMap<>(expectedEventCount);
	}

	private boolean areEventsAnalogous(final Event first, final Event second) {
		final Event firstWithoutTime = fetchEventWithoutTime(first);
		final Event secondWithoutTime = fetchEventWithoutTime(second);
		return Objects.equals(firstWithoutTime, secondWithoutTime);
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

	private void processSession(final Path infilePath) throws IOException {
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
		// analogousEventTimes.forEach((event, times) -> {
		// System.out.println("Event: " + event);
		// System.out.println("Times: " + times);
		// });

		final Stream<Entry<Event, NavigableSet<LocalDateTime>>> sortedAnalogousEventTimes = analogousEventTimes
				.entrySet().stream().sorted(Comparator.comparing(entry -> {
					NavigableSet<LocalDateTime> times = entry.getValue();
					return times.first();
				}));
		Stream<String> jsonLines = sortedAnalogousEventTimes.map(eventTimes -> {
			Event eventWithTime = (Event) eventTimes.getKey().deepClone();
			eventWithTime.setTime(EventTimes.FORMATTER.format(eventTimes.getValue().first()));
			return eventWithTime;
		}).map(Event::toJSON).map(JsonObject::toString);
		jsonLines.forEach(System.out::println);
		
		// TODO: Finish
		// Sanity check
		// eventLogEventsWithoutTime.forEach((firstPath,
		// firstEventListWithoutTime) -> {
		// eventLogEventsWithoutTime.forEach((nextPath,
		// nextEventListWithoutTime) -> {
		// if (!firstEventListWithoutTime.equals(nextEventListWithoutTime)){
		// throw new IllegalArgumentException(String.format("Ordering of events
		// in file \"%s\" does not match that of \"%s\".", firstPath,
		// nextPath));
		// }
		// });
		// });

		// Stream<List<Event>> eventLists =
		// eventLogEventsWithoutTime.values().stream();
		// List<Event> eventLogEventStreamWithoutTimeIter =
		// eventLists.reduce((first, second) ->
		// Arrays.asList(Lists.unifySequenceElements(first,
		// second).toArray(Event[]::new))).get();
		// eventLogEventStreamWithoutTimeIter.stream().map(this::fe)
		// for (Event nextEvent : eventLogEventStreamWithoutTimeIter){
		// System.out.println(nextEvent);
		// }

		// Collection<Entry<Path, List<Event>>> logFileEventLists =
		// eventLogEventsWithoutTime.entrySet();
		// List<Entry<Path, ListIterator<Event>>> logFileEventListIters =
		// logFileEventLists.stream()
		// .map(logFileEventListPair -> Pair.of(logFileEventListPair.getKey(),
		// logFileEventListPair.getValue().listIterator()))
		// .collect(Collectors.toList());
		// Set<Entry<Path, Iterator<Event>>> exhaustedLogFileEventListIters =
		// Sets
		// .newHashSetWithExpectedSize(logFileEventListIters.size());
		// while (exhaustedLogFileEventListIters.size() <
		// logFileEventListIters.size()) {
		// List<Event> nextEvents = new
		// ArrayList<>(logFileEventListIters.size());
		// for (Entry<Path, ListIterator<Event>> firstLogFileEventListIterPair :
		// logFileEventListIters) {
		// Path logFile = logFileEventListIterPair.getKey();
		// Iterator<Event> eventListIter = logFileEventListIterPair.getValue();
		// if (eventListIter.hasNext()) {
		// Event nextEvent = eventListIter.next();
		// nextEvents.add(nextEvent);
		// } else {
		// exhaustedLogFileEventListIters.add(logFileEventListIterPair);
		// }
		// }
		// System.out.println("Next events:");
		// nextEvents.forEach(System.out::println);
		// }
	}

	private void processSessions(final Path inpath) throws JAXBException, IOException {
		final Path[] infilePaths = Files.walk(inpath, FileVisitOption.FOLLOW_LINKS).filter(Files::isRegularFile)
				.filter(filePath -> filePath.getFileName().toString().endsWith(".properties")).toArray(Path[]::new);
		for (final Path infilePath : infilePaths) {
			LOGGER.info("Reading batch job properties from \"{}\".", infilePath);
			processSession(infilePath);
		}
	}

}
