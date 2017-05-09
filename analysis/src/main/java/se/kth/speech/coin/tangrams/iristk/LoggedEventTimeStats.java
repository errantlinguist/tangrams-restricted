/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.analysis.
 *
 *  tangrams is free software: you can redistribute it and/or modify
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
package se.kth.speech.coin.tangrams.iristk;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.Timestamp;
import java.util.DoubleSummaryStatistics;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Maps;

import iristk.system.Event;
import se.kth.speech.coin.tangrams.iristk.io.LoggedEvents;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Apr 29, 2017
 *
 */
public final class LoggedEventTimeStats {

	private static final Logger LOGGER = LoggerFactory.getLogger(LoggedEventTimeStats.class);

	public static void main(final String[] args) throws IOException {
		if (args.length != 2) {
			throw new IllegalArgumentException(
					String.format("Usage :%s MINUEND_INFILE SUBTRAHEND_INFILE", LoggedEventTimeStats.class.getSimpleName()));
		} else {

			// String testStr =
			// "{\"class\":\"iristk.system.Event\",\"event_sender\":\"LoggingModule\",\"system\":\"TangramsClient\",\"event_id\":\"LoggingModule.3823903.1\",\"event_name\":\"monitor.module.ping\",\"event_time\":\"2017-03-30
			// 13:43:27.355\"}";
			// Record testRec1 = Record.fromJSON(testStr);
			// Event testEvent1 = (Event) testRec1;
			// Record testRec2 = Record.fromJSON(testStr);
			// assert testRec1.equals(testRec2);
			// Event testEvent2 = (Event) testRec2;
			// assert testEvent1.equals(testEvent2);
			// Record testCloneRecord = testRec1.clone();
			// assert testCloneRecord.equals(testRec1);
			// Event testCloneEvent = (Event) testCloneRecord;
			// assert testCloneEvent.equals(testEvent1);
			// assert new Event(testEvent1).equals(testEvent2);
			// assert testCloneEvent.hashCode() == testEvent1.hashCode();
			final Path minuendInfile = Paths.get(args[0]);
			final Map<String, Map<Path, Timestamp>> analogousEventTimestampMappings = new HashMap<>();
			putEventTimestamps(analogousEventTimestampMappings, minuendInfile, args.length);
			final Path subtrahendInfile = Paths.get(args[1]);
			putEventTimestamps(analogousEventTimestampMappings, subtrahendInfile, args.length);
			final Map<String, Double> analogousEventTimeDiffs = Maps
					.newHashMapWithExpectedSize(analogousEventTimestampMappings.size());
			analogousEventTimestampMappings.forEach((eventId, analogousEventTimestampMapping) -> {
				final Timestamp minuendTimestamp = analogousEventTimestampMapping.get(minuendInfile);
				if (minuendTimestamp == null) {
					LOGGER.warn("Event ID \"{}\" not present in file \"{}\".", eventId, minuendInfile);
				} else {
					final Timestamp subtrahendTimestamp = analogousEventTimestampMapping.get(subtrahendInfile);
					if (subtrahendTimestamp == null) {
						LOGGER.warn("Event ID \"{}\" not present in file \"{}\".", eventId, subtrahendInfile);
					} else {
						final long diffMills = minuendTimestamp.getTime() - subtrahendTimestamp.getTime();
						final double diffSecs = diffMills / 1000.0;
						LOGGER.debug("Diff in secs: {}", diffSecs);
						analogousEventTimeDiffs.put(eventId, diffSecs);
					}
				}
			});
			final DoubleSummaryStatistics stats = analogousEventTimeDiffs.values().stream()
					.collect(Collectors.summarizingDouble(Double::doubleValue));
			System.out.println("COUNT\t" + stats.getCount());
			System.out.println("MIN\t" + stats.getMin());
			System.out.println("MAX\t" + stats.getMax());
			System.out.println("MEAN\t" + stats.getAverage());
		}
	}

	private static void putEventTimestamps(final Map<String, Map<Path, Timestamp>> analogousEventTimestampMappings,
			final Path infilePath, final int expectedAnalogousEventCount) throws IOException {
		LOGGER.info("Reading logged events from \"{}\".", infilePath);
		final Stream<Event> fileEvents = LoggedEvents.parseLoggedEvents(Files.lines(infilePath));
		fileEvents.forEach(fileEvent -> {
			final String eventId = fileEvent.getId();
			final Map<Path, Timestamp> analogousEventMapping = analogousEventTimestampMappings.computeIfAbsent(eventId,
					key -> Maps.newHashMapWithExpectedSize(expectedAnalogousEventCount));
			analogousEventMapping.put(infilePath, Timestamp.valueOf(fileEvent.getTime()));
		});
	}

}
