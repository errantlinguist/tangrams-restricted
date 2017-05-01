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
import java.io.UncheckedIOException;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.Timestamp;
import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Maps;
import com.google.common.collect.Sets;

import iristk.system.Event;
import se.kth.speech.BigDecimalSummaryStatistics;
import se.kth.speech.coin.tangrams.iristk.io.LoggedEvents;
import se.kth.speech.collections.EqualityMap;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Apr 29, 2017
 *
 */
public final class LoggedEventTimeStats {

	private static class EventTimestampSubtractor implements Function<Event, BigDecimal> {

		private final Timestamp minuendTimestamp;

		private EventTimestampSubtractor(final Timestamp minuendTimestamp) {
			this.minuendTimestamp = minuendTimestamp;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.util.function.Function#apply(java.lang.Object)
		 */
		@Override
		public BigDecimal apply(final Event subtrahendEvent) {
			final Timestamp subtrahendTimestamp = Timestamp.valueOf(subtrahendEvent.getTime());
			final BigDecimal diffMills = new BigDecimal(minuendTimestamp.getTime() - subtrahendTimestamp.getTime());
			return diffMills.divide(ONE_THOUSAND);
		}
	}

	private static final Logger LOGGER = LoggerFactory.getLogger(LoggedEventTimeStats.class);

	private static final BigDecimal ONE_THOUSAND = new BigDecimal(1000);

	public static void main(final String[] args) throws IOException {
		if (args.length < 1) {
			System.err.println(String.format("Usage :%s INFILES...", LoggedEventTimeStats.class.getName()));
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

			final Stream<Path> infilePaths = Arrays.stream(args).map(Paths::get);
			final Map<Event, Set<Event>> analogousEventSets = new EqualityMap<>();
			infilePaths.forEach(infilePath -> {
				Stream<Event> fileEvents;
				LOGGER.info("Reading logged events from \"{}\".", infilePath);
				try {
					fileEvents = LoggedEvents.parseLoggedEvents(Files.lines(infilePath));
					fileEvents.forEach(fileEvent -> {
						final Event clone = (Event) fileEvent.clone();
						assert clone.equals(fileEvent);
						clone.setTime(null);
						final Set<Event> analogousEventSet = analogousEventSets.computeIfAbsent(clone,
								key -> Sets.newHashSetWithExpectedSize(args.length));
						analogousEventSet.add(fileEvent);
					});
				} catch (final IOException e) {
					throw new UncheckedIOException(e);
				}
			});
			final Map<Event, Map<Event, BigDecimal>> analogousEventTimeDiffTable = Maps
					.newHashMapWithExpectedSize(analogousEventSets.size() * args.length);
			analogousEventSets.values().stream().forEach(analogousEventSet -> {
				for (final Event event1 : analogousEventSet) {
					final Timestamp timestamp1 = Timestamp.valueOf(event1.getTime());
					for (final Event event2 : analogousEventSet) {
						final Timestamp timestamp2 = Timestamp.valueOf(event2.getTime());
						final int cmp = timestamp1.compareTo(timestamp2);
						if (cmp < 0) {
							final Event minuendEvent = event2;
							final EventTimestampSubtractor subtractor = new EventTimestampSubtractor(timestamp2);
							final Event subtrahendEvent = event1;
							final Map<Event, BigDecimal> analogousEventTimeDiffs = analogousEventTimeDiffTable
									.computeIfAbsent(minuendEvent,
											key -> Maps.newHashMapWithExpectedSize(analogousEventSet.size()));
							final BigDecimal diffSecs = analogousEventTimeDiffs.computeIfAbsent(subtrahendEvent,
									subtractor);
							LOGGER.debug("Diff in secs: {}", diffSecs);
						} else if (cmp > 1) {
							final Event minuendEvent = event1;
							final EventTimestampSubtractor subtractor = new EventTimestampSubtractor(timestamp1);
							final Event subtrahendEvent = event2;
							final Map<Event, BigDecimal> analogousEventTimeDiffs = analogousEventTimeDiffTable
									.computeIfAbsent(minuendEvent,
											key -> Maps.newHashMapWithExpectedSize(analogousEventSet.size()));
							final BigDecimal diffSecs = analogousEventTimeDiffs.computeIfAbsent(subtrahendEvent,
									subtractor);
							LOGGER.debug("Diff in secs: {}", diffSecs);
						} else {
							// Do nothing
							LOGGER.debug("Ignoring equal timestamp values.");
						}
					}
				}
			});
			final Stream<BigDecimal> analogousEventTimeDiffs = analogousEventTimeDiffTable.values().stream()
					.map(Map::values).flatMap(Collection::stream);
			final BigDecimalSummaryStatistics stats = new BigDecimalSummaryStatistics();
			analogousEventTimeDiffs.forEach(stats);
			System.out.println("COUNT\t" + stats.getCount());
			System.out.println("MIN\t" + stats.getMin());
			System.out.println("MAX\t" + stats.getMax());
			System.out.println("MEAN\t" + stats.getAverage());
		}
	}

}
