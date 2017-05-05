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

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map.Entry;
import java.util.function.BiFunction;
import java.util.function.Predicate;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import iristk.system.Event;
import se.kth.speech.MutablePair;
import se.kth.speech.TimestampArithmetic;
import se.kth.speech.coin.tangrams.iristk.EventTimes;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 4 May 2017
 *
 */
public final class EventUtteranceFactory
		implements BiFunction<Iterable<Utterance>, GameHistory, Stream<Entry<Event, List<Utterance>>>> {

	private static final Logger LOGGER = LoggerFactory.getLogger(EventUtteranceFactory.class);

	private final Predicate<? super Event> eventFilter;

	private final long timeWindow;

	public EventUtteranceFactory() {
		this(event -> true, 0);
	}

	public EventUtteranceFactory(final Predicate<? super Event> eventFilter) {
		this(eventFilter, 0);
	}

	public EventUtteranceFactory(final Predicate<? super Event> eventFilter, final long timeWindow) {
		this.eventFilter = eventFilter;
		this.timeWindow = timeWindow;
	}

	@Override
	public Stream<Entry<Event, List<Utterance>>> apply(final Iterable<Utterance> utts, final GameHistory history) {
		final Stream<Event> events = history.getEvents().values().stream().flatMap(List::stream).filter(eventFilter);
		final LocalDateTime gameStartTime = history.getStartTime();

		final Iterator<Utterance> uttIter = utts.iterator();
		final Iterator<Event> eventIter = events.iterator();
		Event currentEvent = eventIter.next();
		final Stream.Builder<Entry<Event, List<Utterance>>> resultBuilder = Stream.builder();

		final List<Utterance> firstUtts = createPreEventUtteranceList(uttIter, currentEvent, gameStartTime);
		if (firstUtts.isEmpty()) {
			LOGGER.debug("No utterances before first event.");
		} else {
			resultBuilder.accept(new MutablePair<>(null, firstUtts));
		}

		// Find the next set of utterances following each event
		while (eventIter.hasNext()) {
			final Event nextEvent = eventIter.next();
			LOGGER.debug("Next event: {}", nextEvent);
			List<Utterance> nextUttList = new ArrayList<>();
			final LocalDateTime nextEventTimestamp = EventTimes.parseEventTime(nextEvent.getTime())
					.plusSeconds(timeWindow);
			eventUtts: while (uttIter.hasNext()) {
				final Utterance nextUtt = uttIter.next();
				final LocalDateTime uttStartTimestamp = TimestampArithmetic.createOffsetTimestamp(gameStartTime,
						nextUtt.getStartTime());
				// If the utterance was before the next event, add
				// it to the
				// list of utterances for the current event
				if (nextEventTimestamp.isAfter(uttStartTimestamp)) {
					nextUttList.add(nextUtt);
				} else {
					resultBuilder.accept(new MutablePair<>(currentEvent, nextUttList));
					nextUttList = new ArrayList<>();
					nextUttList.add(nextUtt);
					break eventUtts;
				}
			}
			currentEvent = nextEvent;
		}

		// Get the utterances after the last event
		if (uttIter.hasNext()) {
			final List<Utterance> lastEventUtts = new ArrayList<>();
			do {
				final Utterance nextUtt = uttIter.next();
				lastEventUtts.add(nextUtt);
			} while (uttIter.hasNext());
			resultBuilder.accept(new MutablePair<>(currentEvent, lastEventUtts));
		}
		return resultBuilder.build();
	}

	private List<Utterance> createPreEventUtteranceList(final Iterator<Utterance> uttIter, final Event firstEvent,
			final LocalDateTime gameStartTime) {
		final List<Utterance> result;

		if (uttIter.hasNext()) {
			result = new ArrayList<>();
			{
				// Find all utterances up to the first event
				LOGGER.debug("First event: {}", firstEvent);
				final LocalDateTime firstEventTimestamp = EventTimes.parseEventTime(firstEvent.getTime())
						.plusSeconds(timeWindow);
				do {
					final Utterance nextUtt = uttIter.next();
					final LocalDateTime uttStartTimestamp = TimestampArithmetic.createOffsetTimestamp(gameStartTime,
							nextUtt.getStartTime());
					// If the utterance was before the first event, add it
					// to the
					// list of before-event utterances
					if (firstEventTimestamp.isAfter(uttStartTimestamp)) {
						result.add(nextUtt);
					} else {
						break;
					}
				} while (uttIter.hasNext());
			}

		} else {
			// No utterances were found; Return an empty list of utterances
			result = Collections.emptyList();
		}
		return result;
	}

}
