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
import java.util.Iterator;
import java.util.List;
import java.util.Map.Entry;
import java.util.Queue;
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
		implements BiFunction<Queue<Utterance>, GameHistory, Stream<Entry<Event, List<Utterance>>>> {

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
	public Stream<Entry<Event, List<Utterance>>> apply(final Queue<Utterance> utts, final GameHistory history) {
		final Stream<Event> events = history.getEvents().values().stream().flatMap(List::stream).filter(eventFilter);
		final LocalDateTime gameStartTime = history.getStartTime();

		final Iterator<Event> eventIter = events.iterator();
		Event currentEvent = eventIter.next();
		final Stream.Builder<Entry<Event, List<Utterance>>> resultBuilder = Stream.builder();

		final List<Utterance> firstUtts = createPreEventUtteranceList(utts, currentEvent, gameStartTime);
		if (firstUtts.isEmpty()) {
			LOGGER.debug("No utterances before first event.");
		} else {
			resultBuilder.accept(new MutablePair<>(null, firstUtts));
		}

		// Find the next set of utterances following each event
		while (eventIter.hasNext()) {
			final Event nextEvent = eventIter.next();
			final List<Utterance> nextUttList = createPreEventUtteranceList(utts, nextEvent, gameStartTime);
			resultBuilder.accept(new MutablePair<>(currentEvent, nextUttList));
			currentEvent = nextEvent;
		}

		// Get the utterances after the last event
		final List<Utterance> lastEventUtts = new ArrayList<>(utts);
		resultBuilder.accept(new MutablePair<>(currentEvent, lastEventUtts));
		return resultBuilder.build();
	}

	private List<Utterance> createPreEventUtteranceList(final Queue<Utterance> utts, final Event nextEvent,
			final LocalDateTime gameStartTime) {
		LOGGER.debug("Event: {}", nextEvent);
		final LocalDateTime nextEventTimestamp = EventTimes.parseEventTime(nextEvent.getTime()).plusSeconds(timeWindow);

		final List<Utterance> result = new ArrayList<>();
		while (!utts.isEmpty()) {
			// Find all utterances up to the first event
			final Utterance nextUtt = utts.peek();
			final LocalDateTime uttStartTimestamp = TimestampArithmetic.createOffsetTimestamp(gameStartTime,
					nextUtt.getStartTime());
			// If the utterance was before the first event, add it
			// to the
			// list of before-event utterances
			if (nextEventTimestamp.isAfter(uttStartTimestamp)) {
				result.add(nextUtt);
				utts.remove();
			} else {
				break;
			}
		}
		return result;
	}

}
