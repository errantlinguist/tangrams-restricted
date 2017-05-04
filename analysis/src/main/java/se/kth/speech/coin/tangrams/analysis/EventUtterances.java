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
import java.util.Spliterators;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.function.Predicate;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Lists;

import iristk.system.Event;
import se.kth.speech.MutablePair;
import se.kth.speech.TimestampArithmetic;
import se.kth.speech.coin.tangrams.iristk.EventTimes;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 4 May 2017
 *
 */
public final class EventUtterances {
	
	private static final Logger LOGGER = LoggerFactory.getLogger(EventUtterances.class);

	public static Stream<Entry<Event, List<Utterance>>> createEventUtteranceMappings(final List<Utterance> utts,
			final GameHistory history) {
		return createEventUtteranceMappings(utts, history, event -> true);
	}
	
	
	private static Entry<Event,List<Utterance>> createPreEventUtteranceList(final Iterator<Utterance> uttIter, final Iterator<Event> eventIter,
			final LocalDateTime gameStartTime, final Predicate<? super Event> eventFilter){
		final Entry<Event,List<Utterance>> result;

		if (eventIter.hasNext()) {
			if (uttIter.hasNext()) {
				List<Utterance> nextUttList = new ArrayList<>();
				{
					// Find all utterances up to the first event
					final Event firstEvent = eventIter.next();
					LOGGER.info("First event: {}", firstEvent);
					final LocalDateTime firstEventTimestamp = EventTimes.parseEventTime(firstEvent.getTime());
					do {
						final Utterance nextUtt = uttIter.next();
						final LocalDateTime uttStartTimestamp = TimestampArithmetic.createOffsetTimestamp(gameStartTime,
								nextUtt.getStartTime());
						// If the utterance was before the first event, add it
						// to the
						// list of before-event utterances
						if (uttStartTimestamp.compareTo(firstEventTimestamp) < 0) {
							nextUttList.add(nextUtt);
						} else {
							break;
						}
					} while (uttIter.hasNext());
					result = new MutablePair<>(firstEvent, nextUttList);
				}

			} else {
				// No utterances were found; Return an empty list of utterances
				result = new MutablePair<>(null, Collections.emptyList());
			}

		} else {
			// No events were found; Return all the utterances
			result =  new MutablePair<>(null, Collections.unmodifiableList(Lists.newArrayList(uttIter)));
		}
		return result;		
	}

	public static Stream<Entry<Event, List<Utterance>>> createEventUtteranceMappings(final List<Utterance> utts,
			final GameHistory history, final Predicate<? super Event> eventFilter) {
		final Stream<Event> events = history.getEvents().values().stream().flatMap(List::stream).filter(eventFilter);
		final LocalDateTime gameStartTime = history.getStartTime();

		final Stream<Entry<Event, List<Utterance>>> result;
		
		
		// TODO: Finish
		
		final Iterator<Event> eventIter = events.iterator();
		if (eventIter.hasNext()) {
			final Iterator<Utterance> uttIter = utts.iterator();
			if (uttIter.hasNext()) {
				final Stream.Builder<Entry<Event, List<Utterance>>> resultBuilder = Stream.builder();
				Event currentEvent = null;
				List<Utterance> nextUttList = new ArrayList<>();
				{
					// Find all utterances up to the first event
					final Event firstEvent = eventIter.next();
					LOGGER.info("First event: {}", firstEvent);
					final LocalDateTime firstEventTimestamp = EventTimes.parseEventTime(firstEvent.getTime());
					do {
						final Utterance nextUtt = uttIter.next();
						final LocalDateTime uttStartTimestamp = TimestampArithmetic.createOffsetTimestamp(gameStartTime,
								nextUtt.getStartTime());
						// If the utterance was before the first event, add it
						// to the
						// list of before-event utterances
						if (uttStartTimestamp.compareTo(firstEventTimestamp) < 0) {
							nextUttList.add(nextUtt);
						} else {
							break;
						}
					} while (uttIter.hasNext());
					// Add the first list only if any utterances preceding the
					// first event were found
					if (!nextUttList.isEmpty()) {
						resultBuilder.accept(new MutablePair<>(null, nextUttList));
						nextUttList = new ArrayList<>();
					}

					currentEvent = firstEvent;
				}
				{
					// Find the next set of utterances following each event
					while (eventIter.hasNext()) {
						final Event nextEvent = eventIter.next();
						LOGGER.info("Next event: {}", nextEvent);
						final LocalDateTime nextEventTimestamp = EventTimes.parseEventTime(nextEvent.getTime());
						eventUtts: while (uttIter.hasNext()) {
							final Utterance nextUtt = uttIter.next();
							final float uttStartMills = nextUtt.getStartTime() * SegmentTimes.TIME_TO_MILLS_FACTOR;
							final LocalDateTime uttStartTimestamp = TimestampArithmetic
									.createOffsetTimestamp(gameStartTime, uttStartMills);
							// If the utterance was before the next event, add
							// it to the
							// list of utterances for the current event
							if (uttStartTimestamp.isBefore(nextEventTimestamp)) {
								nextUttList.add(nextUtt);
							} else {
								resultBuilder.accept(new MutablePair<>(currentEvent, nextUttList));
								nextUttList = new ArrayList<>();
								nextUttList.add(nextUtt);
								currentEvent = nextEvent;
								break eventUtts;
							}
						}
					}
				}

				result = resultBuilder.build();

			} else {
				// No utterances were found; Return an empty list of utterances
				// for each event
				final List<Utterance> nullList = Collections.emptyList();
				result = events.map(event -> new MutablePair<>(event, nullList));
			}

		} else {
			// No events were found; Return all the utterances
			result = Stream.of(new MutablePair<>(null, Collections.unmodifiableList(utts)));
		}
		return result;
	}

	private EventUtterances() {
	}

}
