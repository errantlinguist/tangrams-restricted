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
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map.Entry;
import java.util.function.BiFunction;
import java.util.function.Predicate;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import iristk.system.Event;
import se.kth.speech.Iterators;
import se.kth.speech.TimestampArithmetic;
import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.coin.tangrams.iristk.EventTimes;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 4 May 2017
 *
 */
final class EventDialogueFactory // NO_UCD (use default)
		implements BiFunction<ListIterator<Utterance>, GameHistory, Stream<EventDialogue>> {

	private static final Logger LOGGER = LoggerFactory.getLogger(EventDialogueFactory.class);

	private static List<Utterance> createPreEventUtteranceList(final ListIterator<Utterance> utts,
			final Event nextEvent, final LocalDateTime gameStartTime) {
		LOGGER.debug("Event: {}", nextEvent);
		final LocalDateTime nextEventTimestamp = EventTimes.parseEventTime(nextEvent.getTime());
		return createPreEventUtteranceList(utts, nextEventTimestamp, gameStartTime);
	}

	private static List<Utterance> createPreEventUtteranceList(final ListIterator<Utterance> utts,
			final LocalDateTime nextEventTimestamp, final LocalDateTime gameStartTime) {
		final List<Utterance> result;
		if (utts.hasNext()) {
			result = new ArrayList<>();
			do {
				// Find all utterances up to the first event
				final Utterance nextUtt = utts.next();
				final LocalDateTime uttStartTimestamp = TimestampArithmetic.createOffsetTimestamp(gameStartTime,
						nextUtt.getStartTime());
				// If the utterance was before the first event, add it
				// to the
				// list of before-event utterances
				if (nextEventTimestamp.isAfter(uttStartTimestamp)) {
					result.add(nextUtt);
				} else {
					// Put the cursor position back to where it was so the
					// looked-at
					// event can be put into the list for the next event
					utts.previous();
					break;
				}
			} while (utts.hasNext());
		} else {
			result = Collections.emptyList();
		}
		return result;
	}

	private final Predicate<? super Event> dialogueEventDelimiter;

	EventDialogueFactory(final Predicate<? super Event> dialogueEventDelimiter) {
		this.dialogueEventDelimiter = dialogueEventDelimiter;
	}

	@Override
	public Stream<EventDialogue> apply(final ListIterator<Utterance> utts, final GameHistory history) {
		final Stream<Event> events = history.getEventSequence();
		final Iterator<Event> eventIter = events.iterator();
		// Find the set of elements before the first one delimiting a dialogue
		final Entry<Stream<Event>, Event> preDialogueEvents = Iterators.findElementsBeforeDelimiter(eventIter,
				dialogueEventDelimiter);
		Event currentEvent = preDialogueEvents.getValue();

		final Stream.Builder<EventDialogue> resultBuilder = Stream.builder();

		final LocalDateTime gameStartTime = history.getStartTime();
		final List<Utterance> firstUtts = createPreEventUtteranceList(utts, currentEvent, gameStartTime);
		if (firstUtts.isEmpty()) {
			LOGGER.debug("No utterances before first event.");
		} else {
			resultBuilder.accept(
					new EventDialogue(Arrays.asList(preDialogueEvents.getKey().toArray(Event[]::new)), firstUtts));
		}

		while (eventIter.hasNext()) {
			// Find the next set of events which delimit one dialogue
			final Entry<Stream<Event>, Event> nextDialogueEvents = Iterators.findElementsBeforeDelimiter(eventIter,
					dialogueEventDelimiter);
			final Event nextEvent = nextDialogueEvents.getValue();
			LOGGER.debug("Next event is named \"{}\".", nextEvent.getName());
			// Find the set of utterances following the last event
			final List<Utterance> nextUttList = createPreEventUtteranceList(utts, nextEvent, gameStartTime);
			final List<Event> nextDiagEvents = Arrays
					.asList(Stream.concat(Stream.of(currentEvent), nextDialogueEvents.getKey()).toArray(Event[]::new));
			LOGGER.debug("New {} has {} event(s).", EventDialogue.class.getSimpleName(), nextDiagEvents.size());
			resultBuilder.accept(new EventDialogue(nextDiagEvents, nextUttList));
			currentEvent = nextEvent;
		}

		// Get the utterances after the last event
		final List<Utterance> lastEventUtts = new ArrayList<>();
		while (utts.hasNext()) {
			lastEventUtts.add(utts.next());
		}
		LOGGER.debug("Last event is named \"{}\".", currentEvent.getName());
		resultBuilder.accept(new EventDialogue(Collections.singletonList(currentEvent), lastEventUtts));
		return resultBuilder.build();
	}

}
