/*
 *  This file is part of Tangrams-restricted.
 *
 *  Tangrams-restricted is free software: you can redistribute it and/or modify
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
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Predicate;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.Iterators;
import se.kth.speech.TimestampArithmetic;
import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.coin.tangrams.iristk.GameEvent;

/**
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since 4 May 2017
 *
 */
final class EventDialogueFactory // NO_UCD (use default)
		implements BiFunction<ListIterator<Utterance>, GameHistory, Stream<EventDialogue>> {

	private static final List<GameEvent> EMPTY_EVENT_LIST = Collections.emptyList();

	private static final Logger LOGGER = LoggerFactory.getLogger(EventDialogueFactory.class);

	private static List<Utterance> createPreEventUtteranceList(final ListIterator<Utterance> utts,
			final GameEvent nextEvent, final LocalDateTime gameStartTime) {
		LOGGER.debug("Event: {}", nextEvent);
		final LocalDateTime nextEventTimestamp = nextEvent.getTime();
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

	private final Predicate<? super GameEvent> dialogueEventDelimiter;

	EventDialogueFactory(final Predicate<? super GameEvent> dialogueEventDelimiter) {
		this.dialogueEventDelimiter = dialogueEventDelimiter;
	}

	@Override
	public Stream<EventDialogue> apply(final ListIterator<Utterance> utts, final GameHistory history) {
		final Stream<GameEvent> events = history.getEventSequence();
		final Iterator<GameEvent> eventIter = events.iterator();
		// Find the set of elements before the first one delimiting a dialogue
		final Entry<Stream<GameEvent>, Optional<GameEvent>> preDialogueEvents = Iterators
				.findElementsBeforeDelimiter(eventIter, dialogueEventDelimiter);

		final Stream.Builder<EventDialogue> resultBuilder = Stream.builder();
		final Optional<GameEvent> optInitialEvent = preDialogueEvents.getValue();
		if (optInitialEvent.isPresent()) {
			GameEvent currentEvent = optInitialEvent.get();
			final LocalDateTime gameStartTime = history.getStartTime();
			final List<Utterance> firstUtts = createPreEventUtteranceList(utts, currentEvent, gameStartTime);
			if (firstUtts.isEmpty()) {
				LOGGER.debug("No utterances before first event.");
			} else {
				resultBuilder.accept(new EventDialogue(
						Arrays.asList(preDialogueEvents.getKey().toArray(GameEvent[]::new)), firstUtts));
			}

			while (eventIter.hasNext()) {
				// Find the next set of events which delimit one dialogue
				final Entry<Stream<GameEvent>, Optional<GameEvent>> nextDialogueEvents = Iterators
						.findElementsBeforeDelimiter(eventIter, dialogueEventDelimiter);
				// NOTE: This list must be created before "currentEvent" is
				// reassigned
				assert currentEvent != null;
				final List<GameEvent> currentDialogueEvents = Arrays.asList(
						Stream.concat(Stream.of(currentEvent), nextDialogueEvents.getKey()).toArray(GameEvent[]::new));

				final List<Utterance> currentDialogueUttList;
				final Optional<GameEvent> optNextDialogueDelimitingEvent = nextDialogueEvents.getValue();
				if (optNextDialogueDelimitingEvent.isPresent()) {
					final GameEvent nextDialogueDelimitingEvent = optNextDialogueDelimitingEvent.get();
					LOGGER.debug("Next event is named \"{}\".", nextDialogueDelimitingEvent.getName());
					// Find the set of utterances preceding the delimiter for
					// this dialogue, i.e. the first event for the next dialogue
					currentDialogueUttList = createPreEventUtteranceList(utts, nextDialogueDelimitingEvent,
							gameStartTime);
					currentEvent = nextDialogueDelimitingEvent;
				} else {
					// Get the remaining utterances
					currentDialogueUttList = Iterators.createRemainingElementList(utts);
					currentEvent = null;
				}
				LOGGER.debug("New {} has {} event(s).", EventDialogue.class.getSimpleName(),
						currentDialogueEvents.size());
				assert !currentDialogueEvents.isEmpty();
				resultBuilder.accept(new EventDialogue(currentDialogueEvents, currentDialogueUttList));
			}
			// Get the utterances after the last event
			final List<Utterance> lastEventUtts = Iterators.createRemainingElementList(utts);
			final List<GameEvent> lastDiagEvents;
			if (currentEvent == null) {
				LOGGER.debug("No trailing event.");
				lastDiagEvents = EMPTY_EVENT_LIST;
			} else {
				LOGGER.debug("Last event is named \"{}\".", currentEvent.getName());
				lastDiagEvents = Collections.singletonList(currentEvent);
			}
			if (!(lastEventUtts.isEmpty() && lastDiagEvents.isEmpty())) {
				resultBuilder.accept(new EventDialogue(lastDiagEvents, lastEventUtts));
			}
		} else {
			LOGGER.warn("No delimiting events found; Creating a single {} instance.",
					EventDialogue.class.getSimpleName());
			final List<Utterance> allRemainingUtts = Iterators.createRemainingElementList(utts);
			resultBuilder.accept(new EventDialogue(EMPTY_EVENT_LIST, allRemainingUtts));
		}

		return resultBuilder.build();
	}

}
