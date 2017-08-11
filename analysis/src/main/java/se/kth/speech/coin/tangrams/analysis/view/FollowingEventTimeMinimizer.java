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
package se.kth.speech.coin.tangrams.analysis.view;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.Collections;
import java.util.Optional;
import java.util.stream.IntStream;

import javax.swing.JOptionPane;
import javax.swing.JTable;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import iristk.system.Event;
import se.kth.speech.TimestampArithmetic;
import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.iristk.EventTimes;

final class FollowingEventTimeMinimizer implements ActionListener {

	private static final Logger LOGGER = LoggerFactory.getLogger(FollowingEventTimeMinimizer.class);

	private final JTable diagTable;

	private final Component dialogueMessageParentComponent;

	private final int firstEvtTimeColIdx;

	private final LocalDateTime gameStartTime;

	FollowingEventTimeMinimizer(final JTable diagTable, final Component dialogueMessageParentComponent,
			final LocalDateTime gameStartTime) {
		this.diagTable = diagTable;
		this.dialogueMessageParentComponent = dialogueMessageParentComponent;
		this.gameStartTime = gameStartTime;

		firstEvtTimeColIdx = IntStream.range(0, diagTable.getColumnCount()).filter(colIdx -> {
			final String colName = diagTable.getColumnName(colIdx);
			return EventDialogueAttribute.FIRST_EVENT_TIME.toString().equals(colName);
		}).findAny().getAsInt();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
	 * ActionEvent)
	 */
	@Override
	public void actionPerformed(final ActionEvent event) {
		final int[] selectedRows = diagTable.getSelectedRows();
		for (final int rowIdx : selectedRows) {
			// NOTE: Even though the last event of the selected row is used for
			// comparison, the actual EventDialogue instannce backing both the
			// first and last event columns is the same
			final EventDialogue rowEventDiag = (EventDialogue) diagTable.getValueAt(rowIdx, firstEvtTimeColIdx);
			final Optional<LocalDateTime> optLatestEvtDiagTime = findLatestEventDialogueTime(rowEventDiag);
			if (optLatestEvtDiagTime.isPresent()) {
				final LocalDateTime lastestEvtDiagTime = optLatestEvtDiagTime.get();
				final int followingRowIdx = rowIdx + 1;
				try {
					final EventDialogue followingRowEventDiag = (EventDialogue) diagTable.getValueAt(followingRowIdx,
							firstEvtTimeColIdx);
					final Optional<Event> optFollowingRowFirstEvent = followingRowEventDiag.getFirstEvent();
					if (optFollowingRowFirstEvent.isPresent()) {
						final Event followingRowFirstEvent = optFollowingRowFirstEvent.get();
						final String followingRowEventTimeStr = followingRowFirstEvent.getTime();
						final LocalDateTime followingRowEventTime = EventTimes.parseEventTime(followingRowEventTimeStr);
						final int timeCmp = lastestEvtDiagTime.compareTo(followingRowEventTime);
						if (timeCmp < 0) {
							final String newEventTimeStr = EventTimes.FORMATTER.format(lastestEvtDiagTime);
							followingRowFirstEvent.setTime(newEventTimeStr);
							LOGGER.info("Set time of event ID \"{}\" to \"{}\".", followingRowFirstEvent.getId(),
									newEventTimeStr);
							diagTable.setValueAt(followingRowEventDiag, followingRowIdx, firstEvtTimeColIdx);
						} else if (timeCmp > 0) {
							JOptionPane.showMessageDialog(dialogueMessageParentComponent,
									String.format(
											"Latest time of preceding event (\"%s\") is after the time of the following event (\"%s\", ID \"%s\"), i.e. the utterance overlaps the start of the next event.",
											EventTimes.FORMATTER.format(lastestEvtDiagTime), followingRowEventTimeStr,
											followingRowFirstEvent.getId()),
									"Overlapping times", JOptionPane.WARNING_MESSAGE);
						} else {
							JOptionPane.showMessageDialog(dialogueMessageParentComponent,
									String.format(
											"Time of event ID \"%s\" is already equal to the end time of the preceding event's last utterance (\"%s\").",
											followingRowFirstEvent.getId(), followingRowEventTimeStr));
						}
					} else {
						JOptionPane.showMessageDialog(dialogueMessageParentComponent,
								"Following event dialogue does not have an event.", "No following event",
								JOptionPane.WARNING_MESSAGE);
					}

				} catch (final ArrayIndexOutOfBoundsException ex) {
					JOptionPane.showMessageDialog(dialogueMessageParentComponent,
							String.format("No row for index %s.", ex.getLocalizedMessage()),
							ex.getClass().getSimpleName(), JOptionPane.WARNING_MESSAGE);
				}
			} else {
				JOptionPane.showMessageDialog(dialogueMessageParentComponent,
						"Cannot minimize event time to last utterance end time because both the event and utterance lists are empty.",
						"No utterances", JOptionPane.WARNING_MESSAGE);
			}
		}
	}

	private Optional<LocalDateTime> findLatestEventDialogueTime(final EventDialogue eventDiag) {
		final Optional<LocalDateTime> result;

		final Optional<LocalDateTime> optLastUttEndTime = eventDiag.getLastUtterance()
				.map(utt -> TimestampArithmetic.createOffsetTimestamp(gameStartTime, utt.getEndTime()));
		final Optional<LocalDateTime> optLastEventTime = eventDiag.getLastEvent().map(Event::getTime)
				.map(EventTimes::parseEventTime);
		if (optLastUttEndTime.isPresent()) {
			final LocalDateTime lastUttEndTime = optLastUttEndTime.get();
			if (optLastEventTime.isPresent()) {
				final LocalDateTime lastEventTime = optLastEventTime.get();
				result = Optional.of(Collections.max(Arrays.asList(lastUttEndTime, lastEventTime)));
			} else {
				result = optLastUttEndTime;
			}
		} else {
			result = optLastEventTime;
		}

		return result;
	}

}