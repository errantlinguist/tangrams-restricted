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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Stream;

import javax.swing.JOptionPane;
import javax.swing.JTable;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.TimestampArithmetic;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.coin.tangrams.iristk.EventTimes;

final class FollowingEventTimeMinimizer implements ActionListener {

	private static final Logger LOGGER = LoggerFactory.getLogger(FollowingEventTimeMinimizer.class);

	private final JTable diagTable;

	private final Component dialogueMessageParentComponent;

	private final Map<EventDialogueAttribute, Integer> evtDiagAttrColIdxs;

	private final LocalDateTime gameStartTime;

	FollowingEventTimeMinimizer(final JTable diagTable, final Map<EventDialogueAttribute, Integer> evtDiagAttrColIdxs,
			final Component dialogueMessageParentComponent, final LocalDateTime gameStartTime) {
		this.diagTable = diagTable;
		this.evtDiagAttrColIdxs = evtDiagAttrColIdxs;
		this.dialogueMessageParentComponent = dialogueMessageParentComponent;
		this.gameStartTime = gameStartTime;
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
			minimizeFollowingEventTime(rowIdx);
		}
	}

	private List<Utterance> createUtteranceList(final int rowIdx) {
		final List<Utterance> result = new ArrayList<>(
				AttributeType.UTTERANCE.getValueListSize(diagTable.getColumnModel()));
		for (int colIdx = 0; colIdx < diagTable.getColumnCount(); ++colIdx) {
			final Class<?> colClass = diagTable.getColumnClass(colIdx);
			if (Utterance.class.isAssignableFrom(colClass)) {
				final Object cellValue = diagTable.getValueAt(rowIdx, colIdx);
				if (cellValue != null) {
					final Utterance utt = (Utterance) cellValue;
					result.add(utt);
				}
			}
		}
		return result;
	}

	private Optional<LocalDateTime> findLastUtteranceTime(final int rowIdx) {
		final List<Utterance> utts = createUtteranceList(rowIdx);
		final Stream<LocalDateTime> uttEndTimes = utts.stream()
				.map(utt -> TimestampArithmetic.createOffsetTimestamp(gameStartTime, utt.getEndTime()));
		return uttEndTimes.max(Comparator.naturalOrder());
	}

	private Optional<LocalDateTime> findLatestEventDialogueTime(final int rowIdx) {
		final Optional<LocalDateTime> result;
		// NOTE: Even though the last event of the selected row is used for
		// comparison, the actual EventDialogue instance backing both the
		// first and last event columns is the same
		final LocalDateTime rowLastEventTime = (LocalDateTime) diagTable.getValueAt(rowIdx,
				evtDiagAttrColIdxs.get(EventDialogueAttribute.LAST_EVENT_TIME));
		final Optional<LocalDateTime> optLastUttTime = findLastUtteranceTime(rowIdx);
		if (rowLastEventTime == null) {
			result = optLastUttTime;
		} else if (optLastUttTime.isPresent()) {
			final LocalDateTime lastUttTIme = optLastUttTime.get();
			result = Optional.of(Collections.max(Arrays.asList(rowLastEventTime, lastUttTIme)));
		} else {
			result = Optional.of(rowLastEventTime);
		}

		return result;
	}

	private void minimizeFollowingEventTime(final int rowIdx) {
		final Optional<LocalDateTime> optLatestEvtDiagTime = findLatestEventDialogueTime(rowIdx);
		if (optLatestEvtDiagTime.isPresent()) {
			final LocalDateTime lastestEvtDiagTime = optLatestEvtDiagTime.get();
			final int followingRowIdx = rowIdx + 1;
			try {
				final int firstEventColIdx = evtDiagAttrColIdxs.get(EventDialogueAttribute.FIRST_EVENT_TIME);
				final LocalDateTime followingRowFirstEventTime = (LocalDateTime) diagTable.getValueAt(followingRowIdx,
						firstEventColIdx);
				if (followingRowFirstEventTime == null) {
					JOptionPane.showMessageDialog(dialogueMessageParentComponent,
							"Cannot minimize event time of following row to last utterance end time because the following row has no event(s).",
							"No events", JOptionPane.WARNING_MESSAGE);
				} else {
					final int timeCmp = lastestEvtDiagTime.compareTo(followingRowFirstEventTime);
					if (timeCmp < 0) {
						final String newEventTimeStr = EventTimes.FORMATTER.format(lastestEvtDiagTime);
						// Explicitly call method to fire a model update
						// event
						diagTable.setValueAt(lastestEvtDiagTime, followingRowIdx, firstEventColIdx);
						LOGGER.info("Set time of first event for row {} to \"{}\".", followingRowIdx, newEventTimeStr);
					} else if (timeCmp > 0) {
						JOptionPane.showMessageDialog(dialogueMessageParentComponent,
								String.format(
										"Latest time of preceding event (\"%s\") is after the time of the following event (\"%s\"), i.e. the utterance overlaps the start of the next event.",
										EventTimes.FORMATTER.format(lastestEvtDiagTime),
										EventTimes.FORMATTER.format(followingRowFirstEventTime)),
								"Overlapping times", JOptionPane.WARNING_MESSAGE);
					} else {
						JOptionPane.showMessageDialog(dialogueMessageParentComponent,
								String.format(
										"Time of following event is already equal to the end time of the preceding event's last utterance (\"%s\").",
										EventTimes.FORMATTER.format(followingRowFirstEventTime)));
					}
				}

			} catch (final ArrayIndexOutOfBoundsException ex) {
				JOptionPane.showMessageDialog(dialogueMessageParentComponent,
						String.format("No row for index %s.", ex.getLocalizedMessage()), ex.getClass().getSimpleName(),
						JOptionPane.WARNING_MESSAGE);
			}
		} else {
			JOptionPane.showMessageDialog(dialogueMessageParentComponent,
					"Cannot minimize event time of the following row to last utterance end time because both the event and utterance lists are empty.",
					"No utterances", JOptionPane.WARNING_MESSAGE);
		}
	}

}