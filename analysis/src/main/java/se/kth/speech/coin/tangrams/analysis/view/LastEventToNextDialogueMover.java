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
import java.time.temporal.TemporalAmount;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;

import javax.swing.JOptionPane;
import javax.swing.JTable;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.TimestampArithmetic;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;

final class LastEventToNextDialogueMover implements ActionListener {

	private static final Logger LOGGER = LoggerFactory.getLogger(LastEventToNextDialogueMover.class);

	private final JTable diagTable;

	private final Component dialogueMessageParentComponent;

	private final Map<EventDialogueAttribute, Integer> evtDiagAttrColIdxs;

	private final LocalDateTime gameStartTime;

	private final TemporalAmount minEventTimeDiff;

	private final Function<? super LocalDateTime, String> evtTimeFormatter;

	LastEventToNextDialogueMover(final JTable diagTable, final Map<EventDialogueAttribute, Integer> evtDiagAttrColIdxs,
			final Component dialogueMessageParentComponent, final LocalDateTime gameStartTime,
			final TemporalAmount minEventTimeDiff, final Function<? super LocalDateTime, String> evtTimeFormatter) {
		this.diagTable = diagTable;
		this.evtDiagAttrColIdxs = evtDiagAttrColIdxs;
		this.dialogueMessageParentComponent = dialogueMessageParentComponent;
		this.gameStartTime = gameStartTime;
		this.minEventTimeDiff = minEventTimeDiff;
		this.evtTimeFormatter = evtTimeFormatter;
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
			moveFirstEventToPreviousDialogue(rowIdx);
		}
	}

	private void moveFirstEventToPreviousDialogue(final int rowIdx) {
		final Optional<Utterance> optLastUtt = TableUtterances.findLastUtterance(diagTable, gameStartTime, rowIdx);
		if (optLastUtt.isPresent()) {
			final Utterance lastUtt = optLastUtt.get();
			final LocalDateTime minFollowingEvtDiagTime;
			{
				final LocalDateTime lastUttStartTime = TimestampArithmetic.createOffsetTimestamp(gameStartTime,
						lastUtt.getStartTime());
				minFollowingEvtDiagTime = lastUttStartTime.minus(minEventTimeDiff);
			}

			final int followingRowIdx = rowIdx + 1;
			final int firstEventColIdx = evtDiagAttrColIdxs.get(EventDialogueAttribute.FIRST_EVENT_TIME);
			try {
				final LocalDateTime followingRowFirstEventTime = (LocalDateTime) diagTable.getValueAt(followingRowIdx,
						firstEventColIdx);
				if (followingRowFirstEventTime == null) {
					JOptionPane.showMessageDialog(dialogueMessageParentComponent,
							"Cannot minimize event time of following row to last utterance end time because the following row has no event(s).",
							"No events", JOptionPane.ERROR_MESSAGE);
				} else {
					diagTable.setValueAt(minFollowingEvtDiagTime, followingRowIdx, firstEventColIdx);
					LOGGER.info("Set time of first event for row {} to \"{}\".", followingRowIdx,
							evtTimeFormatter.apply(minFollowingEvtDiagTime));
				}
			} catch (final ArrayIndexOutOfBoundsException ex) {
				JOptionPane.showMessageDialog(dialogueMessageParentComponent,
						String.format("No row for index %s.", ex.getLocalizedMessage()), ex.getClass().getSimpleName(),
						JOptionPane.ERROR_MESSAGE);
			}
		} else {
			JOptionPane.showMessageDialog(dialogueMessageParentComponent, "No utterances in the current row to move.",
					"No utterances", JOptionPane.ERROR_MESSAGE);
		}

	}

}