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
import java.util.Objects;
import java.util.Optional;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import javax.swing.JOptionPane;
import javax.swing.JTable;
import javax.swing.table.TableModel;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import iristk.system.Event;
import se.kth.speech.TimestampArithmetic;
import se.kth.speech.coin.tangrams.analysis.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.Utterance;
import se.kth.speech.coin.tangrams.iristk.EventTimes;

final class FollowingEventTimeMinimizer implements ActionListener {

	private static final Logger LOGGER = LoggerFactory.getLogger(FollowingEventTimeMinimizer.class);

	private final JTable diagTable;

	private final Component dialogueMessageParentComponent;

	private final LocalDateTime gameStartTime;

	FollowingEventTimeMinimizer(final JTable diagTable, final Component dialogueMessageParentComponent,
			final LocalDateTime gameStartTime) {
		this.diagTable = diagTable;
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
		final TableModel model = diagTable.getModel();
		// Find the index of any column with Event instances as cell
		// values
		final int eventAttrColIdx = IntStream.range(0, diagTable.getColumnCount())
				.filter(colIdx -> AttributeType.EVENT_DIALOGUE.isMatchingTypeColumn(model, colIdx)).findAny()
				.getAsInt();
		final int[] selectedRows = diagTable.getSelectedRows();
		for (final int rowIdx : selectedRows) {
			final IntStream uttColIdxs = AttributeType.UTTERANCE.getMatchingTypeColumnIndices(model);
			final Stream<Object> uttColVals = uttColIdxs.mapToObj(colIdx -> diagTable.getValueAt(rowIdx, colIdx));
			final Stream<Utterance> nonNullUtts = uttColVals.filter(Objects::nonNull).map(Utterance.class::cast);
			final Optional<Utterance> optLastUtt = nonNullUtts.reduce((first, second) -> second);
			if (optLastUtt.isPresent()) {
				final Utterance lastUtt = optLastUtt.get();
				final LocalDateTime lastUttEndTime = TimestampArithmetic.createOffsetTimestamp(gameStartTime,
						lastUtt.getEndTime());
				try {
					final EventDialogue followingRowEventDiag = (EventDialogue) diagTable.getValueAt(rowIdx + 1,
							eventAttrColIdx);
					final Optional<Event> optFollowingRowFirstEvent = followingRowEventDiag.getFirstEvent();
					if (optFollowingRowFirstEvent.isPresent()) {
						final Event followingRowFirstEvent = optFollowingRowFirstEvent.get();
						final String followingRowEventTimeStr = followingRowFirstEvent.getTime();
						final LocalDateTime followingRowEventTime = EventTimes.parseEventTime(followingRowEventTimeStr);
						final int timeCmp = lastUttEndTime.compareTo(followingRowEventTime);
						if (timeCmp < 0) {
							final String newEventTimeStr = EventTimes.FORMATTER.format(lastUttEndTime);
							followingRowFirstEvent.setTime(newEventTimeStr);
							LOGGER.info("Set time of event ID \"{}\" to \"{}\".", followingRowFirstEvent.getId(),
									newEventTimeStr);
							diagTable.repaint();
						} else if (timeCmp > 0) {
							JOptionPane.showMessageDialog(dialogueMessageParentComponent, String.format(
									"End time of preceding event's last utterance (\"%s\") is after the time of the following event (\"%s\", ID \"%s\"), i.e. the utterance overlaps the start of the next event.",
									EventTimes.FORMATTER.format(lastUttEndTime), followingRowEventTimeStr,
									followingRowFirstEvent.getId()), "Overlapping times", JOptionPane.WARNING_MESSAGE);
						} else {
							JOptionPane.showMessageDialog(dialogueMessageParentComponent, String.format(
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
						"Cannot minimize event time to last utterance end time because utterance list is empty.",
						"No utterances", JOptionPane.WARNING_MESSAGE);
			}
		}
	}

}