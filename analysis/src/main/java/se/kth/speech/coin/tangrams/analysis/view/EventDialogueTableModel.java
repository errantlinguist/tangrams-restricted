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

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;

import javax.swing.table.AbstractTableModel;

import iristk.system.Event;
import se.kth.speech.coin.tangrams.analysis.SessionGame;
import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;

final class EventDialogueTableModel extends AbstractTableModel {

	/**
	 *
	 */
	private static final long serialVersionUID = -4475137472007680337L;

	private static EventDialogueAttribute getColumnEventDialogueAttribute(final int colIdx) {
		final EventDialogueAttribute[] atts = EventDialogueAttribute.values();
		return atts.length <= colIdx ? null : atts[colIdx];
	}

	private final List<EventDialogue> diags;

	private final List<Event> events;

	private final Function<? super LocalDateTime, String> timeFormatter;

	private final Function<? super String, LocalDateTime> timeParser;

	private final List<Utterance> utts;

	EventDialogueTableModel(final Function<? super String, LocalDateTime> timeParser,
			final Function<? super LocalDateTime, String> timeFormatter, final SessionGame game) {
		this.timeParser = timeParser;
		this.timeFormatter = timeFormatter;
		events = game.getEvents();
		utts = game.getUtterances();
		diags = game.getEventDialogues();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see javax.swing.table.AbstractTableModel#getColumnClass(int)
	 */
	@Override
	public Class<?> getColumnClass(final int columnIndex) {
		final Class<?> result;
		final EventDialogueAttribute colEventDiagAttr = getColumnEventDialogueAttribute(columnIndex);
		if (colEventDiagAttr == null) {
			result = AttributeType.UTTERANCE.getValueClass();
		} else {
			switch (colEventDiagAttr) {
			case FIRST_EVENT_SENDER:
				result = String.class;
				break;
			case FIRST_EVENT_TIME:
				result = LocalDateTime.class;
				break;
			case LAST_EVENT_TIME:
				result = LocalDateTime.class;
				break;
			default:
				throw new AssertionError("No logic for handing switch case.");
			}
		}
		return result;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see javax.swing.table.TableModel#getColumnCount()
	 */
	@Override
	public int getColumnCount() {
		final int maxUttCount = diags.stream().map(EventDialogue::getUtterances).mapToInt(List::size).max().orElse(0);
		return maxUttCount + 1;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see javax.swing.table.AbstractTableModel#getColumnName(int)
	 */
	@Override
	public String getColumnName(final int column) {
		final String result;
		final EventDialogueAttribute colEventDiagAttr = getColumnEventDialogueAttribute(column);
		if (colEventDiagAttr == null) {
			final int uttIdx = AttributeType.UTTERANCE.getValueListIdx(column);
			result = Utterance.class.getSimpleName() + "_" + uttIdx;
		} else {
			result = colEventDiagAttr.toString();
		}
		return result;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see javax.swing.table.TableModel#getRowCount()
	 */
	@Override
	public int getRowCount() {
		return diags.size();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see javax.swing.table.TableModel#getValueAt(int, int)
	 */
	@Override
	public Object getValueAt(final int rowIndex, final int columnIndex) {
		final EventDialogue diag = diags.get(rowIndex);
		final Object result;
		final EventDialogueAttribute colEventDiagAttr = getColumnEventDialogueAttribute(columnIndex);
		if (colEventDiagAttr == null) {
			final List<Utterance> diagUtts = diag.getUtterances();
			final int diagUttIdx = AttributeType.UTTERANCE.getValueListIdx(columnIndex);
			result = diagUtts.size() <= diagUttIdx ? null : diagUtts.get(diagUttIdx);
		} else {
			switch (colEventDiagAttr) {
			case FIRST_EVENT_SENDER: {
				final Optional<Event> optEvent = diag.getFirstEvent();
				result = optEvent.map(event -> event.get(GameManagementEvent.Attribute.PLAYER_ID.toString()))
						.orElse(null);
				break;
			}
			case FIRST_EVENT_TIME: {
				final Optional<Event> optEvent = diag.getFirstEvent();
				result = optEvent.map(Event::getTime).map(timeParser).orElse(null);
				break;
			}
			case LAST_EVENT_TIME: {
				final Optional<Event> optEvent = diag.getLastEvent();
				result = optEvent.map(Event::getTime).map(timeParser).orElse(null);
				break;
			}
			default:
				throw new AssertionError("No logic for handing switch case.");
			}
		}
		return result;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see javax.swing.table.AbstractTableModel#setValueAt(java.lang.Object,
	 * int, int)
	 */
	@Override
	public void setValueAt(final Object aValue, final int rowIndex, final int columnIndex) {
		final EventDialogue diag = diags.get(rowIndex);
		final EventDialogueAttribute colEventDiagAttr = getColumnEventDialogueAttribute(columnIndex);
		if (colEventDiagAttr == null) {
			final List<Utterance> diagUtts = diag.getUtterances();
			final int diagUttIdx = AttributeType.UTTERANCE.getValueListIdx(columnIndex);
			// result = diagUtts.size() <= diagUttIdx ? null :
			// diagUtts.get(diagUttIdx);
		} else {
			switch (colEventDiagAttr) {
			case FIRST_EVENT_SENDER: {
				final Optional<Event> optEvent = diag.getFirstEvent();
				if (optEvent.isPresent()) {
					final Event event = optEvent.get();
					event.put(GameManagementEvent.Attribute.PLAYER_ID.toString(), aValue);
				} else {
					throw new IllegalArgumentException(String.format("No value at %d*%d.", rowIndex, columnIndex));
				}
				break;
			}
			case FIRST_EVENT_TIME: {
				final Optional<Event> optEvent = diag.getFirstEvent();
				if (optEvent.isPresent()) {
					final Event event = optEvent.get();
					final LocalDateTime time = (LocalDateTime) aValue;
					event.setTime(timeFormatter.apply(time));
				} else {
					throw new IllegalArgumentException(String.format("No value at %d*%d.", rowIndex, columnIndex));
				}
				break;
			}
			case LAST_EVENT_TIME: {
				final Optional<Event> optEvent = diag.getLastEvent();
				if (optEvent.isPresent()) {
					final Event event = optEvent.get();
					final LocalDateTime time = (LocalDateTime) aValue;
					event.setTime(timeFormatter.apply(time));
				} else {
					throw new IllegalArgumentException(String.format("No value at %d*%d.", rowIndex, columnIndex));
				}
				break;
			}
			default:
				throw new AssertionError("No logic for handing switch case.");
			}
		}

		fireTableCellUpdated(rowIndex, columnIndex);
	}

}