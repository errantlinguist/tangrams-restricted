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

import java.util.Arrays;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;

import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;

import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;

final class EventDialogueTableModel extends AbstractTableModel {

	private static final Map<EventDialogueAttribute, Integer> EVENT_DIALOGUE_ATTR_IDXS = createEventDialogueAttributeIndexMap();

	/**
	 *
	 */
	private static final long serialVersionUID = -4475137472007680337L;

	private static Map<EventDialogueAttribute, TableColumn> createEventDialogueAttributeColumnMap(
			final TableColumnModel colModel) {
		final Map<EventDialogueAttribute, TableColumn> result = new EnumMap<>(EventDialogueAttribute.class);
		final EventDialogueAttribute[] attrs = EventDialogueAttribute.values();
		for (final EventDialogueAttribute attr : EventDialogueAttribute.values()) {
			result.put(attr, getEventDialogueAttributeColumn(colModel, attr));
		}
		assert result.size() == attrs.length;
		return result;
	}

	private static Map<EventDialogueAttribute, Integer> createEventDialogueAttributeIndexMap() {
		final Map<EventDialogueAttribute, Integer> result = new EnumMap<>(EventDialogueAttribute.class);
		final EventDialogueAttribute[] attrs = EventDialogueAttribute.values();
		for (int i = 0; i < attrs.length; ++i) {
			final EventDialogueAttribute attr = attrs[i];
			result.put(attr, i);
		}
		assert result.size() == attrs.length;
		return result;
	}

	private static EventDialogueAttribute getColumnEventDialogueAttribute(final int colIdx) {
		final EventDialogueAttribute[] atts = EventDialogueAttribute.values();
		return atts.length <= colIdx ? null : atts[colIdx];
	}

	private static TableColumn getEventDialogueAttributeColumn(final TableColumnModel colModel,
			final EventDialogueAttribute attr) {
		final int colIdx = EVENT_DIALOGUE_ATTR_IDXS.get(attr);
		return colModel.getColumn(colIdx);
	}

	private final EventDialogue[] diags;

	EventDialogueTableModel(final EventDialogue[] diags) {
		this.diags = diags;
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
			result = AttributeType.EVENT_DIALOGUE.getValueClass();
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
		final int maxUttCount = Arrays.stream(diags).map(EventDialogue::getUtterances).mapToInt(List::size).max().orElse(0);
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
		return diags.length;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see javax.swing.table.TableModel#getValueAt(int, int)
	 */
	@Override
	public Object getValueAt(final int rowIndex, final int columnIndex) {
		final EventDialogue diag = diags[rowIndex];
		final Object result;
		final EventDialogueAttribute colEventDiagAttr = getColumnEventDialogueAttribute(columnIndex);
		if (colEventDiagAttr == null) {
			final List<Utterance> diagUtts = diag.getUtterances();
			final int diagUttIdx = AttributeType.UTTERANCE.getValueListIdx(columnIndex);
			result = diagUtts.size() <= diagUttIdx ? null : diagUtts.get(diagUttIdx);
		} else {
			result = diag;
		}
		return result;
	}

}