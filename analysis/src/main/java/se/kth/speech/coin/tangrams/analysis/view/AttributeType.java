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

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.stream.IntStream;

import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableModel;

import se.kth.speech.coin.tangrams.analysis.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.Utterance;

enum AttributeType {
	EVENT_DIALOGUE(EventDialogue.class) {
		@Override
		protected int getValueListIdx(final int columnIndex) {
			return columnIndex;
		}

		@Override
		protected int getValueListSize(final TableColumnModel colModel) {
			return EVENT_DIALOGUE_ATTR_COUNT;
		}
	},
	UTTERANCE(Utterance.class) {
		@Override
		protected int getValueListIdx(final int columnIndex) {
			return columnIndex - EVENT_DIALOGUE_ATTR_COUNT;
		}

		@Override
		protected int getValueListSize(final TableColumnModel colModel) {
			return colModel.getColumnCount() - EVENT_DIALOGUE.getValueListSize(colModel);
		}
	};

	private static final int EVENT_DIALOGUE_ATTR_COUNT = EventDialogueAttribute.values().length;

	final Class<?> valueClass;

	private AttributeType(final Class<?> valueClass) {
		this.valueClass = valueClass;
	}

	private List<TableColumn> createColumnList(final TableModel model, final TableColumnModel colModel) {
		final int resultSize = getValueListSize(colModel);
		final List<TableColumn> result = new ArrayList<>(resultSize);
		final Enumeration<TableColumn> cols = colModel.getColumns();

		int colIdx = 0;
		while (result.size() < resultSize && cols.hasMoreElements()) {
			final TableColumn col = cols.nextElement();
			if (isMatchingTypeColumn(model, colIdx)) {
				result.add(col);
			}
			colIdx++;
		}
		return result;
	}

	protected IntStream getMatchingTypeColumnIndices(final TableModel model) {
		return IntStream.range(0, model.getColumnCount()).filter(colIdx -> isMatchingTypeColumn(model, colIdx));
	}

	protected abstract int getValueListIdx(final int columnIndex);

	protected abstract int getValueListSize(final TableColumnModel colModel);

	protected boolean isMatchingTypeColumn(final TableModel model, final int colIdx) {
		final Class<?> colClass = model.getColumnClass(colIdx);
		return valueClass.isAssignableFrom(colClass);
	}
}