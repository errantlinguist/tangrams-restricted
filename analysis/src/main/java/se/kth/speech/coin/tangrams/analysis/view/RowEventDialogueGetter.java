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
package se.kth.speech.coin.tangrams.analysis.view;

import java.util.function.IntFunction;
import java.util.stream.IntStream;

import javax.swing.JTable;

import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;

final class RowEventDialogueGetter implements IntFunction<EventDialogue> {

	private final JTable diagTable;

	private final int eventDiagAttrColIdx;

	private RowEventDialogueGetter(final JTable diagTable, final int eventDiagAttrColIdx) {
		this.diagTable = diagTable;
		this.eventDiagAttrColIdx = eventDiagAttrColIdx;
	}

	RowEventDialogueGetter(final JTable diagTable) {
		// Find the index of any column with EventDialogue instances as cell values
		this(diagTable, IntStream.range(0, diagTable.getColumnCount())
				.filter(colIdx -> AttributeType.EVENT_DIALOGUE.isMatchingTypeColumn(diagTable.getModel(), colIdx))
				.findAny().getAsInt());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.util.function.IntFunction#apply(int)
	 */
	@Override
	public EventDialogue apply(final int rowIdx) {
		return (EventDialogue) diagTable.getValueAt(rowIdx, eventDiagAttrColIdx);
	}

}