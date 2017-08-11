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

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

import javax.swing.JTable;

import se.kth.speech.TimestampArithmetic;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 11 Aug 2017
 *
 */
final class TableUtterances {

	static List<Utterance> createUtteranceList(final JTable diagTable, final int rowIdx) {
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

	static Optional<LocalDateTime> findLastUtteranceTime(final JTable diagTable, final LocalDateTime gameStartTime,
			final int rowIdx) {
		final List<Utterance> utts = createUtteranceList(diagTable, rowIdx);
		final Stream<LocalDateTime> uttEndTimes = utts.stream()
				.map(utt -> TimestampArithmetic.createOffsetTimestamp(gameStartTime, utt.getEndTime()));
		return uttEndTimes.max(Comparator.naturalOrder());
	}

	private TableUtterances() {
	}

}
