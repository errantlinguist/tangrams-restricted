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

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.function.Function;

import javax.swing.table.DefaultTableCellRenderer;

import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;

final class UtteranceCellRenderer extends DefaultTableCellRenderer {

	private static final int ESTIMATED_DIAGS_PER_SESSION = 96;

	private static final int ESTIMATED_UNIQUE_UTTS_PER_DIAG = 8;

	/**
	 *
	 */
	private static final long serialVersionUID = -259987246089416410L;

	private static final Function<Utterance, String> UTT_REPR_FACTORY = utt -> {
		final String tokenSeqRepr = utt.getTokenStr();
		return String.format("**%s:** %s", utt.getSpeakerId(), tokenSeqRepr);
	};

	private final ConcurrentMap<Utterance, String> uttReprs;

	UtteranceCellRenderer() {
		uttReprs = new ConcurrentHashMap<>(ESTIMATED_DIAGS_PER_SESSION * ESTIMATED_UNIQUE_UTTS_PER_DIAG);
	}

	@Override
	public void setValue(final Object value) {
		// http://docs.oracle.com/javase/tutorial/uiswing/components/table.html#renderer
		final String repr;
		if (value == null) {
			repr = "";
		} else {
			final Utterance utt = (Utterance) value;
			repr = uttReprs.computeIfAbsent(utt, UTT_REPR_FACTORY);
		}
		setText(repr);
	}

}