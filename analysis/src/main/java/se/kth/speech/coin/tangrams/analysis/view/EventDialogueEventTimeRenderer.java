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

import java.util.Optional;
import java.util.function.Function;

import javax.swing.table.DefaultTableCellRenderer;

import iristk.system.Event;
import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;

final class EventDialogueEventTimeRenderer extends DefaultTableCellRenderer {

	/**
	 *
	 */
	private static final long serialVersionUID = 1591902695449966670L;

	private final Function<? super EventDialogue, Optional<Event>> eventGetter;

	private final String nullValueRepr;

	EventDialogueEventTimeRenderer(final Function<? super EventDialogue, Optional<Event>> eventGetter, String nullValueRepr) {
		this.eventGetter = eventGetter;
		this.nullValueRepr = nullValueRepr;
	}

	@Override
	public void setValue(final Object value) {
		// http://docs.oracle.com/javase/tutorial/uiswing/components/table.html#renderer
		final String repr;
		if (value == null) {
			repr = nullValueRepr;
		} else {
			final EventDialogue eventDiag = (EventDialogue) value;
			final Optional<Event> optEvent = eventGetter.apply(eventDiag);
			repr = optEvent.map(Event::getTime).orElse(nullValueRepr);
		}
		setText(repr);
	}
}