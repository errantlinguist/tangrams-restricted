/*
 *  This file is part of tangrams.
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
package se.kth.speech.coin.tangrams.view;

import java.util.Observable;
import java.util.Observer;

import javax.swing.JLabel;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.coin.tangrams.iristk.events.Turn;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 9 Jan 2017
 *
 */
final class MoveCounterLabel extends JLabel implements Observer {

	private static final Logger LOGGER = LoggerFactory.getLogger(MoveCounterLabel.class);

	/**
	 *
	 */
	private static final long serialVersionUID = -4867110204206142268L;

	private int currentTurnCount = -1;

	MoveCounterLabel(final int initialCount) {
		setAlignmentX(CENTER_ALIGNMENT);
		update(initialCount);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.Observer#update(java.util.Observable, java.lang.Object)
	 */
	@Override
	public void update(final Observable o, final Object arg) {
		if (arg instanceof Turn) {
			final Turn turn = (Turn) arg;
			final String turnPlayerId = turn.getPlayerId();
			LOGGER.debug("Observed event representing a turn completed by \"{}\".", turnPlayerId);
			final int newTurnCount = turn.getSequenceNumber();
			if (currentTurnCount != newTurnCount) {
				update(newTurnCount);
			}
		}
	}

	private void update(final int newTurnCount) {
		// FIXME: Indicator goes blank sometimes for some unknown reason
		setText(Integer.toString(newTurnCount));
		setToolTipText(String.format("%d move(s) taken in this game.", newTurnCount));
		currentTurnCount = newTurnCount;
		repaint();
	}

}
