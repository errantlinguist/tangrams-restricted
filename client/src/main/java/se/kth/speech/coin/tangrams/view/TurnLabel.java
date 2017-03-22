/*
 *  This file is part of se.kth.speech.coin.tangrams.client.
 *
 *  se.kth.speech.coin.tangrams.client is free software: you can redistribute it and/or modify
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

import java.awt.Color;

import javax.swing.JLabel;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 1 Feb 2017
 *
 */
public final class TurnLabel extends JLabel {

	/**
	 *
	 */
	private static final long serialVersionUID = 5446856514315600216L;

	private PlayerTurnStatus currentStatus;

	public TurnLabel(final PlayerTurnStatus initialStatus) {
		currentStatus = initialStatus;
		updateStatus(initialStatus);
	}

	private void updateStatus(final PlayerTurnStatus newStatus) {
		switch (newStatus) {
		case NOT_READY:
			setText("Not your turn");
			setForeground(Color.BLACK);
			break;
		case READY:
			setText("Your turn!");
			setForeground(Color.RED);
			break;
		default:
			throw new AssertionError(String.format("No logic for handling status %s.", newStatus));
		}
	}

	synchronized void setStatus(final PlayerTurnStatus newStatus) {
		final PlayerTurnStatus oldStatus = currentStatus;
		currentStatus = newStatus;
		if (!oldStatus.equals(newStatus)) {
			updateStatus(newStatus);
		}
	}

}
