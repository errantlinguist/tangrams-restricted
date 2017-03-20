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

import java.awt.Component;

import javax.swing.JOptionPane;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 25 Jan 2017
 *
 */
final class MoveDialogs {

	public static int showSelectConfirmDialog(final Component parentComponent) {
		return JOptionPane.showConfirmDialog(parentComponent,
				"Are you sure you want to choose the selected piece?", "Select piece",
				JOptionPane.YES_NO_OPTION);
	}

	private MoveDialogs() {

	}

}
