/*
 *  This file is part of client.
 *
 *  client is free software: you can redistribute it and/or modify
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

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.List;

import javax.swing.JFrame;

import se.kth.speech.coin.tangrams.content.ImageDatum;
import se.kth.speech.coin.tangrams.game.LocalController;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 2 Mar 2017
 *
 */
final class GameViewFrame extends JFrame {

	/**
	 *
	 */
	private static final long serialVersionUID = -4129777933223228599L;

	/**
	 * @param localController
	 */
	public GameViewFrame(final LocalController<Integer> localController) {
		// TODO Auto-generated constructor stub
	}

	GameViewFrame(final List<ImageDatum> imageData, final LocalController<? super Integer> localController) {
		GameBoardPanel boardPanel = new GameBoardPanelFactory().apply(imageData);
		add(boardPanel);
		setDefaultCloseOperation(DISPOSE_ON_CLOSE);
		addWindowListener(new WindowAdapter() {

			/*
			 * (non-Javadoc)
			 *
			 * @see java.awt.event.WindowAdapter#windowClosed(java.awt.event.
			 * WindowEvent)
			 */
			@Override
			public void windowClosed(final WindowEvent e) {
				// TODO Auto-generated method stub
				super.windowClosed(e);
			}

		});
		// TODO Auto-generated constructor stub
	}

}
