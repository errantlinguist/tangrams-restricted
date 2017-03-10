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

import java.awt.BorderLayout;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.Collection;
import java.util.List;
import java.util.Random;
import java.util.function.BiFunction;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;

import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;

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

	GameViewFrame(final List<ImageVisualizationInfo> imgVisualizationInfoData, final Random rnd,
			final BiFunction<Collection<ImageVisualizationInfo>, Random, GameBoardPanel> gameBoardPanelFactory) {
		setLayout(new BorderLayout());
		final GameBoardPanel boardPanel = gameBoardPanelFactory.apply(imgVisualizationInfoData, rnd);
		add(boardPanel, BorderLayout.CENTER);

		final JPanel buttonPanel = new JPanel();
		add(buttonPanel, BorderLayout.SOUTH);
		final JButton undoButton = new JButton("undo");
		buttonPanel.add(undoButton);
		undoButton.addActionListener(continueEvent -> {
			boardPanel.notifyUndo();
		});
		final JButton continueButton = new JButton("continue");
		buttonPanel.add(continueButton);
		continueButton.addActionListener(continueEvent -> {
			boardPanel.notifyContinue();
		});
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
