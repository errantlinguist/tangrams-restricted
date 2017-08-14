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

import java.awt.Container;
import java.awt.HeadlessException;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.time.LocalDateTime;
import java.time.temporal.TemporalAmount;
import java.util.Map;
import java.util.function.Function;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.KeyStroke;
import javax.swing.ScrollPaneConstants;

final class EventDialogueAdjusterFrame extends JFrame {

	/**
	 *
	 */
	private static final long serialVersionUID = -5412327154602984470L;

	EventDialogueAdjusterFrame(final String title, final LocalDateTime gameStartTime,
			final EventDialogueAdjusterTable diagTable, final JFileChooser eventLogExportFileChooser,
			final TemporalAmount minEventTimeDiff, final Function<? super LocalDateTime, String> evtTimeFormatter)
			throws HeadlessException {
		super(title);
		final Container content = getContentPane();
		content.setLayout(new BoxLayout(content, BoxLayout.PAGE_AXIS));
		// https://stackoverflow.com/a/2452758/1391325
		content.add(new JScrollPane(diagTable, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
				ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED));

		final Map<EventDialogueAttribute, Integer> evtDiagAttrColIdxs = EventDialogueAttributeTables
				.createEventDialogueAttributeColumnIndexMap(diagTable);

		final JPanel actionPanel = new JPanel();
		content.add(actionPanel);
		{
			final JButton newButton = new JButton("Minimize following event time");
			newButton.addActionListener(new FollowingEventTimeMinimizer(diagTable, evtDiagAttrColIdxs, this,
					gameStartTime, minEventTimeDiff, evtTimeFormatter));
			actionPanel.add(newButton);
		}
		{
			final JButton newButton = new JButton("Move last utterance to following dialogue");
			newButton.addActionListener(new LastEventToNextDialogueMover(diagTable, evtDiagAttrColIdxs, this,
					gameStartTime, minEventTimeDiff, evtTimeFormatter));
			actionPanel.add(newButton);
		}

		final JMenuBar menuBar = new JMenuBar();
		setJMenuBar(menuBar);
		final JMenu fileMenu = new JMenu("File");
		fileMenu.setMnemonic(KeyEvent.VK_F);
		menuBar.add(fileMenu);
		{
			final int mnemonic = KeyEvent.VK_E;
			final JMenuItem exportItem = new JMenuItem("Export event log...");
			exportItem.setMnemonic(mnemonic);
			exportItem.setAccelerator(KeyStroke.getKeyStroke(mnemonic, InputEvent.CTRL_MASK));
			exportItem.addActionListener(new SaveAction(eventLogExportFileChooser, this));
			fileMenu.add(exportItem);
		}
	}

}