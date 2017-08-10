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

import java.awt.Dimension;
import java.time.LocalDateTime;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.stream.Collector;
import java.util.stream.Collectors;

import javax.swing.JTable;
import javax.swing.WindowConstants;

import se.kth.speech.coin.tangrams.analysis.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.SessionEventDialogueManager;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 9 Aug 2017
 *
 */
public final class SessionEventLogAdjusterGUI implements Runnable {

	private static final int ESTIMATED_DIAGS_PER_SESSION = 96;

	private static final int ESTIMATED_UNIQUE_TOKEN_SEQ_PER_DIAG_COUNT = 8;

	private static final Collector<CharSequence, ?, String> WORD_JOINER = Collectors.joining(" ");

	private static String createHistoryTitleStr(final String gameId, final LocalDateTime startTime) {
		return String.format("Game %s, started at %s", gameId, startTime);
	}

	private static void setMaxPreferredScrollableViewportSize(final JTable table) {
		final Dimension diagTablereferredScrollableViewportSize = table.getPreferredScrollableViewportSize();
		// http://stackoverflow.com/a/1936582/1391325
		final Dimension screenSize = table.getToolkit().getScreenSize();
		final double scaleFactor = 1.0;
		final Dimension maxPreferredSize = new Dimension(Math.toIntExact(Math.round(screenSize.width * scaleFactor)),
				Math.toIntExact(Math.round(screenSize.height * scaleFactor)));
		if (diagTablereferredScrollableViewportSize.width < maxPreferredSize.width) {
			diagTablereferredScrollableViewportSize.width = maxPreferredSize.width;
		}
		table.setPreferredScrollableViewportSize(diagTablereferredScrollableViewportSize);
	}

	private final SessionEventDialogueManager.SessionGame game;

	private final ConcurrentMap<List<String>, String> tokenSeqReprs;

	public SessionEventLogAdjusterGUI(final SessionEventDialogueManager.SessionGame game) {
		this.game = game;
		tokenSeqReprs = new ConcurrentHashMap<>(
				ESTIMATED_DIAGS_PER_SESSION * ESTIMATED_UNIQUE_TOKEN_SEQ_PER_DIAG_COUNT);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Runnable#run()
	 */
	@Override
	public void run() {
		vizualize(game);
	}

	private String fetchTokenSeqRepr(final List<String> tokenSeq) {
		return tokenSeqReprs.computeIfAbsent(tokenSeq, key -> key.stream().collect(WORD_JOINER));
	}

	private void vizualize(final SessionEventDialogueManager.SessionGame game) {
		final LocalDateTime gameStart = game.getHistory().getStartTime();
		final String title = createHistoryTitleStr(game.getGameId(), gameStart);
		final List<EventDialogue> diags = game.getEventDialogues();
		final EventDialogueAdjusterTable diagTable = new EventDialogueAdjusterTable(
				new EventDialogueTableModel(diags.toArray(new EventDialogue[diags.size()])),
				new UtteranceCellRenderer(this::fetchTokenSeqRepr));
		setMaxPreferredScrollableViewportSize(diagTable);

		final EventDialogueAdjusterFrame frame = new EventDialogueAdjusterFrame(title, gameStart, diagTable);
		frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		frame.pack();
		frame.setVisible(true);
	}

}
