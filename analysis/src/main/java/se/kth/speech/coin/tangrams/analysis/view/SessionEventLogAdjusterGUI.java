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
import java.io.File;
import java.time.LocalDateTime;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;

import javax.swing.JFileChooser;
import javax.swing.JTable;
import javax.swing.WindowConstants;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.table.TableCellRenderer;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Maps;

import se.kth.speech.coin.tangrams.analysis.SessionGame;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.coin.tangrams.iristk.EventTimes;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 9 Aug 2017
 *
 */
public final class SessionEventLogAdjusterGUI implements Runnable {

	private static final Logger LOGGER = LoggerFactory.getLogger(SessionEventLogAdjusterGUI.class);

	private static final String NULL_VALUE_REPR = "-";

	private static final Function<LocalDateTime, String> TIME_FORMATTER = time -> EventTimes.FORMATTER.format(time);

	private static final Function<String, LocalDateTime> TIME_PARSER = EventTimes::parseEventTime;

	private static Map<Class<?>, TableCellRenderer> createDefaultRendererMap() {
		final Map<Class<?>, TableCellRenderer> result = Maps.newHashMapWithExpectedSize(3);
		result.put(String.class, new DefaultValueStringRenderer(NULL_VALUE_REPR));
		result.put(LocalDateTime.class, new DefaultValueTimeRenderer(TIME_FORMATTER, NULL_VALUE_REPR));
		result.put(Utterance.class, new UtteranceCellRenderer());
		return result;
	}

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

	private final Optional<? extends File> eventLogExportDir;

	private final SessionGame game;

	public SessionEventLogAdjusterGUI(final SessionGame game, final Optional<? extends File> eventLogExportDir) {
		this.game = game;
		this.eventLogExportDir = eventLogExportDir;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Runnable#run()
	 */
	@Override
	public void run() {
		final LocalDateTime gameStart = game.getHistory().getStartTime();
		final String title = createHistoryTitleStr(game.getGameId(), gameStart);
		final EventDialogueAdjusterTable diagTable = new EventDialogueAdjusterTable(
				new EventDialogueTableModel(game, TIME_PARSER, TIME_FORMATTER),
				createDefaultRendererMap().entrySet().stream());
		setMaxPreferredScrollableViewportSize(diagTable);

		final JFileChooser eventLogFileChooser = createEventLogExportFileChooser();

		final EventDialogueAdjusterFrame frame = new EventDialogueAdjusterFrame(title, gameStart, diagTable,
				eventLogFileChooser);
		frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		frame.pack();
		frame.setVisible(true);
	}

	private JFileChooser createEventLogExportFileChooser() {
		final JFileChooser result = eventLogExportDir.map(dir -> {
			final boolean exportDirWasNonexistent = dir.mkdirs();
			if (exportDirWasNonexistent) {
				LOGGER.debug("Created non-existent event log export directory \"{}\".", dir);
			}
			return new JFileChooser(dir);
		}).orElseGet(JFileChooser::new);
		result.setFileFilter(new FileNameExtensionFilter("Text files (*.txt)", "txt"));
		return result;
	}

}
