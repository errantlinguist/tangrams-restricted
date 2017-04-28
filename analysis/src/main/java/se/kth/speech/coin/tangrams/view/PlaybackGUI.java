/*
 *  This file is part of se.kth.speech.coin.tangrams.playback.
 *
 *  se.kth.speech.coin.tangrams.playback is free software: you can redistribute it and/or modify
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
import java.awt.EventQueue;
import java.awt.Window;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Random;

import javax.swing.JOptionPane;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.SpatialMatrix;
import se.kth.speech.coin.tangrams.analysis.GameHistory;
import se.kth.speech.coin.tangrams.game.Controller;
import se.kth.speech.coin.tangrams.iristk.GameManagementClient;
import se.kth.speech.coin.tangrams.iristk.events.Area2D;
import se.kth.speech.coin.tangrams.iristk.events.GameStateDescription;
import se.kth.speech.coin.tangrams.iristk.events.GameStateUnmarshalling;
import se.kth.speech.coin.tangrams.iristk.events.Move;
import se.kth.speech.coin.tangrams.iristk.io.LoggedEvents;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 25 Jan 2017
 *
 */
public final class PlaybackGUI implements Runnable {

	private class ClosingFileOpener implements Runnable {

		private final Window view;

		private ClosingFileOpener(final Window view) {
			this.view = view;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.lang.Runnable#run()
		 */
		@Override
		public void run() {
			LOGGER.debug("Running closing file opener for view: {}", view);
			PlaybackFrame newView = null;
			try {
				newView = createPlaybackView(view);
				if (newView == null) {
					LOGGER.debug("User cancelled file opening.");
				} else {
					// http://stackoverflow.com/a/1235994/1391325
					view.dispatchEvent(new WindowEvent(view, WindowEvent.WINDOW_CLOSING));
					showView(newView);
				}
			} catch (final IOException e) {
				JOptionPane.showMessageDialog(view, e.getClass().getSimpleName(), e.getLocalizedMessage(),
						JOptionPane.ERROR_MESSAGE);
			}
		}
	}

	private static class PlaybackClient implements GameManagementClient {

		/*
		 * (non-Javadoc)
		 *
		 * @see se.kth.speech.coin.tangrams.iristk.GameManagementClient#
		 * rejectSelection(java.lang.Integer,
		 * se.kth.speech.coin.tangrams.iristk.events.Area2D)
		 */
		@Override
		public void rejectSelection(final Integer pieceId, final Area2D area) {
			// TODO Auto-generated method stub

		}

		/*
		 * (non-Javadoc)
		 *
		 * @see se.kth.speech.coin.tangrams.iristk.GameManagementClient#
		 * requestJoinGame()
		 */
		@Override
		public void requestJoinGame() {
			// TODO Auto-generated method stub

		}

		/*
		 * (non-Javadoc)
		 *
		 * @see se.kth.speech.coin.tangrams.iristk.GameManagementClient#
		 * requestNextMove(se.kth.speech.coin.tangrams.iristk.events.Move)
		 */
		@Override
		public void requestNextMove(final Move move) {
			// TODO Auto-generated method stub

		}

		/*
		 * (non-Javadoc)
		 *
		 * @see se.kth.speech.coin.tangrams.iristk.GameManagementClient#
		 * requestSelection(java.lang.Integer,
		 * se.kth.speech.coin.tangrams.iristk.events.Area2D)
		 */
		@Override
		public void requestSelection(final Integer pieceId, final Area2D area) {
			// TODO Auto-generated method stub

		}

		/*
		 * (non-Javadoc)
		 *
		 * @see se.kth.speech.coin.tangrams.iristk.GameManagementClient#
		 * requestTurnCompletion(se.kth.speech.coin.tangrams.iristk.events.Move)
		 */
		@Override
		public void requestTurnCompletion(final Move move) {
			// TODO Auto-generated method stub

		}

	}

	private static final Logger LOGGER = LoggerFactory.getLogger(PlaybackGUI.class);

	private static final Path SETTINGS_FILE = Paths.get(".settings", PlaybackGUI.class.getName() + ".properties");

	private static final String WORKING_DIR_PROP_NAME = "workingdir";

	public static void main(final String[] args) throws IOException {
		final Properties settings = new Properties();
		try (InputStream settingsInput = Files.newInputStream(SETTINGS_FILE)) {
			settings.load(settingsInput);
		} catch (final NoSuchFileException e) {
			LOGGER.debug("Settings file not found at \"{}\".", SETTINGS_FILE);
		}
		EventQueue.invokeLater(new PlaybackGUI(settings));
	}

	private static PlaybackFrame createPlaybackView(final Path eventLogFile) throws IOException {
		final PlaybackFrame result;
		final Map<String, GameHistory> gameHistories = LoggedEvents.parseGameHistories(Files.lines(eventLogFile));
		final int gameCount = gameHistories.size();
		switch (gameCount) {
		case 1: {
			final Entry<String, GameHistory> toVisualize = gameHistories.entrySet().iterator().next();
			LOGGER.info("Creating playback view for game \"{}\".", toVisualize.getKey());
			final GameHistory history = toVisualize.getValue();
			final GameStateDescription initialState = history.getInitialState();
			final SpatialMatrix<Integer> model = GameStateUnmarshalling.createModel(initialState.getModelDescription(),
					SpatialMatrix.Factory.STABLE_ITER_ORDER);
			final Controller controller = new PlaybackController(model, new PlaybackClient());
			final Random rnd = new Random(initialState.getSeed());
			result = new PlaybackFrameFactory()
					.apply(new PlaybackFrameFactory.Parameters(controller, history, rnd));
			break;
		}
		default: {
			throw new UnsupportedOperationException(
					String.format("No logic for handling a game count of %d.", gameCount));
		}
		}

		return result;
	}

	private final InputPlaybackFilePromptFactory fileOpener;

	private final Properties settings;

	public PlaybackGUI(final Properties settings) {
		this.settings = settings;
		final String workingDirStr = settings.getProperty(WORKING_DIR_PROP_NAME);
		final InputPlaybackFilePromptFactory fileOpener;
		if (workingDirStr == null) {
			fileOpener = new InputPlaybackFilePromptFactory();
		} else {
			LOGGER.debug("Using \"{}\" as the working directory.", workingDirStr);
			fileOpener = new InputPlaybackFilePromptFactory(new File(workingDirStr));
		}
		this.fileOpener = fileOpener;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Runnable#run()
	 */
	@Override
	public void run() {
		PlaybackFrame playbackView = null;
		boolean parsedInput = false;
		do {
			try {
				playbackView = createPlaybackView((Component) null);
				if (playbackView == null) {
					LOGGER.info("User cancelled file opening.");
				} else {
					showView(playbackView);
				}
				parsedInput = true;

			} catch (final IOException e) {
				JOptionPane.showMessageDialog(null, e.getClass().getSimpleName(), e.getLocalizedMessage(),
						JOptionPane.ERROR_MESSAGE);
			}
		} while (!parsedInput);

	}

	private PlaybackFrame createPlaybackView(final Component parent) throws IOException {
		final PlaybackFrame result;
		final File eventLogFile = fileOpener.apply(parent);
		if (eventLogFile == null) {
			result = null;
		} else {
			LOGGER.info("Reading event logfile \"{}\".", eventLogFile);
			result = createPlaybackView(eventLogFile.toPath());
			result.addWindowListener(new WindowAdapter() {

				@Override
				public void windowClosed(final WindowEvent event) {
					LOGGER.debug("Persisting settings to disk.");
					try {
						persistSettings();
					} catch (final IOException ex) {
						throw new UncheckedIOException(ex);
					}
				}

			});
		}
		return result;
	}

	private void persistSettings() throws IOException {
		settings.setProperty(WORKING_DIR_PROP_NAME, fileOpener.getChooser().getCurrentDirectory().getAbsolutePath());
		final Path settingsDir = SETTINGS_FILE.getParent();
		if (settingsDir != null) {
			Files.createDirectories(settingsDir);
		}
		try (OutputStream os = Files.newOutputStream(SETTINGS_FILE, StandardOpenOption.CREATE,
				StandardOpenOption.TRUNCATE_EXISTING)) {
			settings.store(os, "The persisted user settings for " + PlaybackGUI.class.getName() + ".");
		}
	}

	private void showView(final PlaybackFrame playbackView) {
		playbackView.getFileOpeningHooks().add(new ClosingFileOpener(playbackView));
		playbackView.setLocationByPlatform(true);
		playbackView.pack();
		playbackView.setVisible(true);
	}

}
