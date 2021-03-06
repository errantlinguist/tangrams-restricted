/*
 *  This file is part of Tangrams-restricted.
 *
 *  Tangrams-restricted is free software: you can redistribute it and/or modify
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

import java.awt.Dimension;
import java.awt.Point;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Iterator;
import java.util.Map.Entry;
import java.util.concurrent.ExecutorService;
import java.util.function.Consumer;
import java.util.function.Supplier;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfoTableWriter;
import se.kth.speech.coin.tangrams.iristk.GameState;

/**
 *
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since 16 Nov 2016
 *
 */
public final class GameGUI implements Runnable {

	private static final String IMAGE_INFO_LOGFILE_NAME = "img-info.tsv";

	// private static JMenu createFileMenu(final Window view) {
	// final JMenu result = new JMenu("File");
	// result.setMnemonic(KeyEvent.VK_F);
	// final String desc = "File actions";
	// result.getAccessibleContext().setAccessibleDescription(desc);
	// result.setToolTipText(desc);
	// {
	// final JMenuItem quitItem = createQuitMenuItem(view);
	// result.add(quitItem);
	// {
	// final int shortcut = KeyEvent.VK_Q;
	// quitItem.setMnemonic(shortcut);
	// quitItem.setAccelerator(KeyStroke.getKeyStroke(shortcut,
	// InputEvent.CTRL_MASK));
	// }
	// }
	// return result;
	// }
	//
	// private static JMenuBar createMenuBar(final Window view) {
	// final JMenuBar result = new JMenuBar();
	// final JMenu fileMenu = createFileMenu(view);
	// result.add(fileMenu);
	// return result;
	// }
	//
	// private static JMenuItem createQuitMenuItem(final Window windowToClose) {
	// final JMenuItem result = new JMenuItem("Quit");
	// {
	// final int shortcut = KeyEvent.VK_Q;
	// result.setMnemonic(shortcut);
	// result.setAccelerator(KeyStroke.getKeyStroke(shortcut,
	// InputEvent.CTRL_MASK));
	// }
	// final String desc = "Quit the application.";
	// result.getAccessibleContext().setAccessibleDescription(desc);
	// result.setToolTipText(desc);
	// result.addItemListener(e -> {
	// windowToClose.dispatchEvent(new WindowEvent(windowToClose,
	// WindowEvent.WINDOW_CLOSING));
	// });
	// return result;
	// }

	private static final Logger LOGGER = LoggerFactory.getLogger(GameGUI.class);

	private static Consumer<Iterator<? extends Entry<Integer, ImageVisualizationInfo.Datum>>> createImgVizInfoWriter(
			final ExecutorService backgroundJobService, final Supplier<? extends Path> logOutdirPathSupplier) {
		return imgVizInfoData -> {
			backgroundJobService.submit(() -> {
				final Path outdir = logOutdirPathSupplier.get();
				final Path outfile = outdir.resolve(IMAGE_INFO_LOGFILE_NAME);
				try (final BufferedWriter writer = Files.newBufferedWriter(outfile)) {
					final ImageVisualizationInfoTableWriter infoWriter = new ImageVisualizationInfoTableWriter(writer);
					infoWriter.write(imgVizInfoData);
				} catch (final IOException e) {
					throw new UncheckedIOException(e);
				}
			});
		};
	}

	private final GameState gameState;

	private final String title;

	private final Point viewCenterpoint;

	private final WindowListener viewClosedListener;

	private final InteractiveGameViewFrameFactory viewFactory;

	public GameGUI(final String title, final Point viewCenterpoint, final GameState gameState,
			final Supplier<? extends Path> logOutdirPathSupplier, final ExecutorService backgroundJobService,
			final Runnable closeHook, final boolean analysisEnabled) {
		final Supplier<Path> screenshotDirSupplier = () -> logOutdirPathSupplier.get().resolve("screenshots");
		viewFactory = new InteractiveGameViewFrameFactory(
				new ScreenshotLogger(screenshotDirSupplier, () -> gameState.getController().getPlayerId(),
						backgroundJobService),
				createImgVizInfoWriter(backgroundJobService, logOutdirPathSupplier), backgroundJobService,
				analysisEnabled);
		this.title = title;
		this.viewCenterpoint = viewCenterpoint;
		this.gameState = gameState;
		viewClosedListener = new WindowAdapter() {

			/*
			 * (non-Javadoc)
			 *
			 * @see java.awt.event.WindowAdapter#windowClosed(java.awt.event.
			 * WindowEvent)
			 */
			@Override
			public void windowClosed(final WindowEvent e) {
				closeHook.run();
			}

		};
	}

	@Override
	public void run() {
		final InteractiveGameViewFrame view = viewFactory.apply(new InteractiveGameViewFrameFactory.Parameters(
				gameState.getController(), gameState.getImageVisualizationInfo(), gameState.getRnd()));
		view.addWindowListener(viewClosedListener);
		view.setTitle(title);
		// gameViewFrame.setJMenuBar(createMenuBar(gameViewFrame));
		// http://stackoverflow.com/a/1936582/1391325
		final Dimension screenSize = view.getToolkit().getScreenSize();
		LOGGER.debug("Setting maximum component size to {}.", screenSize);
		view.setMaximumSize(screenSize);
		view.pack();

		final Point viewLocation = new Point(viewCenterpoint.x - view.getWidth() / 2,
				viewCenterpoint.y - view.getHeight() / 2);
		view.setLocation(viewLocation);
		view.setVisible(true);
	}
}