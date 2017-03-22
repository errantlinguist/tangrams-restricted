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

import java.awt.Component;
import java.awt.Image;
import java.awt.Point;
import java.awt.Toolkit;
import java.nio.file.Path;
import java.util.Map;
import java.util.Random;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Supplier;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Maps;

import se.kth.speech.SpatialMatrix;
import se.kth.speech.coin.tangrams.content.ImageLoadingImageViewInfoFactory;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfoFactory;
import se.kth.speech.coin.tangrams.game.GameState;
import se.kth.speech.coin.tangrams.iristk.Controller;
import se.kth.speech.coin.tangrams.iristk.events.Selection;
import se.kth.speech.coin.tangrams.iristk.events.Turn;

/**
 *
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 16 Nov 2016
 *
 */
public final class GameGUI implements Runnable {

	private static final Logger LOGGER = LoggerFactory.getLogger(GameGUI.class);

	private static final BiFunction<Image, Toolkit, Image> DEFAULT_POST_COLORING_IMG_TRANSFORMER = new BiFunction<Image, Toolkit, Image>() {

		@Override
		public Image apply(final Image img, final Toolkit toolkit) {
			// Do nothing
			LOGGER.debug("Created instance {}.", img);
			return img;
		}

	};

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

	private final Runnable closeHook;

	private final BiConsumer<Component, Selection> selectionLogger;

	private final String title;

	private final BiConsumer<Component, Turn> turnScreenshotLogger;

	private final Point viewLocation;

	private final GameState gameState;

	public GameGUI(final String title, final Point viewLocation, final GameState gameState,
			final Supplier<? extends Path> logOutdirSupplier, final Runnable closeHook) {
		this.title = title;
		this.viewLocation = viewLocation;
		this.gameState = gameState;
		final ExecutorService screenshotLoggingExecutor = Executors.newSingleThreadExecutor();
		{
			final ScreenshotLogger screenshotLogger = new ScreenshotLogger(logOutdirSupplier,
					() -> gameState.getController().getPlayerId(), screenshotLoggingExecutor);
			turnScreenshotLogger = (view, turn) -> {
				final int turnNo = turn.getSequenceNumber();
				final String filenamePrefix = "turn-" + Integer.toString(turnNo);
				screenshotLogger.accept(view, filenamePrefix);
			};
			selectionLogger = new TimestampingSelectionLogger(screenshotLogger);
		}
		this.closeHook = () -> {
			try {
				closeHook.run();
			} finally {
				screenshotLoggingExecutor.shutdown();
			}
		};

	}

	@Override
	public void run() {
		LOGGER.debug("Creating view components.");
		final Controller controller = gameState.getController();
		final SpatialMatrix<Integer> model = controller.getModel();
		final int pieceCount = model.getElementPlacements().getAllElements().size();
		final Random rnd = gameState.getRnd();
		final ImageVisualizationInfoFactory imgVisInfoFactory = new ImageVisualizationInfoFactory(rnd);
		final Map<Integer, Image> pieceImgs = Maps.newHashMapWithExpectedSize(pieceCount);
		final GameBoardPanel gameBoardPanel = new GameBoardPanel(model, pieceImgs, controller,
				turnScreenshotLogger, selectionLogger);
		final ImageLoadingImageViewInfoFactory imgViewInfoFactory = new ImageLoadingImageViewInfoFactory(
				gameBoardPanel.getToolkit(), DEFAULT_POST_COLORING_IMG_TRANSFORMER,
				Maps.newHashMapWithExpectedSize(imgVisInfoFactory.getImgResourceUsageCounts().keySet().size()));
		final Stream<ImageVisualizationInfo> imgVisualizationInfoData = Stream.generate(imgVisInfoFactory::next)
				.limit(pieceCount);

		imgVisualizationInfoData.map(imgViewInfoFactory::apply).forEach(imgViewInfoDatum -> {
			final Image oldImg = pieceImgs.put(pieceImgs.size(), imgViewInfoDatum.getValue());
			assert oldImg == null;
		});

		final GameViewFrame gameViewFrame = new GameViewFrame(gameBoardPanel, rnd, controller, closeHook);
		gameViewFrame.setTitle(title);
		// gameViewFrame.setJMenuBar(createMenuBar(gameViewFrame));

		// Have the game view frame manage the lifetime of the frames accessed
		// via menu
		// gameViewFrame.getChildWindows().add(winningConfigFrame);

		gameViewFrame.pack();
		// gameViewFrame.setLocationByPlatform(true);
		gameViewFrame.setLocation(viewLocation);
		// gameViewFrame.setLocationRelativeTo(null);
		gameViewFrame.setVisible(true);

	}
}