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
import java.awt.Window;
import java.awt.event.InputEvent;
import java.awt.event.ItemEvent;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.swing.JCheckBoxMenuItem;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Maps;

import se.kth.speech.SpatialMatrix;
import se.kth.speech.coin.tangrams.RandomModelPopulator;
import se.kth.speech.coin.tangrams.content.ImageLoadingImageViewInfoFactory;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfoFactory;
import se.kth.speech.coin.tangrams.game.LocalController;
import se.kth.speech.coin.tangrams.game.RemoteController;
import se.kth.speech.coin.tangrams.iristk.GameState;
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

	private static JMenuBar createMenuBar(final Window winningConfigurationView,
			final boolean isWinningConfigItemSelected) {
		final JMenuBar result = new JMenuBar();

		final JMenuItem viewMenu = createViewMenuItem(winningConfigurationView, isWinningConfigItemSelected);
		result.add(viewMenu);

		return result;
	}

	private static JMenuItem createViewMenuItem(final Window winningConfigurationView,
			final boolean isWinningConfigItemSelected) {
		final JMenu result = new JMenu("View");
		result.setMnemonic(KeyEvent.VK_V);
		final String desc = "View options";
		result.getAccessibleContext().setAccessibleDescription(desc);
		result.setToolTipText(desc);
		final JMenuItem winningConfigItem = new JCheckBoxMenuItem("Show winning configuration",
				isWinningConfigItemSelected);
		result.add(winningConfigItem);
		{
			final int viewMenuShortcut = KeyEvent.VK_W;
			winningConfigItem.setMnemonic(viewMenuShortcut);
			winningConfigItem.setAccelerator(KeyStroke.getKeyStroke(viewMenuShortcut, InputEvent.ALT_MASK));
		}
		final String winningConfigItemDesc = "Show the winning board configuration.";
		winningConfigItem.getAccessibleContext().setAccessibleDescription(winningConfigItemDesc);
		winningConfigItem.setToolTipText(winningConfigItemDesc);
		winningConfigItem.addItemListener(e -> {
			final boolean isNowVisible;

			final int event = e.getStateChange();
			switch (event) {
			case ItemEvent.DESELECTED: {
				isNowVisible = false;
				break;
			}
			case ItemEvent.SELECTED: {
				isNowVisible = true;
				break;
			}
			default: {
				// Do nothing
				isNowVisible = winningConfigurationView.isVisible();
				break;
			}
			}

			winningConfigurationView.setVisible(isNowVisible);
		});

		winningConfigurationView.addWindowListener(new WindowAdapter() {

			@Override
			public void windowClosing(final WindowEvent e) {
				LOGGER.debug("Closing winning configuration view.");
				winningConfigItem.setSelected(false);
			}

			@Override
			public void windowOpened(final WindowEvent e) {
				LOGGER.debug("Opening winning configuration view.");
				winningConfigItem.setSelected(true);
			}
		});

		return result;
	}

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
					() -> gameState.getLocalController().getPlayerId(), screenshotLoggingExecutor);
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

	private static final BiFunction<Image, Toolkit, Image> DEFAULT_POST_COLORING_IMG_TRANSFORMER = new BiFunction<Image, Toolkit, Image>() {

		@Override
		public Image apply(final Image img, final Toolkit toolkit) {
			// Do nothing
			LOGGER.debug("Created instance {}.", img);
			return img;
		}

	};

	@Override
	public void run() {
		LOGGER.debug("Creating view components.");
		LocalController<Integer> localController = gameState.getLocalController();
		final SpatialMatrix<Integer> model = localController.getModel();
		final int pieceCount = model.getElementPlacements().getAllElements().size();
		final Random rnd = gameState.getRnd();
		ImageVisualizationInfoFactory imgVisInfoFactory = new ImageVisualizationInfoFactory(rnd);
		final Map<Integer, Image> pieceImgs = Maps.newHashMapWithExpectedSize(pieceCount);
		GameBoardPanel<Integer> gameBoardPanel = new GameBoardPanel<>(model, pieceImgs);
		final ImageLoadingImageViewInfoFactory imgViewInfoFactory = new ImageLoadingImageViewInfoFactory(
				gameBoardPanel.getToolkit(), DEFAULT_POST_COLORING_IMG_TRANSFORMER,
				Maps.newHashMapWithExpectedSize(imgVisInfoFactory.getImgResourceUsageCounts().keySet().size()));
		final Stream<ImageVisualizationInfo> imgVisualizationInfoData = Stream.generate(imgVisInfoFactory::next)
				.limit(pieceCount);
		
		imgVisualizationInfoData.map(imgViewInfoFactory::apply).forEach(imgViewInfoDatum -> {
			Image oldImg = pieceImgs.put(pieceImgs.size(), imgViewInfoDatum.getValue());
			assert oldImg == null;
		});
		
		final GameViewFrame gameViewFrame = new GameViewFrame(gameBoardPanel, rnd);
		gameViewFrame.pack();
		gameViewFrame.setLocation(viewLocation);
		gameViewFrame.setVisible(true);
	}
}