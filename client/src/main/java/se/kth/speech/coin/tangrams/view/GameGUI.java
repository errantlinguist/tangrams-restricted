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
import java.awt.Dimension;
import java.awt.Image;
import java.awt.Point;
import java.awt.Window;
import java.awt.event.InputEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.image.BufferedImage;
import java.nio.file.Path;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.IntFunction;
import java.util.function.Supplier;

import javax.swing.JCheckBoxMenuItem;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;
import javax.swing.WindowConstants;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.coin.tangrams.game.LocalController;
import se.kth.speech.coin.tangrams.game.Model;
import se.kth.speech.coin.tangrams.game.RemoteController;
import se.kth.speech.coin.tangrams.iristk.events.Selection;
import se.kth.speech.coin.tangrams.iristk.events.Turn;

/**
 *
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 16 Nov 2016
 *
 */
public final class GameGUI<T> implements Runnable {

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
		winningConfigItem.addItemListener(new ItemListener() {

			@Override
			public void itemStateChanged(final ItemEvent e) {
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
			}

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

	private final Function<? super T, ? extends Image> coordOccupantImageFactory;

	private final LocalController<T> localController;

	private final RemoteController<T> remoteController;

	private final IntFunction<? extends BufferedImage> rowBackgroundImageFactory;

	private final BiConsumer<Component, Selection> selectionLogger;

	private final String title;

	private final BiConsumer<Component, Turn> turnScreenshotLogger;

	private final Point viewLocation;

	private final Model<T> winningModel;

	public GameGUI(final String title, final Point viewLocation, final LocalController<T> localController,
			final RemoteController<T> remoteController, final Model<T> winningModel,
			final Function<? super T, ? extends Image> coordOccupantImageFactory,
			final IntFunction<? extends BufferedImage> rowBackgroundImageFactory,
			final Supplier<? extends Path> logOutdirSupplier, final Runnable closeHook) {
		this.title = title;
		this.viewLocation = viewLocation;
		this.localController = localController;
		this.remoteController = remoteController;
		this.winningModel = winningModel;
		this.coordOccupantImageFactory = coordOccupantImageFactory;
		this.rowBackgroundImageFactory = rowBackgroundImageFactory;
		final ExecutorService screenshotLoggingExecutor = Executors.newSingleThreadExecutor();
		{
			final ScreenshotLogger screenshotLogger = new ScreenshotLogger(logOutdirSupplier,
					localController::getPlayerId, screenshotLoggingExecutor);
			this.turnScreenshotLogger = new BiConsumer<Component, Turn>() {

				@Override
				public void accept(final Component view, final Turn turn) {
					final int turnNo = turn.getSequenceNumber();
					final String filenamePrefix = "turn-" + Integer.toString(turnNo);
					screenshotLogger.accept(view, filenamePrefix);
				}
			};
			this.selectionLogger = new TimestampingSelectionLogger(screenshotLogger);
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
		final BiFunction<Model<T>, Dimension, ModelCoordinateGridPanel<?>> gridPanelFactory = new RowBackgroundPaintedModelCoordinateGridPanelFactory<>(
				coordOccupantImageFactory, rowBackgroundImageFactory);
		// TODO: Make size configurable
		final JFrame winningConfigFrame = new WinningConfigurationViewFactory<>(gridPanelFactory, 75)
				.apply(winningModel);
		winningConfigFrame.pack();
		winningConfigFrame.setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
		final boolean isWinningConfigViewVisible = true;
		final GameViewFrame<T> gameViewFrame = new GameViewFrame<>(title, localController, remoteController,
				gridPanelFactory, turnScreenshotLogger, selectionLogger, closeHook);
		gameViewFrame.setJMenuBar(createMenuBar(winningConfigFrame, isWinningConfigViewVisible));
		// Have the game view frame manage the lifetime of the winning config
		// frame
		gameViewFrame.getChildWindows().add(winningConfigFrame);

		// Wait until another player has joined
		// TODO: Add toggle for single-player mode
		gameViewFrame.pack();
		// gameViewFrame.setLocationByPlatform(true);
		gameViewFrame.setLocation(viewLocation);
		// gameViewFrame.setLocationRelativeTo(null);
		gameViewFrame.setVisible(true);

		winningConfigFrame.setLocationRelativeTo(gameViewFrame);
		// final Point initialLocation = winningConfigFrame.getLocation();
		// final Dimension gameViewFrameSize = gameViewFrame.getSize();
		// final int xOffset = gameViewFrameSize.width + 10;
		// final int yOffset = gameViewFrameSize.height + 10;
		// winningConfigFrame.setLocation(initialLocation.x + xOffset,
		// initialLocation.y + yOffset);

		// Set winning config view to visible after the game view frame so the
		// former loads after the latter
		winningConfigFrame.setVisible(isWinningConfigViewVisible);
	}
}