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

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Image;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.image.FilteredImageSource;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Random;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.concurrent.ExecutorService;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.IntFunction;
import java.util.function.Supplier;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Maps;

import se.kth.speech.MutablePair;
import se.kth.speech.URLFilenameBaseSplitter;
import se.kth.speech.awt.OpaqueTransparencyReplacementImageFilter;
import se.kth.speech.coin.tangrams.content.BoardArea;
import se.kth.speech.coin.tangrams.content.ImageLoadingImageViewInfoFactory;
import se.kth.speech.coin.tangrams.content.ImageViewInfo;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfoTableWriter;
import se.kth.speech.coin.tangrams.game.Controller;
import se.kth.speech.coin.tangrams.game.PatternMoveFactory;
import se.kth.speech.coin.tangrams.iristk.GameState;

/**
 *
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 16 Nov 2016
 *
 */
public final class GameGUI implements Runnable {

	private static final BiFunction<Image, Toolkit, Image> DEFAULT_POST_COLORING_IMG_TRANSFORMER = new BiFunction<Image, Toolkit, Image>() {

		@Override
		public Image apply(final Image img, final Toolkit toolkit) {
			// Do nothing
			LOGGER.debug("Created instance {}.", img);
			return img;
		}

	};

	private static final String IMAGE_INFO_LOGFILE_NAME = "img-info.txt";

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

	private final boolean analysisEnabled;

	private final ExecutorService backgroundJobService;

	private final Runnable closeHook;

	private final GameState gameState;

	private final Consumer<Iterator<Entry<Integer, ImageVisualizationInfo.Datum>>> imgVizInfoWriter;

	private final ScreenshotLogger screenshotLogger;

	private final String title;

	private final Point viewCenterpoint;

	public GameGUI(final String title, final Point viewCenterpoint, final GameState gameState,
			final Supplier<? extends Path> logOutdirPathSupplier, final ExecutorService backgroundJobService,
			final Runnable closeHook, final boolean analysisEnabled) {
		this.title = title;
		this.viewCenterpoint = viewCenterpoint;
		this.gameState = gameState;
		this.analysisEnabled = analysisEnabled;
		screenshotLogger = new ScreenshotLogger(logOutdirPathSupplier, () -> gameState.getController().getPlayerId(),
				backgroundJobService);
		this.closeHook = closeHook;
		this.backgroundJobService = backgroundJobService;

		{
			final Function<URL, String> imgNameFactory = new URLFilenameBaseSplitter();
			imgVizInfoWriter = imgVizInfoData -> {
				backgroundJobService.submit(() -> {
					final Path outdir = logOutdirPathSupplier.get();
					final Path outfile = outdir.resolve(IMAGE_INFO_LOGFILE_NAME);
					try (final BufferedWriter writer = Files.newBufferedWriter(outfile)) {
						final ImageVisualizationInfoTableWriter infoWriter = new ImageVisualizationInfoTableWriter(
								writer, imgNameFactory);
						infoWriter.accept(imgVizInfoData);
					} catch (final IOException e) {
						throw new UncheckedIOException(e);
					}
				});
			};
		}
	}

	@Override
	public void run() {
		LOGGER.debug("Creating view components.");
		final Controller controller = gameState.getController();
		// String matrixStrRepr = new
		// MatrixStringReprFactory().apply(model.getPositionMatrix());
		// System.out.println(matrixStrRepr);
		final ImageVisualizationInfo imgVizInfo = gameState.getImageVisualizationInfo();
		final List<ImageVisualizationInfo.Datum> imgVizInfoData = imgVizInfo.getData();
		final Map<BoardArea, Color> boardAreaColors = BoardArea.getDefaultBoardAreaColorMap();
		final Random rnd = gameState.getRnd();
		final List<Image> pieceImgs = new ArrayList<>(imgVizInfoData.size());

		final IntFunction<Image> pieceIdImgGetter = pieceImgs::get;
		final int uniqueImageResourceCount = imgVizInfo.getUniqueImageResourceCount();
		final Entry<GameBoardPanel, ImageLoadingImageViewInfoFactory> gameBoardImgViewInfoFactory = analysisEnabled
				? createDebugGameBoardImgViewInfoFactory(controller, pieceIdImgGetter, uniqueImageResourceCount,
						boardAreaColors)
				: createProdGameBoardImgViewInfoFactory(controller, pieceIdImgGetter, uniqueImageResourceCount,
						boardAreaColors);
		final GameBoardPanel gameBoardPanel = gameBoardImgViewInfoFactory.getKey();
		final ImageLoadingImageViewInfoFactory imgViewInfoFactory = gameBoardImgViewInfoFactory.getValue();

		final SortedMap<Integer, ImageVisualizationInfo.Datum> imgVisualizationInfoDataById = new TreeMap<>();

		for (final ListIterator<ImageVisualizationInfo.Datum> imgVizInfoDataIter = imgVizInfoData
				.listIterator(); imgVizInfoDataIter.hasNext();) {
			final int id = imgVizInfoDataIter.nextIndex();
			ImageVisualizationInfo.Datum imgVisualizationInfoDatum = imgVizInfoDataIter.next();
			final ImageVisualizationInfo.Datum oldVizInfo = imgVisualizationInfoDataById.put(id,
					imgVisualizationInfoDatum);
			assert oldVizInfo == null;

			final Entry<ImageViewInfo, Image> imgViewInfoDatum = imgViewInfoFactory.apply(imgVisualizationInfoDatum);
			final boolean wasAdded = pieceImgs.add(imgViewInfoDatum.getValue());
			assert wasAdded;
		}
		imgVizInfoWriter.accept(imgVisualizationInfoDataById.entrySet().iterator());

		// http://stackoverflow.com/a/1936582/1391325
		final Dimension screenSize = gameBoardPanel.getToolkit().getScreenSize();
		LOGGER.debug("Setting maximum component size to {}.", screenSize);
		final int shortestScreenLength = (int) (Math.min(screenSize.width, screenSize.height) * 0.8);
		final Dimension preferredSize = new Dimension(shortestScreenLength, shortestScreenLength);
		final PatternMoveFactory moveFactory = new PatternMoveFactory(rnd, controller.getModel(), 4);
		controller.getListeners().add(moveFactory);
		final GameViewFrame view = new GameViewFrame(gameBoardPanel, controller, moveFactory, closeHook, preferredSize);
		view.setTitle(title);
		// gameViewFrame.setJMenuBar(createMenuBar(gameViewFrame));
		view.setMaximumSize(screenSize);

		view.pack();

		final Point viewLocation = new Point(viewCenterpoint.x - view.getWidth() / 2,
				viewCenterpoint.y - view.getHeight() / 2);
		view.setLocation(viewLocation);
		view.setVisible(true);

	}

	private Entry<GameBoardPanel, ImageLoadingImageViewInfoFactory> createDebugGameBoardImgViewInfoFactory(
			final Controller controller, final IntFunction<? extends Image> pieceIdImageFactory,
			final int uniqueImageResourceCount, final Map<BoardArea, Color> boardAreaColors) {
		final GameBoardPanel gameBoardPanel = new GameBoardPanel(controller.getModel(), pieceIdImageFactory, controller,
				boardAreaColors.get(BoardArea.HIGHLIGHT), screenshotLogger, backgroundJobService, true);
		gameBoardPanel.setBackground(boardAreaColors.get(BoardArea.BACKGROUND));
		final OpaqueTransparencyReplacementImageFilter imgFilter = new OpaqueTransparencyReplacementImageFilter(128);
		final BiFunction<Image, Toolkit, Image> tranparencyFilterer = (img, toolkit) -> {
			return toolkit.createImage(new FilteredImageSource(img.getSource(), imgFilter));
		};
		final ImageLoadingImageViewInfoFactory imgViewInfoFactory = new ImageLoadingImageViewInfoFactory(
				gameBoardPanel.getToolkit(), tranparencyFilterer,
				Maps.newHashMapWithExpectedSize(uniqueImageResourceCount));
		return new MutablePair<>(gameBoardPanel, imgViewInfoFactory);
	}

	private Entry<GameBoardPanel, ImageLoadingImageViewInfoFactory> createProdGameBoardImgViewInfoFactory(
			final Controller controller, final IntFunction<? extends Image> pieceIdImageFactory,
			final int uniqueImageResourceCount, final Map<BoardArea, Color> boardAreaColors) {
		final GameBoardPanel gameBoardPanel = new GameBoardPanel(controller.getModel(), pieceIdImageFactory, controller,
				boardAreaColors.get(BoardArea.HIGHLIGHT), screenshotLogger, backgroundJobService);
		gameBoardPanel.setBackground(boardAreaColors.get(BoardArea.BACKGROUND));
		final ImageLoadingImageViewInfoFactory imgViewInfoFactory = new ImageLoadingImageViewInfoFactory(
				gameBoardPanel.getToolkit(), DEFAULT_POST_COLORING_IMG_TRANSFORMER,
				Maps.newHashMapWithExpectedSize(uniqueImageResourceCount));
		return new MutablePair<>(gameBoardPanel, imgViewInfoFactory);
	}
}