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

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Image;
import java.awt.Toolkit;
import java.awt.image.FilteredImageSource;
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
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.MutablePair;
import se.kth.speech.awt.OpaqueTransparencyReplacementImageFilter;
import se.kth.speech.coin.tangrams.content.BoardArea;
import se.kth.speech.coin.tangrams.content.ImageLoadingImageViewInfoFactory;
import se.kth.speech.coin.tangrams.content.ImageViewInfo;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;
import se.kth.speech.coin.tangrams.game.GameplayController;
import se.kth.speech.coin.tangrams.game.PatternMoveFactory;

/**
 *
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since 16 Nov 2016
 *
 */
final class InteractiveGameViewFrameFactory implements Function<InteractiveGameViewFrameFactory.Parameters, InteractiveGameViewFrame> {

	public static final class Parameters {

		private final GameplayController controller;

		private final ImageVisualizationInfo imgVizInfo;

		private final Random rnd;

		public Parameters(final GameplayController controller, final ImageVisualizationInfo imgVizInfo,
				final Random rnd) {
			this.controller = controller;
			this.imgVizInfo = imgVizInfo;
			this.rnd = rnd;
		}
	}

	private static final BiFunction<Image, Toolkit, Image> DEFAULT_POST_COLORING_IMG_TRANSFORMER = new BiFunction<Image, Toolkit, Image>() {

		@Override
		public Image apply(final Image img, final Toolkit toolkit) {
			// Do nothing
			LOGGER.debug("Created instance {}.", img);
			return img;
		}

	};

	private static final Logger LOGGER = LoggerFactory.getLogger(InteractiveGameViewFrameFactory.class);

	private final boolean analysisEnabled;

	private final ExecutorService backgroundJobService;

	private final Consumer<? super Iterator<Entry<Integer, ImageVisualizationInfo.Datum>>> imgVizInfoWriter;

	private final BiConsumer<? super Component, ? super String> screenshotLogger;

	public InteractiveGameViewFrameFactory(final BiConsumer<? super Component, ? super String> screenshotLogger,
			final Consumer<? super Iterator<Entry<Integer, ImageVisualizationInfo.Datum>>> imgVizInfoWriter,
			final ExecutorService backgroundJobService, final boolean analysisEnabled) {
		this.screenshotLogger = screenshotLogger;
		this.imgVizInfoWriter = imgVizInfoWriter;
		this.backgroundJobService = backgroundJobService;
		this.analysisEnabled = analysisEnabled;
	}
	
	@Override
	public InteractiveGameViewFrame apply(final Parameters params) {
		LOGGER.debug("Creating view components.");
		final GameplayController controller = params.controller;
		// String matrixStrRepr = new
		// MatrixStringReprFactory().apply(model.getPositionMatrix());
		// System.out.println(matrixStrRepr);
		final ImageVisualizationInfo imgVizInfo = params.imgVizInfo;
		final List<ImageVisualizationInfo.Datum> imgVizInfoData = imgVizInfo.getData();
		final Map<BoardArea, Color> boardAreaColors = BoardArea.getDefaultBoardAreaColorMap();
		final Random rnd = params.rnd;
		final List<Image> pieceImgs = new ArrayList<>(imgVizInfoData.size());

		final Function<Integer, Image> pieceIdImgGetter = pieceImgs::get;
		final int uniqueImageResourceCount = imgVizInfo.getUniqueImageResourceCount();
		final Entry<InteractiveGameBoardPanel, ImageLoadingImageViewInfoFactory> gameBoardImgViewInfoFactory = analysisEnabled
				? createDebugGameBoardImgViewInfoFactory(controller, pieceIdImgGetter, uniqueImageResourceCount,
						boardAreaColors)
				: createProdGameBoardImgViewInfoFactory(controller, pieceIdImgGetter, uniqueImageResourceCount,
						boardAreaColors);
		final InteractiveGameBoardPanel gameBoardPanel = gameBoardImgViewInfoFactory.getKey();
		final ImageLoadingImageViewInfoFactory imgViewInfoFactory = gameBoardImgViewInfoFactory.getValue();

		final SortedMap<Integer, ImageVisualizationInfo.Datum> imgVisualizationInfoDataById = new TreeMap<>();

		for (final ListIterator<ImageVisualizationInfo.Datum> imgVizInfoDataIter = imgVizInfoData
				.listIterator(); imgVizInfoDataIter.hasNext();) {
			final ImageVisualizationInfo.Datum imgVisualizationInfoDatum = imgVizInfoDataIter.next();
			// Entity IDs are 1-indexed
			final int id = imgVizInfoDataIter.nextIndex();
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
		final int shortestScreenLength = Math.toIntExact(Math.round(Math.floor(Math.min(screenSize.width, screenSize.height) * 0.8f)));
		final Dimension preferredSize = new Dimension(shortestScreenLength, shortestScreenLength);
		final PatternMoveFactory moveFactory = new PatternMoveFactory(rnd, controller.getModel(), 4);
		controller.getListeners().add(moveFactory);
		return new InteractiveGameViewFrame(gameBoardPanel, controller, moveFactory, preferredSize);
	}

	private Entry<InteractiveGameBoardPanel, ImageLoadingImageViewInfoFactory> createDebugGameBoardImgViewInfoFactory(
			final GameplayController controller, final Function<? super Integer, ? extends Image> pieceIdImageFactory,
			final int uniqueImageResourceCount, final Map<BoardArea, Color> boardAreaColors) {
		final InteractiveGameBoardPanel gameBoardPanel = new InteractiveGameBoardPanel(controller.getModel(),
				pieceIdImageFactory, controller, boardAreaColors.get(BoardArea.HIGHLIGHT), screenshotLogger,
				backgroundJobService, true);
		gameBoardPanel.setBackground(boardAreaColors.get(BoardArea.BACKGROUND));
		final OpaqueTransparencyReplacementImageFilter imgFilter = new OpaqueTransparencyReplacementImageFilter(128);
		final BiFunction<Image, Toolkit, Image> tranparencyFilterer = (img, toolkit) -> {
			return toolkit.createImage(new FilteredImageSource(img.getSource(), imgFilter));
		};
		final ImageLoadingImageViewInfoFactory imgViewInfoFactory = new ImageLoadingImageViewInfoFactory(
				gameBoardPanel.getToolkit(), tranparencyFilterer, uniqueImageResourceCount);
		return new MutablePair<>(gameBoardPanel, imgViewInfoFactory);
	}

	private Entry<InteractiveGameBoardPanel, ImageLoadingImageViewInfoFactory> createProdGameBoardImgViewInfoFactory(
			final GameplayController controller, final Function<? super Integer, ? extends Image> pieceIdImageFactory,
			final int uniqueImageResourceCount, final Map<BoardArea, Color> boardAreaColors) {
		final InteractiveGameBoardPanel gameBoardPanel = new InteractiveGameBoardPanel(controller.getModel(),
				pieceIdImageFactory, controller, boardAreaColors.get(BoardArea.HIGHLIGHT), screenshotLogger,
				backgroundJobService);
		gameBoardPanel.setBackground(boardAreaColors.get(BoardArea.BACKGROUND));
		final ImageLoadingImageViewInfoFactory imgViewInfoFactory = new ImageLoadingImageViewInfoFactory(
				gameBoardPanel.getToolkit(), DEFAULT_POST_COLORING_IMG_TRANSFORMER, uniqueImageResourceCount);
		return new MutablePair<>(gameBoardPanel, imgViewInfoFactory);
	}
}