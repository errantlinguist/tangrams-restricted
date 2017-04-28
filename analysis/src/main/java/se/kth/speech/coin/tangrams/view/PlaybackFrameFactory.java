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
import java.awt.Toolkit;
import java.awt.image.FilteredImageSource;
import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Random;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.function.BiFunction;
import java.util.function.Function;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.MutablePair;
import se.kth.speech.SpatialMatrix;
import se.kth.speech.SpatialRegion;
import se.kth.speech.awt.OpaqueTransparencyReplacementImageFilter;
import se.kth.speech.coin.tangrams.analysis.GameHistory;
import se.kth.speech.coin.tangrams.content.BoardArea;
import se.kth.speech.coin.tangrams.content.IconImages;
import se.kth.speech.coin.tangrams.content.ImageLoadingImageViewInfoFactory;
import se.kth.speech.coin.tangrams.content.ImageViewInfo;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;
import se.kth.speech.coin.tangrams.game.Controller;
import se.kth.speech.coin.tangrams.game.PatternMoveFactory;
import se.kth.speech.coin.tangrams.iristk.ImageVisualizationInfoUnmarshaller;

/**
 *
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 16 Nov 2016
 *
 */
final class PlaybackFrameFactory implements Function<PlaybackFrameFactory.Parameters, PlaybackFrame> {

	public static final class Parameters {

		private final Controller controller;

		private final GameHistory history;

		private final Random rnd;

		public Parameters(final Controller controller, final GameHistory history, final Random rnd) {
			this.controller = controller;
			this.history = history;
			this.rnd = rnd;
		}
	}

	private static class PlaybackGameBoardPanel extends AbstractGameBoardPanel {

		/**
		 *
		 */
		private static final long serialVersionUID = 5656752301161168246L;

		private final Controller controller;

		/**
		 * @param posMatrix
		 * @param pieceIdImageFactory
		 * @param highlightColor
		 */
		public PlaybackGameBoardPanel(final SpatialMatrix<Integer> posMatrix,
				final Function<? super Integer, ? extends Image> pieceIdImageFactory, final Color highlightColor,
				final Controller controller) {
			super(posMatrix, pieceIdImageFactory, highlightColor, true);
			this.controller = controller;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see se.kth.speech.coin.tangrams.view.AbstractGameBoardPanel#
		 * notifyNextMove(se.kth.speech.SpatialRegion,
		 * se.kth.speech.SpatialRegion, java.lang.Integer)
		 */
		@Override
		protected void notifyNextMove(final SpatialRegion source, final SpatialRegion target, final Integer pieceId) {
			LOGGER.debug("Notified of continue event.");
			controller.submitNextMove(source, target, pieceId);
		}

	}

	private static final ImageVisualizationInfoUnmarshaller IMG_INFO_UNMARSHALLER = new ImageVisualizationInfoUnmarshaller(
			IconImages.getImageResources()::get);

	private static final Logger LOGGER = LoggerFactory.getLogger(PlaybackFrameFactory.class);

	@Override
	public PlaybackFrame apply(final Parameters params) {
		LOGGER.debug("Creating view components.");
		final Controller controller = params.controller;
		// String matrixStrRepr = new
		// MatrixStringReprFactory().apply(model.getPositionMatrix());
		// System.out.println(matrixStrRepr);
		final GameHistory history = params.history;
		final ImageVisualizationInfo imgVizInfo = IMG_INFO_UNMARSHALLER
				.apply(history.getInitialState().getImageVisualizationInfoDescription());
		final List<ImageVisualizationInfo.Datum> imgVizInfoData = imgVizInfo.getData();
		final Map<BoardArea, Color> boardAreaColors = BoardArea.getDefaultBoardAreaColorMap();
		final Random rnd = params.rnd;
		final List<Image> pieceImgs = new ArrayList<>(imgVizInfoData.size());

		final Function<Integer, Image> pieceIdImgGetter = pieceImgs::get;
		final int uniqueImageResourceCount = imgVizInfo.getUniqueImageResourceCount();
		final Entry<AbstractGameBoardPanel, ImageLoadingImageViewInfoFactory> gameBoardImgViewInfoFactory = createDebugGameBoardImgViewInfoFactory(
				controller, pieceIdImgGetter, uniqueImageResourceCount, boardAreaColors);
		final AbstractGameBoardPanel gameBoardPanel = gameBoardImgViewInfoFactory.getKey();
		final ImageLoadingImageViewInfoFactory imgViewInfoFactory = gameBoardImgViewInfoFactory.getValue();

		final SortedMap<Integer, ImageVisualizationInfo.Datum> imgVisualizationInfoDataById = new TreeMap<>();

		for (final ListIterator<ImageVisualizationInfo.Datum> imgVizInfoDataIter = imgVizInfoData
				.listIterator(); imgVizInfoDataIter.hasNext();) {
			final int id = imgVizInfoDataIter.nextIndex();
			final ImageVisualizationInfo.Datum imgVisualizationInfoDatum = imgVizInfoDataIter.next();
			final ImageVisualizationInfo.Datum oldVizInfo = imgVisualizationInfoDataById.put(id,
					imgVisualizationInfoDatum);
			assert oldVizInfo == null;

			final Entry<ImageViewInfo, Image> imgViewInfoDatum = imgViewInfoFactory.apply(imgVisualizationInfoDatum);
			final boolean wasAdded = pieceImgs.add(imgViewInfoDatum.getValue());
			assert wasAdded;
		}
		// http://stackoverflow.com/a/1936582/1391325
		final Dimension screenSize = gameBoardPanel.getToolkit().getScreenSize();
		LOGGER.debug("Setting maximum component size to {}.", screenSize);
		final int shortestScreenLength = (int) (Math.min(screenSize.width, screenSize.height) * 0.8);
		final Dimension preferredSize = new Dimension(shortestScreenLength, shortestScreenLength);
		final PatternMoveFactory moveFactory = new PatternMoveFactory(rnd, controller.getModel(), 4);
		controller.getListeners().add(moveFactory);
		return new PlaybackFrame(gameBoardPanel, controller, moveFactory, preferredSize, history);
	}

	private Entry<AbstractGameBoardPanel, ImageLoadingImageViewInfoFactory> createDebugGameBoardImgViewInfoFactory(
			final Controller controller, final Function<? super Integer, ? extends Image> pieceIdImageFactory,
			final int uniqueImageResourceCount, final Map<BoardArea, Color> boardAreaColors) {
		final AbstractGameBoardPanel gameBoardPanel = new PlaybackGameBoardPanel(controller.getModel(),
				pieceIdImageFactory, boardAreaColors.get(BoardArea.HIGHLIGHT), controller);
		gameBoardPanel.setBackground(boardAreaColors.get(BoardArea.BACKGROUND));
		final OpaqueTransparencyReplacementImageFilter imgFilter = new OpaqueTransparencyReplacementImageFilter(128);
		final BiFunction<Image, Toolkit, Image> tranparencyFilterer = (img, toolkit) -> {
			return toolkit.createImage(new FilteredImageSource(img.getSource(), imgFilter));
		};
		final ImageLoadingImageViewInfoFactory imgViewInfoFactory = new ImageLoadingImageViewInfoFactory(
				gameBoardPanel.getToolkit(), tranparencyFilterer, uniqueImageResourceCount);
		return new MutablePair<>(gameBoardPanel, imgViewInfoFactory);
	}

}