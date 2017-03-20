/*
 *  This file is part of client.
 *
 *  client is free software: you can redistribute it and/or modify
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
import java.awt.Image;
import java.awt.Toolkit;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Random;
import java.util.function.BiFunction;
import java.util.function.Function;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Maps;

import se.kth.speech.Matrix;
import se.kth.speech.SpatialMap;
import se.kth.speech.SpatialMatrix;
import se.kth.speech.coin.tangrams.RandomModelPopulator;
import se.kth.speech.coin.tangrams.content.ImageLoadingImageViewInfoFactory;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 2 Mar 2017
 *
 */
final class GameBoardPanelFactory implements Function<Collection<ImageVisualizationInfo>, GameBoardPanel<Integer>> {

	private static class PositionGridSizeSummary {
		private final List<Integer> commonDivisors;

		private final int[] maxImgGridSize;

		private final int[] minImgGridSize;

		private final int totalImgGridArea;

		private PositionGridSizeSummary(final int[] minImgGridSize, final int[] maxImgGridSize,
				final int totalImgGridArea, final List<Integer> commonDivisors) {
			this.minImgGridSize = minImgGridSize;
			this.maxImgGridSize = maxImgGridSize;
			this.totalImgGridArea = totalImgGridArea;
			this.commonDivisors = commonDivisors;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.lang.Object#toString()
		 */
		@Override
		public String toString() {
			final StringBuilder builder = new StringBuilder();
			builder.append("PositionGridSizeSummary [minImgGridSize=");
			builder.append(Arrays.toString(minImgGridSize));
			builder.append(", maxImgGridSize=");
			builder.append(Arrays.toString(maxImgGridSize));
			builder.append(", totalImgGridArea=");
			builder.append(totalImgGridArea);
			builder.append(", commonDivisors=");
			builder.append(commonDivisors);
			builder.append("]");
			return builder.toString();
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

	private static final Logger LOGGER = LoggerFactory.getLogger(GameBoardPanelFactory.class);

	private static int[] createPositionGridSize(final PositionGridSizeSummary posGridSizeSummary,
			final Dimension maxBoardSize, final double occupiedGridArea) {
		// NOTE: "rows" in the matrix go top-bottom and "cols" go
		// left-right
		int[] result = null;
		final List<Integer> commonDivisors = posGridSizeSummary.commonDivisors;
		LOGGER.info("Trying common divisors {} for board dimensions {}.", commonDivisors, maxBoardSize);
		for (final ListIterator<Integer> imgCommonDivisorIter = commonDivisors
				.listIterator(posGridSizeSummary.commonDivisors.size()); imgCommonDivisorIter.hasPrevious();) {
			final int nextGreatestCommonDivisor = imgCommonDivisorIter.previous();
			if (maxBoardSize.height % nextGreatestCommonDivisor != 0) {
				LOGGER.info("Board height ({}) not divisible by {}.", maxBoardSize.height, nextGreatestCommonDivisor);
			} else {
				final int rows = maxBoardSize.height / nextGreatestCommonDivisor;
				if (maxBoardSize.width % nextGreatestCommonDivisor != 0) {
					LOGGER.info("Board width ({}) not divisible by {}.", maxBoardSize.width, nextGreatestCommonDivisor);
				} else {
					final int cols = maxBoardSize.width / nextGreatestCommonDivisor;
					LOGGER.info("Trying to size the board using a grid of size {}*{}.", rows, cols);
					if (rows < posGridSizeSummary.maxImgGridSize[0]) {
						LOGGER.info("Too few rows ({}) to accommodate biggest image (with a row count of {}).", rows,
								posGridSizeSummary.maxImgGridSize[0]);
					} else if (cols < posGridSizeSummary.maxImgGridSize[1]) {
						LOGGER.info("Too few columns ({}) to accommodate biggest image (with a column count of {}).",
								cols, posGridSizeSummary.maxImgGridSize[1]);
					} else {
						final int totalBoardGridArea = rows * cols;
						final int allowedOccupiedBoardArea = (int) (totalBoardGridArea * occupiedGridArea);
						if (allowedOccupiedBoardArea < posGridSizeSummary.totalImgGridArea) {
							LOGGER.info(
									"Too few grid cells ({} = {} * {}) to accommodate all images (required minimum of {}).",
									new Object[] { allowedOccupiedBoardArea, totalBoardGridArea, occupiedGridArea,
											posGridSizeSummary.totalImgGridArea, });
						} else {
							LOGGER.info("Found valid board size {}*{} (with a common divisor of {}).",
									new Object[] { rows, cols, nextGreatestCommonDivisor });
							result = new int[] { rows, cols };
							break;
						}
					}
				}
			}

		}
		if (result == null) {
			throw new IllegalArgumentException("Could not find a valid board size.");
		}
		return result;
	}

	private static Dimension estimatePreferredBoardSize(final Toolkit toolkit) {
		// http://stackoverflow.com/a/1936582/1391325
		final Dimension screenSize = toolkit.getScreenSize();
		final int shortestScreenLength = (int) (Math.min(screenSize.width, screenSize.height) * 0.75);
		return new Dimension(shortestScreenLength, shortestScreenLength);
	}

	private static boolean isDimensionDivisibleIntoGrid(final Dimension dim, final Matrix<?> matrix) {
		final int[] matrixDims = matrix.getDimensions();
		return dim.getHeight() % matrixDims[0] == 0 && dim.getWidth() % matrixDims[1] == 0;
	}

	private Dimension boardSize;

	private int[] gridSize;

	private final double occupiedGridArea;

	private BiFunction<? super Image, ? super Toolkit, ? extends Image> postColoringImgTransformer = DEFAULT_POST_COLORING_IMG_TRANSFORMER;

	private final Random rnd;

	private int uniqueImgResourceCount = -1;

	private final boolean allowFailedPlacements;

	/**
	 *
	 */
	GameBoardPanelFactory(final Random rnd, final double occupiedGridArea, final boolean allowFailedPlacements) {
		this.rnd = rnd;
		this.occupiedGridArea = occupiedGridArea;
		this.allowFailedPlacements = allowFailedPlacements;
		postColoringImgTransformer = DEFAULT_POST_COLORING_IMG_TRANSFORMER;
	}

	@Override
	public GameBoardPanel<Integer> apply(final Collection<ImageVisualizationInfo> imgVisualizationInfoData) {
		final int expectedPieceCount = uniqueImgResourceCount < 0 ? imgVisualizationInfoData.size()
				: uniqueImgResourceCount;
		GameBoardPanel<Integer> result;
		if (gridSize == null) {
			throw new UnsupportedOperationException("Auto grid sizing not (yet) available");
		} else {
			result = createWithDefinedGridSize(gridSize, imgVisualizationInfoData, expectedPieceCount);
		}

		return result;
	}

	/**
	 * @param boardSize
	 *            the boardSize to set
	 */
	public void setBoardSize(final Dimension boardSize) {
		this.boardSize = boardSize;
	}

	/**
	 * @param gridSize
	 *            the gridSize to set
	 */
	public void setGridSize(final int[] gridSize) {
		this.gridSize = gridSize;
	}

	// private GameBoardPanel createWithAutoGridSize(final
	// Collection<ImageVisualizationInfo> imgVisualizationInfoData, final int
	// expectedPieceCount) {
	// final Map<ImageViewInfo, Integer> pieceIds =
	// Maps.newHashMapWithExpectedSize(expectedPieceCount);
	// final GameBoardPanel result = new GameBoardPanel(pieceIds::get,
	// expectedPieceCount);
	// Toolkit toolkit = result.getToolkit();
	// final Map<ImageViewInfo, Image> pieceImgs =
	// createImageViewInfoMap(imgVisualizationInfoData, expectedPieceCount,
	// toolkit, postColoringImgTransformer);
	// final PositionGridSizeSummary posGridSizeSummary =
	// createPositionGridSizeSummary(pieceImgs.keySet().iterator());
	// LOGGER.info("Position grid size summary: {}", posGridSizeSummary);
	// final Dimension boardSize = this.boardSize == null ?
	// estimatePreferredBoardSize(toolkit) : this.boardSize;
	// final int[] gridSize = createPositionGridSize(posGridSizeSummary,
	// boardSize, occupiedGridArea);
	// final Collection<ImageViewInfo> pieces = pieceImgs.keySet();
	//
	//
	// final Function<ImageViewInfo, Integer> incrementingPieceIdGetter = piece
	// -> pieceIds.computeIfAbsent(piece,
	// k -> pieceIds.size());
	// final RandomMatrixPositionFiller<Integer> matrixFiller =
	// new RandomMatrixPositionFiller<>(
	// posMatrix, incrementingPieceIdGetter, rnd, piecePosMatrixSizeFactory);
	// matrixFiller.apply(pieces);
	// return result;
	// }

	/**
	 * @param postColoringImgTransformer
	 *            the postColoringImgTransformer to set
	 */
	public void setPostColoringImgTransformer(
			final BiFunction<? super Image, ? super Toolkit, ? extends Image> postColoringImgTransformer) {
		this.postColoringImgTransformer = postColoringImgTransformer;
	}

	/**
	 * @param uniqueImgResourceCount
	 *            the uniqueImgResourceCount to set
	 */
	public void setUniqueImgResourceCount(final int uniqueImgResourceCount) {
		this.uniqueImgResourceCount = uniqueImgResourceCount;
	}

	private GameBoardPanel<Integer> createWithDefinedGridSize(final int[] gridSize,
			final Collection<ImageVisualizationInfo> imgVisualizationInfoData, final int expectedPieceCount) {
		LOGGER.info("Creating a position matrix of size {}.", gridSize);
		final SpatialMatrix<Integer> posMatrix = new SpatialMatrix<>(gridSize, new SpatialMap<>(expectedPieceCount));
		final GameBoardPanel<Integer> result = new GameBoardPanel<>(posMatrix, expectedPieceCount);
		final Toolkit toolkit = result.getToolkit();
		final Map<Integer, Image> pieceImgs = result.getPieceImgs();
		final ImageLoadingImageViewInfoFactory imgViewInfoFactory = new ImageLoadingImageViewInfoFactory(toolkit,
				postColoringImgTransformer, Maps.newHashMapWithExpectedSize(uniqueImgResourceCount));
		final RandomModelPopulator modelPopulator = new RandomModelPopulator(posMatrix, imgVisualizationInfoData,
				occupiedGridArea, allowFailedPlacements, expectedPieceCount, pieceImgs::put, imgViewInfoFactory);
		modelPopulator.accept(rnd);
		return result;
	}

}
