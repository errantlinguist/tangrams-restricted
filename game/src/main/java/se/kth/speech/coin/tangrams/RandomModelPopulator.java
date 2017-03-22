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
package se.kth.speech.coin.tangrams;

import java.awt.Image;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Random;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Maps;

import se.kth.speech.IntArrays;
import se.kth.speech.MathDivisors;
import se.kth.speech.MatrixStringReprFactory;
import se.kth.speech.RandomMatrixPositionFiller;
import se.kth.speech.SpatialMatrix;
import se.kth.speech.coin.tangrams.content.ImageSize;
import se.kth.speech.coin.tangrams.content.ImageViewInfo;
import se.kth.speech.coin.tangrams.content.ImageViewInfo.RasterizationInfo;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 3 Jan 2017
 *
 */
final class RandomModelPopulator implements Consumer<Random> {

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
			builder.append(']');
			return builder.toString();
		}

	}

	private static final Logger LOGGER = LoggerFactory.getLogger(RandomModelPopulator.class);

	private static final Function<ImageSize, Integer> IMAGE_SIZE_FACTORS;

	private static final Function<? super ImageViewInfo, int[]> PIECE_GRID_SIZE_FACTORY;

	static {
		IMAGE_SIZE_FACTORS = ImageSize.createDefaultImageSizeFactorMap()::get;
		PIECE_GRID_SIZE_FACTORY = imgViewInfo -> imgViewInfo.getGridSize(IMAGE_SIZE_FACTORS);
	}

	private static PositionGridSizeSummary createPositionGridSizeSummary(
			final Iterator<ImageViewInfo> imageViewInfoData) {
		final int[] maxImgGridSize = new int[] { Integer.MIN_VALUE, Integer.MIN_VALUE };
		final int[] minImgGridSize = new int[] { Integer.MAX_VALUE, Integer.MAX_VALUE };
		int totalImgGridArea = 0;
		final List<Integer> commonDivisors;
		{
			final ImageViewInfo imgViewInfoDatum = imageViewInfoData.next();
			final int[] imgGridSize = imgViewInfoDatum.getGridSize(IMAGE_SIZE_FACTORS);
			IntArrays.mutate(minImgGridSize, imgGridSize, Math::min);
			IntArrays.mutate(maxImgGridSize, imgGridSize, Math::max);
			final RasterizationInfo rasterInfo = imgViewInfoDatum.getRasterization();
			commonDivisors = MathDivisors.createCommonDivisorList(rasterInfo.getWidth(), rasterInfo.getHeight());
			final int imgGridArea = IntArrays.product(imgGridSize);
			totalImgGridArea += imgGridArea;
		}
		while (imageViewInfoData.hasNext()) {
			final ImageViewInfo imgViewInfoDatum = imageViewInfoData.next();
			final int[] imgGridSize = imgViewInfoDatum.getGridSize(IMAGE_SIZE_FACTORS);
			IntArrays.mutate(minImgGridSize, imgGridSize, Math::min);
			IntArrays.mutate(maxImgGridSize, imgGridSize, Math::max);
			final RasterizationInfo rasterInfo = imgViewInfoDatum.getRasterization();
			MathDivisors.removeNonDivisors(commonDivisors.iterator(), rasterInfo.getWidth(), rasterInfo.getHeight());
			final int imgGridArea = IntArrays.product(imgGridSize);
			totalImgGridArea += imgGridArea;
		}
		LOGGER.debug("Common divisors for all images are {}.", commonDivisors);
		return new PositionGridSizeSummary(minImgGridSize, maxImgGridSize, totalImgGridArea, commonDivisors);
	}

	private final Function<? super ImageVisualizationInfo, ? extends Entry<ImageViewInfo, ? extends Image>> imgViewInfoFactory;

	private final boolean allowFailedPlacements;

	private final Collection<ImageVisualizationInfo> imgVisualizationInfoData;

	private final int uniqueImgResourceCount;

	private final double occupiedGridArea;

	private final BiConsumer<? super Integer, ? super Image> pieceImgHook;

	private final SpatialMatrix<Integer> posMatrix;

	RandomModelPopulator(final SpatialMatrix<Integer> posMatrix,
			final Collection<ImageVisualizationInfo> imgVisualizationInfoData, final double occupiedGridArea,
			final boolean allowFailedPlacements, final int uniqueImgResourceCount,
			final BiConsumer<? super Integer, ? super Image> pieceImgHook,
			final Function<? super ImageVisualizationInfo, ? extends Entry<ImageViewInfo, ? extends Image>> imgViewInfoFactory) {
		this.posMatrix = posMatrix;
		this.imgVisualizationInfoData = imgVisualizationInfoData;
		this.occupiedGridArea = occupiedGridArea;
		this.uniqueImgResourceCount = uniqueImgResourceCount;
		this.allowFailedPlacements = allowFailedPlacements;
		this.pieceImgHook = pieceImgHook;
		this.imgViewInfoFactory = imgViewInfoFactory;
	}

	@Override
	public void accept(final Random rnd) {
		final List<? extends Entry<ImageViewInfo, ? extends Image>> imgViewInfoLoadedImgs = imgVisualizationInfoData
				.stream().map(imgViewInfoFactory)
				.collect(Collectors.toCollection(() -> new ArrayList<>(imgVisualizationInfoData.size())));
		// Add the mapping of image to piece ID to the mapping for the game
		// board panel. LinkedHashSet in order to preserve iteration order
		// across instances
		final Map<ImageViewInfo, Integer> pieceIds = Maps.newLinkedHashMapWithExpectedSize(uniqueImgResourceCount);
		imgViewInfoLoadedImgs.stream().forEach(imgViewInfoLoadedImg -> {
			final ImageViewInfo imgViewInfo = imgViewInfoLoadedImg.getKey();
			final Integer pieceId = pieceIds.computeIfAbsent(imgViewInfo, k -> pieceIds.size());
			final Image img = imgViewInfoLoadedImg.getValue();
			pieceImgHook.accept(pieceId, img);
		});
		final PositionGridSizeSummary posGridSizeSummary = createPositionGridSizeSummary(pieceIds.keySet().iterator());
		LOGGER.info("Position grid size summary: {}", posGridSizeSummary);

		fillMatrix(posMatrix, pieceIds.entrySet(), rnd);
		if (moreOccupiedSpaceThanExpected(posMatrix)) {
			throw new IllegalArgumentException(String.format(
					"Grid size of %s is not enough to hold %d pieces with the given occupied-space ratio of %f.",
					Arrays.toString(posMatrix.getDimensions()), pieceIds.size(), occupiedGridArea));
		} else {
			if (LOGGER.isDebugEnabled()) {
				final String matrixStrRepr = new MatrixStringReprFactory().apply(posMatrix.getPositionMatrix());
				LOGGER.debug("PIECE PLACEMENTS" + System.lineSeparator() + matrixStrRepr);
			}
		}
	}

	private void fillMatrix(final SpatialMatrix<Integer> posMatrix,
			final Collection<? extends Entry<ImageViewInfo, Integer>> pieceIds, final Random rnd) {
		final RandomMatrixPositionFiller<Integer, ImageViewInfo> matrixFiller = new RandomMatrixPositionFiller<>(
				posMatrix, rnd, PIECE_GRID_SIZE_FACTORY, allowFailedPlacements);
		matrixFiller.apply(pieceIds);
	}

	private boolean moreOccupiedSpaceThanExpected(final SpatialMatrix<Integer> posMatrix) {
		final double gridSize = posMatrix.getPositionMatrix().getValues().size();
		final double nonNullCells = posMatrix.getCells().filter(Objects::nonNull).count();
		final double occupiedCellRatio = nonNullCells / gridSize;
		LOGGER.info("Created matrix with {} occupied space.", occupiedCellRatio);
		return occupiedCellRatio > occupiedGridArea;
	}

}
