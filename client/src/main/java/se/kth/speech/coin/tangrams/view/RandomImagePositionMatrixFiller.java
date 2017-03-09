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

import java.awt.Image;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Queue;
import java.util.Random;
import java.util.Map.Entry;
import java.util.function.BiFunction;
import java.util.stream.IntStream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Maps;

import se.kth.speech.Matrix;
import se.kth.speech.SpatialMap;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 9 Mar 2017
 *
 */
final class RandomImagePositionMatrixFiller implements BiFunction<Matrix<? super Integer>, List<? extends Entry<? extends Image, ImageViewInfo>>,SpatialMap<Entry<? extends Image, ImageViewInfo>>> {
	
	private static class ImageMatrixPositionInfo {

		private final Entry<? extends Image, ImageViewInfo> imgViewInfoDatum;

		private final int pieceId;

		private final int[] piecePosMatrixSize;

		private ImageMatrixPositionInfo(final int pieceId, final int[] piecePosMatrixSize,
				final Entry<? extends Image, ImageViewInfo> imgViewInfoDatum) {
			this.pieceId = pieceId;
			this.piecePosMatrixSize = piecePosMatrixSize;
			this.imgViewInfoDatum = imgViewInfoDatum;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.lang.Object#equals(java.lang.Object)
		 */
		@Override
		public boolean equals(final Object obj) {
			if (this == obj) {
				return true;
			}
			if (obj == null) {
				return false;
			}
			if (!(obj instanceof ImageMatrixPositionInfo)) {
				return false;
			}
			final ImageMatrixPositionInfo other = (ImageMatrixPositionInfo) obj;
			if (imgViewInfoDatum == null) {
				if (other.imgViewInfoDatum != null) {
					return false;
				}
			} else if (!imgViewInfoDatum.equals(other.imgViewInfoDatum)) {
				return false;
			}
			if (pieceId != other.pieceId) {
				return false;
			}
			if (!Arrays.equals(piecePosMatrixSize, other.piecePosMatrixSize)) {
				return false;
			}
			return true;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.lang.Object#hashCode()
		 */
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + (imgViewInfoDatum == null ? 0 : imgViewInfoDatum.hashCode());
			result = prime * result + pieceId;
			result = prime * result + Arrays.hashCode(piecePosMatrixSize);
			return result;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.lang.Object#toString()
		 */
		@Override
		public String toString() {
			final StringBuilder builder = new StringBuilder();
			builder.append("ImageMatrixPositionInfo [imgViewInfoDatum=");
			builder.append(imgViewInfoDatum);
			builder.append(", pieceId=");
			builder.append(pieceId);
			builder.append(", piecePosMatrixSize=");
			builder.append(Arrays.toString(piecePosMatrixSize));
			builder.append("]");
			return builder.toString();
		}
	}
	
	private static int[] createPosMatrixBoundsArray(final ImageViewInfo viewInfo) {
		final ImageViewInfo.RasterizationInfo rasterizationInfo = viewInfo.getRasterization();
		// NOTE: "rows" in the matrix go top-bottom and "cols" go left-right
		// The number of rows this image takes up in the
		// position matrix
		final int occupiedPosMatrixRowCount = rasterizationInfo.getHeight() / rasterizationInfo.getGcd();
		// The number of columns this image takes up in the
		// position matrix
		final int occupiedPosMatrixColCount = rasterizationInfo.getWidth() / rasterizationInfo.getGcd();
		LOGGER.debug("Calculated position grid size {}*{} for \"{}\".", new Object[] {
				viewInfo.getVisualization().getResourceLoc(), occupiedPosMatrixRowCount, occupiedPosMatrixColCount });

		return new int[] { occupiedPosMatrixRowCount, occupiedPosMatrixColCount };
	}

	
	private static SpatialMap.Region createRandomSpatialRegion(final int[] piecePosMatrixSize, final int[] matrixDims,
			final Random rnd) {
		final IntStream maxPossibleMatrixIdxs = IntStream.range(0, matrixDims.length)
				.map(i -> matrixDims[i] - piecePosMatrixSize[i]);
		// Randomly pick a space in the matrix
		final int[] startMatrixIdx = maxPossibleMatrixIdxs.map(rnd::nextInt).toArray();
		final int[] endMatrixIdx = IntStream.range(0, startMatrixIdx.length)
				.map(i -> startMatrixIdx[i] + piecePosMatrixSize[i]).toArray();
		return createSpatialRegion(startMatrixIdx, endMatrixIdx);
	}

	private static SpatialMap.Region createSpatialRegion(final int[] startMatrixIdx, final int[] endMatrixIdx) {
		return new SpatialMap.Region(startMatrixIdx[0], endMatrixIdx[0], startMatrixIdx[1], endMatrixIdx[1]);
	}

	private static <T> void setMatrixPositionValues(final Matrix<T> posMatrix, final SpatialMap.Region occupiedRegion,
			final T pieceId) {
		final ListIterator<List<T>> rowIter = posMatrix.rowIterator(occupiedRegion.getXLowerBound());
		for (int rowIdx = rowIter.nextIndex(); rowIdx < occupiedRegion.getXUpperBound(); rowIdx++) {
			final List<T> occupiedRow = rowIter.next();
			final ListIterator<T> rowCellIter = occupiedRow.listIterator(occupiedRegion.getYLowerBound());
			for (int colIdx = rowCellIter.nextIndex(); colIdx < occupiedRegion.getYUpperBound(); colIdx++) {
				final T nextOldPieceId = rowCellIter.next();
				assert nextOldPieceId == null;
				rowCellIter.set(pieceId);
			}
		}
	}
	
	private static final Logger LOGGER = LoggerFactory.getLogger(RandomImagePositionMatrixFiller.class);
	
	private static final BiFunction<ImageMatrixPositionInfo, Integer, Integer> INCREMENTING_REMAPPER = new BiFunction<ImageMatrixPositionInfo, Integer, Integer>() {

		/*
		 * (non-Javadoc)
		 *
		 * @see java.util.function.BiFunction#apply(java.lang.Object,
		 * java.lang.Object)
		 */
		@Override
		public Integer apply(final ImageMatrixPositionInfo key, final Integer oldVal) {
			final Integer newVal;
			if (oldVal == null) {
				newVal = 1;
			} else {
				newVal = oldVal + 1;
			}
			return newVal;
		}
	};
	
	private final boolean allowFailedPlacements;

	private final int maxPlacementRetriesPerImg;
	
	private final Random rnd;
	

	RandomImagePositionMatrixFiller(final Random rnd, final int maxPlacementRetriesPerImg, final boolean allowFailedPlacements) {
		this.rnd = rnd;
		this.maxPlacementRetriesPerImg = maxPlacementRetriesPerImg;
		this.allowFailedPlacements = allowFailedPlacements;
	}
	
	@Override
	public SpatialMap<Entry<? extends Image, ImageViewInfo>> apply(Matrix<? super Integer> posMatrix, final List<? extends Entry<? extends Image, ImageViewInfo>> imgViewInfoData) {
		final ListIterator<? extends Entry<? extends Image, ImageViewInfo>> imgViewInfoDataIter = imgViewInfoData.listIterator();
		final SpatialMap<Entry<? extends Image, ImageViewInfo>> result = new SpatialMap<>(imgViewInfoData.size());
		final int[] posDims = posMatrix.getDimensions();
		// Randomly place each image in the position matrix
		final Queue<ImageMatrixPositionInfo> retryStack = new ArrayDeque<>();
		while (imgViewInfoDataIter.hasNext()) {
			final int pieceId = imgViewInfoDataIter.nextIndex();
			final Entry<? extends Image, ImageViewInfo> imgViewInfoDatum = imgViewInfoDataIter.next();
			final ImageViewInfo viewInfo = imgViewInfoDatum.getValue();
			// The number of rows and columns this image takes up in the
			// position matrix
			final int[] piecePosMatrixSize = createPosMatrixBoundsArray(viewInfo);
			// Randomly pick a space in the matrix
			final SpatialMap.Region pieceRegion = createRandomSpatialRegion(piecePosMatrixSize, posDims, rnd);
			if (result.isOccupied(pieceRegion)) {
				LOGGER.debug("Cancelling placement of image because the target space is occupied.");
				retryStack.add(new ImageMatrixPositionInfo(pieceId, piecePosMatrixSize, imgViewInfoDatum));
			} else {
				setMatrixPositionValues(posMatrix, pieceRegion, pieceId);
				result.put(pieceRegion, imgViewInfoDatum);
			}

		}

		// Try to place images which didn't fit the first time
		final int estimatedNumberOfRetriedImgPlacements = retryStack.size() / 2;
		final Map<ImageMatrixPositionInfo, Integer> retryCounter = Maps
				.newHashMapWithExpectedSize(estimatedNumberOfRetriedImgPlacements);
		final List<ImageVisualizationInfo> failedPlacements = new ArrayList<>(estimatedNumberOfRetriedImgPlacements);
		while (!retryStack.isEmpty()) {
			final ImageMatrixPositionInfo imgPlacementInfo = retryStack.remove();
			final SpatialMap.Region imgRegion = createRandomSpatialRegion(imgPlacementInfo.piecePosMatrixSize, posDims,
					rnd);
			if (result.isOccupied(imgRegion)) {
				final Integer tries = retryCounter.compute(imgPlacementInfo, INCREMENTING_REMAPPER);
				if (tries > maxPlacementRetriesPerImg) {
					failedPlacements.add(imgPlacementInfo.imgViewInfoDatum.getValue().getVisualization());
				} else {
					retryStack.add(imgPlacementInfo);
				}
			} else {
				setMatrixPositionValues(posMatrix, imgRegion, imgPlacementInfo.pieceId);
				result.put(imgRegion, imgPlacementInfo.imgViewInfoDatum);
			}
		}

		if (failedPlacements.isEmpty()) {
			LOGGER.info("Successfully placed {} images.", imgViewInfoData.size());
		} else {
			final String errorMsg = createFailedPlacementErrorMsg(failedPlacements);
			if (allowFailedPlacements) {
				LOGGER.warn(errorMsg);
			} else {
				throw new IllegalArgumentException(errorMsg);
			}
		}

		return result;
	}
	
	private String createFailedPlacementErrorMsg(final List<?> failedPlacements) {
		final String errorMsgPrefix = String.format("%d image(s) could not be placed successfully after %d retries:",
				failedPlacements.size(), maxPlacementRetriesPerImg);
		final StringBuilder sb = new StringBuilder(errorMsgPrefix.length() + failedPlacements.size() * 16);
		sb.append(errorMsgPrefix);
		for (final Object failedPlacement : failedPlacements) {
			sb.append(System.lineSeparator());
			sb.append(failedPlacement);
		}
		return sb.toString();
	}


}
