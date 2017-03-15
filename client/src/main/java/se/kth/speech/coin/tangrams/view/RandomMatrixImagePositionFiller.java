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

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Queue;
import java.util.Random;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.stream.IntStream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Maps;

import se.kth.speech.MutablePair;
import se.kth.speech.SpatialMap;
import se.kth.speech.SpatialMap.Region;
import se.kth.speech.SpatialMatrix;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 9 Mar 2017
 *
 */
final class RandomMatrixImagePositionFiller<I>
		implements Function<Collection<? extends ImageViewInfo>, SpatialMap<ImageViewInfo>> {

	private static final Logger LOGGER = LoggerFactory.getLogger(RandomMatrixImagePositionFiller.class);

	private final boolean allowFailedPlacements;

	private final BiFunction<ImageMatrixPositionInfo<I>, Integer, Integer> incrementingRemapper = (key, oldVal) -> {
		final Integer newVal;
		if (oldVal == null) {
			newVal = 1;
		} else {
			newVal = oldVal + 1;
		}
		return newVal;
	};

	private final int maxPlacementRetriesPerImg;

	private final int maxPlacements;

	private final Function<? super ImageViewInfo, I> pieceIdGetter;

	private final Function<? super ImageViewInfo, int[]> piecePosMatrixSizeFactory;

	private final SpatialMatrix<? super I, ImageViewInfo> posMatrix;

	private final Random rnd;

	RandomMatrixImagePositionFiller(final SpatialMatrix<? super I, ImageViewInfo> posMatrix,
			final Function<? super ImageViewInfo, I> pieceIdGetter, final Random rnd, final int maxPlacements,
			final int maxPlacementRetriesPerImg, final boolean allowFailedPlacements,
			final Function<? super ImageViewInfo, int[]> piecePosMatrixSizeFactory) {
		this.posMatrix = posMatrix;
		this.pieceIdGetter = pieceIdGetter;
		this.rnd = rnd;
		this.maxPlacements = maxPlacements;
		this.maxPlacementRetriesPerImg = maxPlacementRetriesPerImg;
		this.allowFailedPlacements = allowFailedPlacements;
		this.piecePosMatrixSizeFactory = piecePosMatrixSizeFactory;
	}

	@Override
	public SpatialMap<ImageViewInfo> apply(final Collection<? extends ImageViewInfo> imgViewInfoData) {
		final SpatialMap<ImageViewInfo> result = new SpatialMap<>(imgViewInfoData.size());
		final Queue<ImageMatrixPositionInfo<I>> retryStack = new ArrayDeque<>();
		int placementCount = 0;
		{
			// Randomly place each image in the position matrix
			final Iterator<? extends ImageViewInfo> imgViewInfoDataIter = imgViewInfoData.iterator();
			while (imgViewInfoDataIter.hasNext() && placementCount < maxPlacements) {
				final ImageViewInfo imgViewInfoDatum = imgViewInfoDataIter.next();
				LOGGER.debug("Adding {}.", imgViewInfoDatum);
				final I pieceId = pieceIdGetter.apply(imgViewInfoDatum);
				final Entry<Region, Boolean> placementResult = placePieceRandomly(imgViewInfoDatum, pieceId, result);
				final Region pieceRegion = placementResult.getKey();
				if (placementResult.getValue()) {
					placementCount++;
				} else {
					LOGGER.debug("Cancelling placement of image because the target space is occupied.");
					final int[] piecePosMatrixSize = pieceRegion.getDimensions();
					LOGGER.debug("Adding image of grid dimensions {} to retry stack.",
							Arrays.toString(piecePosMatrixSize));
					retryStack.add(new ImageMatrixPositionInfo<>(pieceId, piecePosMatrixSize, imgViewInfoDatum));
				}
			}
		}

		{
			// Try to place images which didn't fit the first time
			final int estimatedNumberOfRetriedImgPlacements = retryStack.size() / 2;
			final Map<ImageMatrixPositionInfo<I>, Integer> retryCounter = Maps
					.newHashMapWithExpectedSize(estimatedNumberOfRetriedImgPlacements);
			final List<ImageVisualizationInfo> failedPlacements = new ArrayList<>(
					estimatedNumberOfRetriedImgPlacements);
			while (!retryStack.isEmpty() && placementCount < maxPlacements) {
				final ImageMatrixPositionInfo<I> imgPlacementInfo = retryStack.remove();
				final ImageViewInfo imgViewInfoDatum = imgPlacementInfo.getImgViewInfoDatum();
				final I pieceId = imgPlacementInfo.getPieceId();
				final Entry<Region, Boolean> placementResult = placePieceRandomly(imgViewInfoDatum, pieceId, result);
				if (placementResult.getValue()) {
					LOGGER.debug("Successfully placed piece \"{}\" after retrying.", pieceId);
					placementCount++;
				} else {
					final Integer tries = retryCounter.compute(imgPlacementInfo, incrementingRemapper);
					if (tries > maxPlacementRetriesPerImg) {
						failedPlacements.add(imgPlacementInfo.getImgViewInfoDatum().getVisualization());
					} else {
						retryStack.add(imgPlacementInfo);
					}
				}
			}

			// Check for failed placements
			if (failedPlacements.isEmpty()) {
				LOGGER.info("Successfully placed {} image(s).", placementCount);
			} else {
				final String errorMsg = createFailedPlacementErrorMsg(failedPlacements);
				if (allowFailedPlacements) {
					LOGGER.warn(errorMsg);
				} else {
					throw new IllegalArgumentException(errorMsg);
				}
			}
		}

		return result;
	}

	private String createFailedPlacementErrorMsg(final List<?> failedPlacements) {
		final String errorMsgPrefix = String.format(
				"%d image(s) could not be placed successfully after %d retries each:", failedPlacements.size(),
				maxPlacementRetriesPerImg);
		final StringBuilder sb = new StringBuilder(errorMsgPrefix.length() + failedPlacements.size() * 16);
		sb.append(errorMsgPrefix);
		for (final Object failedPlacement : failedPlacements) {
			sb.append(System.lineSeparator());
			sb.append(failedPlacement);
		}
		return sb.toString();
	}

	private SpatialMap.Region createRandomSpatialRegion(final int[] piecePosMatrixSize, final Random rnd) {
		final int[] posDims = posMatrix.getDimensions();
		final IntStream maxPossibleMatrixIdxs = IntStream.range(0, posDims.length)
				.map(i -> posDims[i] - piecePosMatrixSize[i] + 1);
		// Randomly pick a space in the matrix
		final int[] startMatrixIdx = maxPossibleMatrixIdxs.map(rnd::nextInt).toArray();
		final int[] endMatrixIdx = IntStream.range(0, startMatrixIdx.length)
				.map(i -> startMatrixIdx[i] + piecePosMatrixSize[i]).toArray();
		return createSpatialRegion(startMatrixIdx, endMatrixIdx);
	}

	private SpatialMap.Region createSpatialRegion(final int[] startMatrixIdx, final int[] endMatrixIdx) {
		return posMatrix.getRegion(startMatrixIdx[0], endMatrixIdx[0], startMatrixIdx[1], endMatrixIdx[1]);
	}

	private Entry<SpatialMap.Region, Boolean> placePieceRandomly(final ImageViewInfo piece, final I pieceId,
			final SpatialMap<ImageViewInfo> occupiedPositions) {
		// The number of rows and columns this image takes up in the
		// position matrix
		final int[] piecePosMatrixSize = piecePosMatrixSizeFactory.apply(piece);
		// Randomly pick a space in the matrix
		final SpatialMap.Region piecePosition = createRandomSpatialRegion(piecePosMatrixSize, rnd);
		final boolean success;
		if (occupiedPositions.isOccupied(piecePosition)) {
			success = false;
		} else {
			posMatrix.setPositionValues(piecePosition, pieceId);
			occupiedPositions.put(piece, piecePosition);
			success = true;
		}
		return new MutablePair<>(piecePosition, success);
	}

}
