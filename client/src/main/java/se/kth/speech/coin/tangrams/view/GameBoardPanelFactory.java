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
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.net.URL;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.EnumSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Queue;
import java.util.Random;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.function.IntSupplier;
import java.util.stream.IntStream;

import javax.imageio.ImageIO;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Maps;
import com.google.common.collect.Sets;

import se.kth.speech.MathDenominators;
import se.kth.speech.Matrix;
import se.kth.speech.MutablePair;
import se.kth.speech.SpatialMap;
import se.kth.speech.coin.tangrams.content.ImageSize;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 2 Mar 2017
 *
 */
final class GameBoardPanelFactory implements BiFunction<Collection<ImageVisualizationInfo>, Random, GameBoardPanel> {

	private static class ImageMatrixPositionInfo {

		private final Entry<BufferedImage, ImageViewInfo> imgViewInfoDatum;

		private final int pieceId;

		private final int[] piecePosMatrixSize;

		private ImageMatrixPositionInfo(final int pieceId, final int[] piecePosMatrixSize,
				final Entry<BufferedImage, ImageViewInfo> imgViewInfoDatum) {
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

	private static class SizeValidator {

		private enum ValidationComment {
			GCD_BELOW_MIN, GCD_NOT_MULTIPLE_OF_LCD, HEIGHT_BELOW_MIN, WIDTH_BELOW_MIN;
		}

		private final int lcd;

		private final int minDimLength;

		private final int minGcd;

		private SizeValidator(final int minDimLength, final int minGcd, final int lcd) {
			this.minDimLength = minDimLength;
			this.minGcd = minGcd;
			this.lcd = lcd;
		}

		private EnumSet<ValidationComment> validate(final int width, final int height, final int imgGcd) {
			final EnumSet<ValidationComment> result = EnumSet.noneOf(ValidationComment.class);
			if (width < minDimLength) {
				result.add(ValidationComment.WIDTH_BELOW_MIN);
			}
			if (height < minDimLength) {
				result.add(ValidationComment.HEIGHT_BELOW_MIN);
			}
			if (imgGcd < minGcd) {
				result.add(ValidationComment.GCD_BELOW_MIN);
			}
			if (imgGcd % lcd != 0) {
				result.add(ValidationComment.GCD_NOT_MULTIPLE_OF_LCD);
			}
			return result;
		}

	}

	private static final int IMG_SCALING_HINTS = Image.SCALE_SMOOTH;

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

	private static final Logger LOGGER = LoggerFactory.getLogger(GameBoardPanelFactory.class);

	private static void appendRowTableRepr(final Iterator<?> rowCellIter, final StringBuilder sb) {
		if (rowCellIter.hasNext()) {
			final Object first = rowCellIter.next();
			final String firstRepr = Objects.toString(first, "-");
			sb.append(firstRepr);

			while (rowCellIter.hasNext()) {
				sb.append('\t');
				final Object next = rowCellIter.next();
				final String nextRepr = Objects.toString(next, "-");
				sb.append(nextRepr);
			}
		}
	}

	private static String createImageInfoTable(
			final Collection<? extends Entry<?, ? extends Entry<ImageViewInfo.RasterizationInfo, ?>>> namedImgValData) {
		final String header = "PATH\tWIDTH\tHEIGHT\tGCD\tCOMMENT";
		final StringBuilder sb = new StringBuilder(header.length() + 16 * namedImgValData.size());
		sb.append(header);
		for (final Entry<?, ? extends Entry<ImageViewInfo.RasterizationInfo, ?>> namedImgValDatumComments : namedImgValData) {
			sb.append(System.lineSeparator());
			sb.append(namedImgValDatumComments.getKey());
			sb.append('\t');
			final Entry<ImageViewInfo.RasterizationInfo, ?> imgValDatumComments = namedImgValDatumComments.getValue();
			final ImageViewInfo.RasterizationInfo imgVisualizationInfoDatum = imgValDatumComments.getKey();
			sb.append(imgVisualizationInfoDatum.getWidth());
			sb.append('\t');
			sb.append(imgVisualizationInfoDatum.getHeight());
			sb.append('\t');
			sb.append(imgVisualizationInfoDatum.getGcd());
			sb.append('\t');
			sb.append(imgValDatumComments.getValue());
		}
		return sb.toString();
	}

	private static LinkedHashMap<BufferedImage, ImageViewInfo> createImageViewInfoMap(
			final Collection<ImageVisualizationInfo> imgVisualizationInfoData, final SizeValidator validator)
			throws IOException {
		// Use linked map in order to preserve iteration order in provided
		// sequence
		final LinkedHashMap<BufferedImage, ImageViewInfo> result = Maps
				.newLinkedHashMapWithExpectedSize(imgVisualizationInfoData.size());

		final Map<URL, Entry<ImageViewInfo.RasterizationInfo, Set<SizeValidator.ValidationComment>>> badImgs = Maps
				.newHashMapWithExpectedSize(0);
		for (final ImageVisualizationInfo imgVisualizationInfoDatum : imgVisualizationInfoData) {
			final URL imgResourceLoc = imgVisualizationInfoDatum.getResourceLoc();
			final BufferedImage initialImg = ImageIO.read(imgResourceLoc);

			{
				// Size/aspect ratio calculation
				final IntSupplier widthGetter = initialImg::getWidth;
				final IntSupplier heightGetter = initialImg::getHeight;
				final ImageViewInfo.RasterizationInfo imgRasterizationInfo = new ImageViewInfo.RasterizationInfo(
						widthGetter, heightGetter);
				result.put(initialImg, new ImageViewInfo(imgVisualizationInfoDatum, imgRasterizationInfo));
				{
					// Validate image
					final Set<SizeValidator.ValidationComment> validationComments = validator.validate(
							widthGetter.getAsInt(), heightGetter.getAsInt(),
							MathDenominators.gcd(widthGetter.getAsInt(), heightGetter.getAsInt()));
					if (!validationComments.isEmpty()) {
						badImgs.put(imgResourceLoc, new MutablePair<>(imgRasterizationInfo, validationComments));
					}
				}
			}

		}

		if (badImgs.isEmpty()) {
			return result;
		} else {
			throw new IllegalArgumentException("One or more images failed validation:" + System.lineSeparator()
					+ createImageInfoTable(badImgs.entrySet()));
		}
	}

	private static String createMatrixReprString(final Matrix<?> matrix) {
		final int cellCount = matrix.getValues().size();
		final StringBuilder sb = new StringBuilder(cellCount * 4);

		final ListIterator<? extends List<?>> rowIter = matrix.rowIterator();
		if (rowIter.hasNext()) {
			final List<?> first = rowIter.next();
			appendRowTableRepr(first.iterator(), sb);
			while (rowIter.hasNext()) {
				sb.append(System.lineSeparator());
				final List<?> next = rowIter.next();
				appendRowTableRepr(next.iterator(), sb);
			}
		}
		return sb.toString();
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

	private static Image createScaledImage(final Image origImg, final ImageSize size, final int lesserDimVal,
			final int maxDimLength, final ImageViewInfo.Orientation orientation) {
		final Image result;
		if (lesserDimVal < maxDimLength) {
			switch (orientation) {
			case PORTRAIT: {
				result = origImg.getScaledInstance(-1, maxDimLength, IMG_SCALING_HINTS);
				break;
			}
			default: {
				result = origImg.getScaledInstance(-1, maxDimLength, IMG_SCALING_HINTS);
				break;
			}
			}
		} else {
			// Just use the original, unscaled image
			result = origImg;
		}
		return result;
	}

	private static SpatialMap.Region createSpatialRegion(final int[] startMatrixIdx, final int[] endMatrixIdx) {
		return new SpatialMap.Region(startMatrixIdx[0], endMatrixIdx[0], startMatrixIdx[1], endMatrixIdx[1]);
	}

	private static Image scaleImageByLongerDimension(final BufferedImage origImg, final int longerDimVal) {
		final Image result;
		final ImageViewInfo.Orientation orientation = ImageViewInfo.Orientation.getOrientation(origImg.getWidth(),
				origImg.getHeight());
		switch (orientation) {
		case PORTRAIT: {
			result = origImg.getScaledInstance(-1, longerDimVal, IMG_SCALING_HINTS);
			break;
		}
		default: {
			result = origImg.getScaledInstance(longerDimVal, -1, IMG_SCALING_HINTS);
			break;
		}
		}
		return result;
	}

	private static Image scaleImageByShorterDimension(final BufferedImage origImg, final int shorterDimVal) {
		final Image result;
		final ImageViewInfo.Orientation orientation = ImageViewInfo.Orientation.getOrientation(origImg.getWidth(),
				origImg.getHeight());
		switch (orientation) {
		case PORTRAIT: {
			result = origImg.getScaledInstance(shorterDimVal, -1, IMG_SCALING_HINTS);
			break;
		}
		default: {
			result = origImg.getScaledInstance(-1, shorterDimVal, IMG_SCALING_HINTS);
			break;
		}
		}
		return result;
	}

	private static void setMatrixPositionValues(final Matrix<Integer> posMatrix, final SpatialMap.Region imgRegion,
			final Integer pieceId) {
		for (int rowIdx = imgRegion.getXLowerBound(); rowIdx < imgRegion.getXUpperBound(); rowIdx++) {
			final List<Integer> occupiedRow = posMatrix.getRow(rowIdx);
			for (int colIdx = imgRegion.getYLowerBound(); colIdx < imgRegion.getYUpperBound(); colIdx++) {
				final Integer oldImgId = occupiedRow.set(colIdx, pieceId);
				assert oldImgId == null;
			}
		}
	}

	private final boolean allowFailedPlacements;

	private final int maxPlacementRetriesPerImg;

	GameBoardPanelFactory(final int maxPlacementRetriesPerImg, final boolean allowFailedPlacements) {
		this.maxPlacementRetriesPerImg = maxPlacementRetriesPerImg;
		this.allowFailedPlacements = allowFailedPlacements;
	}

	@Override
	public GameBoardPanel apply(final Collection<ImageVisualizationInfo> imgVisualizationInfoData, final Random rnd) {
		final GameBoardPanel result;
		// The minimum accepted length of the shortest dimension for an image
		final int minDimLength = 300;
		final SizeValidator validator = new SizeValidator(minDimLength, 50, 50);
		try {
			final LinkedHashMap<BufferedImage, ImageViewInfo> imgViewInfoDataMap = createImageViewInfoMap(
					imgVisualizationInfoData, validator);
			// Create a list for assigning an ID (i.e. index) to each image
			final List<Entry<BufferedImage, ImageViewInfo>> imgViewInfoDataList = new ArrayList<>(
					imgViewInfoDataMap.entrySet());
			final Set<Integer> dimensionValues = Sets.newHashSetWithExpectedSize(imgViewInfoDataList.size() + 1);
			for (final Entry<BufferedImage, ImageViewInfo> imgViewInfoDatum : imgViewInfoDataList) {
				final ImageViewInfo viewInfo = imgViewInfoDatum.getValue();
				dimensionValues.add(viewInfo.getRasterization().getGcd());
			}
			final Dimension boardSize = new Dimension(minDimLength * 5, minDimLength * 4);
			dimensionValues.add(boardSize.width);
			dimensionValues.add(boardSize.height);
			// Get the GCD for all components in the view
			final int greatestCommonDenominator = MathDenominators.gcd(dimensionValues.iterator());
			LOGGER.debug("GCD for all components is {}.", greatestCommonDenominator);
			// Validate the size and GCD of all components, including the board
			// itself
			final Set<SizeValidator.ValidationComment> boardValidationComments = validator.validate(boardSize.width,
					boardSize.height, greatestCommonDenominator);
			if (boardValidationComments.isEmpty()) {
				// NOTE: "rows" in the matrix go top-bottom and "cols" go
				// left-right
				final int posMatrixRows = boardSize.height / greatestCommonDenominator;
				final int posMatrixCols = boardSize.width / greatestCommonDenominator;
				LOGGER.info("Creating a position matrix of size {}*{}.", posMatrixRows, posMatrixCols);
				final Integer[] posMatrixBackingArray = new Integer[posMatrixRows * posMatrixCols];
				final Matrix<Integer> posMatrix = new Matrix<>(posMatrixBackingArray, posMatrixCols);
				final SpatialMap<Entry<BufferedImage, ImageViewInfo>> imagePlacements = fillMatrix(imgViewInfoDataList,
						posMatrix, rnd);
				System.out.println("IMAGE PLACEMENTS");
				System.out.println(createMatrixReprString(posMatrix));
				result = new GameBoardPanel(boardSize, imgViewInfoDataList, posMatrix, imagePlacements);
			} else {
				throw new IllegalArgumentException(
						String.format("The board as a whole failed validation with dimensions %s; and GCD %d: %s",
								boardSize, greatestCommonDenominator, boardValidationComments));
			}

		} catch (final IOException e) {
			throw new UncheckedIOException(e);
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

	private SpatialMap<Entry<BufferedImage, ImageViewInfo>> fillMatrix(
			final List<Entry<BufferedImage, ImageViewInfo>> imgViewInfoData, final Matrix<Integer> posMatrix,
			final Random rnd) {
		final ListIterator<Entry<BufferedImage, ImageViewInfo>> imgViewInfoDataIter = imgViewInfoData.listIterator();
		final SpatialMap<Entry<BufferedImage, ImageViewInfo>> result = new SpatialMap<>(imgViewInfoData.size());
		final int[] posDims = posMatrix.getDimensions();
		// Randomly place each image in the position matrix
		final Queue<ImageMatrixPositionInfo> retryStack = new ArrayDeque<>();
		while (imgViewInfoDataIter.hasNext()) {
			final int imgId = imgViewInfoDataIter.nextIndex();
			final Entry<BufferedImage, ImageViewInfo> imgViewInfoDatum = imgViewInfoDataIter.next();
			final ImageViewInfo viewInfo = imgViewInfoDatum.getValue();
			// The number of rows and columns this image takes up in the
			// position matrix
			final int[] piecePosMatrixSize = createPosMatrixBoundsArray(viewInfo);
			// Randomly pick a space in the matrix
			final SpatialMap.Region imgRegion = createRandomSpatialRegion(piecePosMatrixSize, posDims, rnd);
			if (result.isOccupied(imgRegion)) {
				LOGGER.debug("Cancelling placement of image because the target space is occupied.");
				retryStack.add(new ImageMatrixPositionInfo(imgId, piecePosMatrixSize, imgViewInfoDatum));
			} else {
				setMatrixPositionValues(posMatrix, imgRegion, imgId);
				result.put(imgRegion, imgViewInfoDatum);
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

}
