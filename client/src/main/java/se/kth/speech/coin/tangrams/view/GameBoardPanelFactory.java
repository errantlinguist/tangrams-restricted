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
import java.util.Collection;
import java.util.EnumSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.function.Function;

import javax.imageio.ImageIO;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Maps;
import com.google.common.collect.Sets;

import se.kth.speech.MathDenominators;
import se.kth.speech.Matrix;
import se.kth.speech.MutablePair;
import se.kth.speech.coin.tangrams.content.ImageSize;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 2 Mar 2017
 *
 */
final class GameBoardPanelFactory implements Function<Collection<ImageVisualizationInfo>, GameBoardPanel> {

	private enum ImageOrientation {
		LANDSCAPE, PORTRAIT, SQUARE;

		private static ImageOrientation getOrientation(final int width, final int height) {
			final ImageOrientation result;
			if (width < height) {
				result = ImageOrientation.PORTRAIT;
			} else if (height < width) {
				result = ImageOrientation.LANDSCAPE;
			} else {
				result = ImageOrientation.SQUARE;
			}
			return result;
		}
	}

	private static class ImageRasterizationInfo {

		private final int gcd;

		private final int height;

		private final int width;

		private ImageRasterizationInfo(final int width, final int height, final int gcd) {
			this.width = width;
			this.height = height;
			this.gcd = gcd;
		}
	}

	private static class ImageViewInfo {
		private final ImageRasterizationInfo rasterization;

		private final ImageVisualizationInfo visualization;

		private ImageViewInfo(final ImageVisualizationInfo visualization, final ImageRasterizationInfo rasterization) {
			this.visualization = visualization;
			this.rasterization = rasterization;
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

	private static final Logger LOGGER = LoggerFactory.getLogger(GameBoardPanelFactory.class);

	private static final double RATIO_TOLERANCE = 0.05;

	private static String createImageValueTable(
			final Collection<? extends Entry<?, ? extends Entry<ImageRasterizationInfo, ?>>> namedImgValData) {
		final String errorMsgPrefix = "One or more images failed validation:" + System.lineSeparator()
				+ "PATH\tWIDTH\tHEIGHT\tGCD\tCOMMENT";
		final StringBuilder sb = new StringBuilder(errorMsgPrefix.length() + 16 * namedImgValData.size());
		sb.append(errorMsgPrefix);
		for (final Entry<?, ? extends Entry<ImageRasterizationInfo, ?>> namedImgValDatumComments : namedImgValData) {
			sb.append(System.lineSeparator());
			sb.append(namedImgValDatumComments.getKey());
			sb.append('\t');
			final Entry<ImageRasterizationInfo, ?> imgValDatumComments = namedImgValDatumComments.getValue();
			final ImageRasterizationInfo imgVisualizationInfoDatum = imgValDatumComments.getKey();
			sb.append(imgVisualizationInfoDatum.width);
			sb.append('\t');
			sb.append(imgVisualizationInfoDatum.height);
			sb.append('\t');
			sb.append(imgVisualizationInfoDatum.gcd);
			sb.append('\t');
			sb.append(imgValDatumComments.getValue());
		}
		return sb.toString();
	}

	private static Map<BufferedImage, ImageViewInfo> createImageViewInfoMap(
			final Collection<ImageVisualizationInfo> imgVisualizationInfoData, final SizeValidator validator)
			throws IOException {
		// Use linked map in order to preserve iteration order in provided
		// sequence
		final Map<BufferedImage, ImageViewInfo> result = Maps
				.newLinkedHashMapWithExpectedSize(imgVisualizationInfoData.size());

		final Map<URL, Entry<ImageRasterizationInfo, Set<SizeValidator.ValidationComment>>> badImgs = Maps
				.newHashMapWithExpectedSize(0);
		for (final ImageVisualizationInfo imgVisualizationInfoDatum : imgVisualizationInfoData) {
			final URL imgResourceLoc = imgVisualizationInfoDatum.getResourceLoc();
			final BufferedImage initialImg = ImageIO.read(imgResourceLoc);

			{
				// Size/aspect ratio calculation
				final int width = initialImg.getWidth();
				final int height = initialImg.getHeight();
				LOGGER.debug("Image \"{}\" dims: {}*{}", new Object[] { imgResourceLoc, width, height });
				final int imgGcd = MathDenominators.gcd(width, height);
				final ImageRasterizationInfo imgRasterizationInfo = new ImageRasterizationInfo(width, height, imgGcd);
				result.put(initialImg, new ImageViewInfo(imgVisualizationInfoDatum, imgRasterizationInfo));
				{
					// Validate image
					final Set<SizeValidator.ValidationComment> validationComments = validator.validate(width, height,
							imgGcd);
					if (!validationComments.isEmpty()) {
						badImgs.put(imgResourceLoc, new MutablePair<>(imgRasterizationInfo, validationComments));
					}
				}
			}

		}

		if (badImgs.isEmpty()) {
			return result;
		} else {
			throw new IllegalArgumentException(createImageValueTable(badImgs.entrySet()));
		}
	}

	private static Image createScaledImage(final Image origImg, final ImageSize size, final int lesserDimVal,
			final int maxDimLength, final ImageOrientation orientation) {
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

	private static Image scaleImageByLongerDimension(final BufferedImage origImg, final int longerDimVal) {
		final Image result;
		final ImageOrientation orientation = ImageOrientation.getOrientation(origImg.getWidth(), origImg.getHeight());
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
		final ImageOrientation orientation = ImageOrientation.getOrientation(origImg.getWidth(), origImg.getHeight());
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

	GameBoardPanelFactory() {

	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Function#apply(java.lang.Object)
	 */
	@Override
	public GameBoardPanel apply(final Collection<ImageVisualizationInfo> imgVisualizationInfoData) {
		final GameBoardPanel result;
		// The minimum accepted length of the shortest dimension for an image
		final int minDimLength = 300;
		final SizeValidator validator = new SizeValidator(minDimLength, 50, 50);
		try {
			final Map<BufferedImage, ImageViewInfo> imgViewInfoData = createImageViewInfoMap(imgVisualizationInfoData,
					validator);
			final Set<Integer> dimensionValues = Sets.newHashSetWithExpectedSize(imgViewInfoData.size() + 1);
			for (final ImageViewInfo viewInfo : imgViewInfoData.values()) {
				dimensionValues.add(viewInfo.rasterization.gcd);
			}
			// Get the GCD for all components in the view
			final int greatestCommonDenominator = MathDenominators.gcd(dimensionValues.iterator());
			LOGGER.debug("GCD for all components is {}.", greatestCommonDenominator);
			final Dimension boardSize = new Dimension(minDimLength * 5, minDimLength * 4);
			dimensionValues.add(boardSize.width);
			dimensionValues.add(boardSize.height);
			final Set<SizeValidator.ValidationComment> boardValidationComments = validator.validate(boardSize.width,
					boardSize.height, greatestCommonDenominator);
			if (boardValidationComments.isEmpty()) {
				final int posMatrixRows = boardSize.width / greatestCommonDenominator;
				final int posMatrixCols = boardSize.height / greatestCommonDenominator;
				LOGGER.info("Creating a position matrix of size {}*{}.", posMatrixRows, posMatrixCols);
				final Integer[] posMatrixBackingArray = new Integer[posMatrixRows * posMatrixCols];
				final Matrix<Integer> posMatrix = new Matrix<>(posMatrixBackingArray, posMatrixCols);
				// TODO Auto-generated method stub
				result = new GameBoardPanel(boardSize);
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

	private void getImageCoordinateSize(final int width, final int height) {
		final boolean isPortrait;
		final double ratio;
		if (width < height) {
			isPortrait = true;
			ratio = height / (double) width;
		} else {
			isPortrait = false;
			ratio = width / (double) height;
		}
		final long wholePart = (long) ratio;
		final double fractionPart = ratio - wholePart;

	}

}
