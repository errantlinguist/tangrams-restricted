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
import se.kth.speech.coin.tangrams.content.ImageDatum;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 2 Mar 2017
 *
 */
final class GameBoardPanelFactory implements Function<Collection<ImageDatum>, GameBoardPanel> {

	private enum ImageOrientation {
		LANDSCAPE, PORTRAIT;
	}

	private static class ImageValueDatum {
		private final Set<?> comments;

		private final int gcd;

		private final int height;

		private final int width;

		private ImageValueDatum(final int width, final int height, final int gcd, final Set<?> comments) {
			this.width = width;
			this.height = height;
			this.gcd = gcd;
			this.comments = comments;
		}
	}

	private static class SizeValidator {

		private enum ValidationComment {
			BAD_GCD, HEIGHT_BELOW_MINIMUM, WIDTH_BELOW_MINIMUM;
		}

		private final int minDimLength;

		private SizeValidator(final int minDimLength) {
			this.minDimLength = minDimLength;
		}

		private Set<ValidationComment> validate(final int width, final int height, final int imgGcd) {
			final EnumSet<ValidationComment> result = EnumSet.noneOf(ValidationComment.class);
			if (width < minDimLength) {
				result.add(ValidationComment.WIDTH_BELOW_MINIMUM);
			}
			if (height < minDimLength) {
				result.add(ValidationComment.HEIGHT_BELOW_MINIMUM);
			}
			// final int imgGcd = MathDenominators.gcd(width, height);
			if (imgGcd < 2) {
				result.add(ValidationComment.BAD_GCD);
			}
			return result;
		}

	}

	private static final int IMG_SCALING_HINTS = Image.SCALE_SMOOTH;

	private static final Logger LOGGER = LoggerFactory.getLogger(GameBoardPanelFactory.class);

	private static final double RATIO_TOLERANCE = 0.05;

	private static String createImageValueTable(final Collection<? extends Entry<?, ImageValueDatum>> namedImgValData) {
		final String errorMsgPrefix = "One or more images failed validation:" + System.lineSeparator()
				+ "PATH\tWIDTH\tHEIGHT\tGCD\tCOMMENT";
		final StringBuilder sb = new StringBuilder(errorMsgPrefix.length() + 16 * namedImgValData.size());
		sb.append(errorMsgPrefix);
		for (final Entry<?, ImageValueDatum> namedImgValDatum : namedImgValData) {
			sb.append(System.lineSeparator());
			sb.append(namedImgValDatum.getKey());
			sb.append('\t');
			final ImageValueDatum imgValDatum = namedImgValDatum.getValue();
			sb.append(imgValDatum.width);
			sb.append('\t');
			sb.append(imgValDatum.height);
			sb.append('\t');
			sb.append(imgValDatum.gcd);
			sb.append('\t');
			sb.append(imgValDatum.comments);
		}
		return sb.toString();
	}

	private static Image createScaledImage(final Image origImg, final int lesserDimVal, final int minDimLength,
			final ImageOrientation orientation) {
		final Image result;
		if (lesserDimVal < minDimLength) {
			switch (orientation) {
			case PORTRAIT: {
				result = origImg.getScaledInstance(minDimLength, -1, IMG_SCALING_HINTS);
				break;
			}
			default: {
				result = origImg.getScaledInstance(-1, minDimLength, IMG_SCALING_HINTS);
				break;
			}
			}
		} else {
			// Just use the original, unscaled image
			result = origImg;
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
	public GameBoardPanel apply(final Collection<ImageDatum> imgData) {
		// Use linked map in order to preserve iteration order in provided
		// sequence
		final Map<BufferedImage, ImageDatum> imageDataMap = Maps.newLinkedHashMapWithExpectedSize(imgData.size());
		final Set<Integer> dimensionValues = Sets.newHashSetWithExpectedSize(imgData.size() + 1);
		final Map<URL, ImageValueDatum> badImgs = Maps.newHashMapWithExpectedSize(0);
		final int minDimLength = 300;
		final SizeValidator validator = new SizeValidator(minDimLength);
		try {
			for (final ImageDatum imgDatum : imgData) {
				final URL imgResourceLoc = imgDatum.getResourceLoc();
				final BufferedImage initialImg = ImageIO.read(imgResourceLoc);
				imageDataMap.put(initialImg, imgDatum);
				final int width = initialImg.getWidth();
				dimensionValues.add(width);
				final int height = initialImg.getHeight();
				dimensionValues.add(height);
				// System.out.println(String.format("width: %d height: %d",
				// width, height));
				final int imgGcd = MathDenominators.gcd(width, height);
				final Set<SizeValidator.ValidationComment> validationComments = validator.validate(width, height,
						imgGcd);
				if (!validationComments.isEmpty()) {
					badImgs.put(imgResourceLoc, new ImageValueDatum(width, height, imgGcd, validationComments));
				}
				{
					// Image scaling
					final ImageOrientation orientation;
					final int lesserDimVal;
					final boolean isSquare;
					if (width < height) {
						orientation = ImageOrientation.PORTRAIT;
						lesserDimVal = width;
						isSquare = false;
					} else if (height < width) {
						orientation = ImageOrientation.LANDSCAPE;
						lesserDimVal = height;
						isSquare = false;
					} else {
						orientation = ImageOrientation.LANDSCAPE;
						lesserDimVal = width;
						isSquare = true;
					}

					final Image scaledImg = createScaledImage(initialImg, lesserDimVal, minDimLength, orientation);
				}

			}
		} catch (final IOException e) {
			throw new UncheckedIOException(e);
		}

		if (!badImgs.isEmpty()) {
			throw new IllegalArgumentException(createImageValueTable(badImgs.entrySet()));
		}

		final int boardWidth = 500;
		dimensionValues.add(boardWidth);
		final int boardHeight = 500;
		dimensionValues.add(boardHeight);
		final int greatestCommonDenominator = MathDenominators.gcd(dimensionValues.iterator());
		// System.out.println("GCD: " + greatestCommonDenominator);
		// System.out.println("GCD(300,1000,2000): " +
		// MathDenominators.gcd(Arrays.asList(300,1000,200).iterator()));

		final Dimension boardSize = new Dimension(imageDataMap.size() * 100, imageDataMap.size() * 100);
		// TODO Auto-generated method stub
		final GameBoardPanel result = new GameBoardPanel(imageDataMap, boardSize);
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
