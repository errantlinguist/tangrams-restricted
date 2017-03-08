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
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.net.URL;
import java.util.Collection;
import java.util.HashMap;
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

	private static class ImageValues {
		private final int gcd;
		
		private final int height;
		
		private final int width;

		private ImageValues(final int width, final int height, final int gcd) {
			this.width = width;
			this.height = height;
			this.gcd = gcd;
		}
	}

	private static final Logger LOGGER = LoggerFactory.getLogger(GameBoardPanelFactory.class);

	private static final double RATIO_TOLERANCE = 0.05;

	private static BufferedImage createInitialImage(final ImageDatum imgDatum) throws IOException {
		final URL resourceLoc = imgDatum.getResourceLoc();
		LOGGER.debug("Reading image data at \"{}\".", resourceLoc);
		return ImageIO.read(resourceLoc);
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
		final Map<String, ImageValues> badImgs = new HashMap<>(1);
		try {
			for (final ImageDatum imgDatum : imgData) {
				final BufferedImage initialImg = createInitialImage(imgDatum);
				imageDataMap.put(initialImg, imgDatum);
				final int width = initialImg.getWidth();
				dimensionValues.add(width);
				final int height = initialImg.getHeight();
				dimensionValues.add(height);
				// System.out.println(String.format("width: %d height: %d",
				// width, height));
				final int imgGcd = MathDenominators.gcd(width, height);
				if (imgGcd < 2) {
					badImgs.put(imgDatum.getResourceLoc().toString(), new ImageValues(width, height, imgGcd));
				}
				final boolean isSquare;
				if (width < height) {
					isSquare = false;
				} else if (height < width) {
					isSquare = false;
				} else {
					isSquare = true;
				}

			}
		} catch (final IOException e) {
			throw new UncheckedIOException(e);
		}

		if (!badImgs.isEmpty()) {
			final String errorMsgPrefix = "GCD for one or images was bad:" + System.lineSeparator()
					+ "PATH\tWIDTH\tHEIGHT\tGCD";
			final StringBuilder sb = new StringBuilder(errorMsgPrefix.length() + 16 * badImgs.size());
			sb.append(errorMsgPrefix);
			for (final Entry<String, ImageValues> badImg : badImgs.entrySet()) {
				sb.append(System.lineSeparator());
				sb.append(badImg.getKey());
				sb.append('\t');
				final ImageValues imgVals = badImg.getValue();
				sb.append(imgVals.width);
				sb.append('\t');
				sb.append(imgVals.height);
				sb.append('\t');
				sb.append(imgVals.gcd);
			}
			throw new IllegalArgumentException(sb.toString());
		}

		final int boardWidth = 500;
		dimensionValues.add(boardWidth);
		final int boardHeight = 500;
		dimensionValues.add(boardHeight);
		final int greatestCommonDenominator = MathDenominators.gcd(dimensionValues.iterator());
		System.out.println("GCD: " + greatestCommonDenominator);
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
