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
import java.io.IOException;
import java.io.UncheckedIOException;
import java.net.URL;
import java.util.Collection;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Maps;

import se.kth.speech.awt.ColorFilteredImageFactory;
import se.kth.speech.coin.tangrams.content.ImageDatum;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 2 Mar 2017
 *
 */
final class GameBoardPanelFactory implements Function<Collection<ImageDatum>, GameBoardPanel> {

	private static double RATIO_TOLERANCE = 0.05;

	private static final Logger LOGGER = LoggerFactory.getLogger(GameBoardPanelFactory.class);

	private static final ColorFilteredImageFactory COLORED_IMG_FACTORY = new ColorFilteredImageFactory();

	/**
	 * @see <a href=
	 *      "http://stackoverflow.com/a/4009247/1391325">StackOverflow</a>
	 * @param a
	 * @param b
	 * @return
	 */
	public static int GCD(final int a, final int b) {
		if (b == 0) {
			return a;
		}
		return GCD(b, a % b);
	}

	private static Image createInitialImage(final ImageDatum imgDatum) throws IOException {
		final URL resourceLoc = imgDatum.getResourceLoc();
		LOGGER.info("Reading image data at \"{}\".", resourceLoc);
		return COLORED_IMG_FACTORY.apply(resourceLoc, imgDatum.getColor());
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
		final Map<Image, ImageDatum> imageDataMap = imgData.stream().collect(Collectors.toMap(imgDatum -> {
			final Image result;
			try {
				result = createInitialImage(imgDatum);
			} catch (final IOException e) {
				throw new UncheckedIOException(e);
			}
			return result;
		}, Function.identity(), (u, v) -> {
			throw new IllegalStateException(String.format("Duplicate key %s", u));
		}, () -> Maps.newLinkedHashMapWithExpectedSize(imgData.size())));
		// final SortedMap<Image, ImageDatum> imageDataBySizeDescending = new
		// TreeMap<>((img1, img2)-> imageDataMap.get(img1).get);

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
