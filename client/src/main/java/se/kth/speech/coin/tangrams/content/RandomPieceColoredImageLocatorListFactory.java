/*
 *  This file is part of se.kth.speech.coin.tangrams.client.
 *
 *  se.kth.speech.coin.tangrams.client is free software: you can redistribute it and/or modify
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
package se.kth.speech.coin.tangrams.content;

import java.awt.Color;
import java.awt.Image;
import java.net.URL;
import java.util.Collections;
import java.util.List;
import java.util.Map.Entry;
import java.util.Random;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 21 Feb 2017
 *
 */
final class RandomPieceColoredImageLocatorListFactory implements Function<Random, List<Entry<URL, Color>>> {

	private static final Logger LOGGER = LoggerFactory.getLogger(RandomPieceColoredImageLocatorListFactory.class);

	/**
	 * The {@link Function} used for creating random pairs of {@URL} image
	 * locators and {@link Color} instances.
	 */
	private final Function<? super Random, ? extends List<Entry<URL, Color>>> coloredImgLocatorFactory;

	RandomPieceColoredImageLocatorListFactory() {
		this(new RandomColoredImageLocatorFactory(IconImages.getIconImageResources())
				.andThen(stream -> stream.collect(Collectors.toList())));
	}

	/**
	 *
	 * @param coloredImgLocatorFactory
	 *            The {@link Function} used for creating random pairs of
	 *            {@URL} image locators and {@link Color} instances.
	 */
	RandomPieceColoredImageLocatorListFactory(
			final Function<? super Random, ? extends List<Entry<URL, Color>>> coloredImgLocatorFactory) {
		this.coloredImgLocatorFactory = coloredImgLocatorFactory;
	}

	/**
	 *
	 * @param rnd
	 *            The {@link Random} instance to use for generation.
	 * @return A new {@link Function} which returns a reference to a colored
	 *         {@link Image}, for a given {@link Integer} used as an image ID,
	 *         from zero at least until the provided unique image count.
	 */
	@Override
	public List<Entry<URL, Color>> apply(final Random rnd) {
		// First put all of the combinations in a list in order to pick from
		// all of them randomly
		final List<Entry<URL, Color>> result = coloredImgLocatorFactory.apply(rnd);
		LOGGER.debug("Created colored image locator list: {}", result);
		Collections.shuffle(result, rnd);
		return result;
	}

}
