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
import java.util.List;
import java.util.Map.Entry;
import java.util.Random;
import java.util.function.Function;
import java.util.function.IntFunction;

import se.kth.speech.awt.ColorFilteredImageFactory;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 21 Feb 2017
 *
 */
public final class PieceIdImageFactoryFactory implements IntFunction<Function<Integer, Image>> {

	/**
	 * The {@link Random} instance to use for generation.
	 */
	private final Random rnd;

	private final Function<Random, List<Entry<URL, Color>>> coloredImgLocatorFactory;

	/**
	 *
	 * @param rnd
	 *            The {@link Random} instance to use for generation.
	 */
	public PieceIdImageFactoryFactory(final Random rnd) {
		this(rnd, new RandomPieceColoredImageLocatorListFactory());
	}

	/**
	 *
	 * @param rnd
	 *            The {@link Random} instance to use for generation.
	 */
	public PieceIdImageFactoryFactory(final Random rnd,
			final Function<Random, List<Entry<URL, Color>>> coloredImgLocatorFactory) {
		this.rnd = rnd;
		this.coloredImgLocatorFactory = coloredImgLocatorFactory;
	}

	/**
	 *
	 * @param uniqueColoredImgCount
	 *            The total number of unique colored images to create,
	 *            i.e.&nbsp; for two different images, each of which is rendered
	 *            in two different colors, this is <code>4</code>.
	 * @return A new {@link Function} which returns a reference to a colored
	 *         {@link Image}, for a given {@link Integer} used as an image ID,
	 *         from zero at least until the provided unique image count.
	 */
	@Override
	public Function<Integer, Image> apply(final int uniqueColoredImgCount) {
		final List<Entry<URL, Color>> coloredImgLocators = coloredImgLocatorFactory.apply(rnd);
		if (coloredImgLocators.size() < uniqueColoredImgCount) {
			throw new IllegalArgumentException(
					String.format("Not enough image-color combinations (%d) to satisfy supplied result amount of %d.",
							coloredImgLocators.size(), uniqueColoredImgCount));
		}
		return new ColorFilteredImageFactory(coloredImgLocators);
	}

}
