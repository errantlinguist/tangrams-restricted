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
import java.io.IOException;
import java.io.UncheckedIOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Random;
import java.util.SortedMap;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Stream;

import com.github.errantlinguist.ClassProperties;

import se.kth.speech.MutablePair;

final class RandomColoredImageLocatorFactory implements Function<Random, Stream<Entry<URL, Color>>> {

	private static final int DEFAULT_IMG_COLOR_COUNT;

	private static final List<Color> DEFAULT_UNIQUE_IMG_COLORS = Arrays.asList(Color.RED, Color.YELLOW, Color.GREEN,
			Color.BLUE);

	static {
		try {
			final Properties props = ClassProperties.load(RandomColoredImageLocatorFactory.class);
			DEFAULT_IMG_COLOR_COUNT = Integer.parseInt(props.getProperty("color.count"));
		} catch (final IOException e) {
			throw new UncheckedIOException(e);
		}
	}

	/**
	 * The maximum number of different colors each image will be rendered in.
	 */
	private final int imgColorCount;

	private final SortedMap<String, URL> imgResources;

	private final List<Color> uniqueImgColors;

	/**
	 *
	 * @param imgResources
	 *            A {@link Map} of icon names to their corresponding image
	 *            {@link URL} resource locators.
	 * @param uniqueImgColors
	 *            An ordered sequence of the unique {@link Color} instances to
	 *            use for coloring the icon images.
	 * @param imgColorCount
	 *            The maximum number of different colors each image will be
	 *            rendered in.
	 */
	private RandomColoredImageLocatorFactory(final SortedMap<String, URL> imgResources,
			final List<Color> uniqueImgColors, final int imgColorCount) {
		if (uniqueImgColors.size() < imgColorCount) {
			throw new IllegalArgumentException(String.format(
					"Not enough colors supported (%d) in order to create the desired amount of image-color combinations (%d).",
					uniqueImgColors.size(), imgColorCount));
		}
		this.imgResources = imgResources;
		this.uniqueImgColors = uniqueImgColors;
		this.imgColorCount = imgColorCount;
	}

	/**
	 *
	 * @param imgResources
	 *            A {@link Map} of icon names to their corresponding image
	 *            {@link URL} resource locators.
	 */
	RandomColoredImageLocatorFactory(final SortedMap<String, URL> imgResources) {
		this(imgResources, DEFAULT_UNIQUE_IMG_COLORS, DEFAULT_IMG_COLOR_COUNT);
	}

	/**
	 *
	 * @param rnd
	 *            The {@link Random} instance to use for randomization.
	 * @return A {@link Stream} of {@link Entry} objects with the image resource
	 *         {@link URL} and respective {@link Color} to use for filtering it.
	 */
	@Override
	public Stream<Entry<URL, Color>> apply(final Random rnd) {
		// The colors in which each individual image will be rendered in
		final Map<String, List<Color>> namedImgColorLists = new HashMap<>(imgResources.size() + 1, 1.0f);
		final Iterator<Entry<String, URL>> imgResourceIter = imgResources.entrySet().iterator();
		while (imgResourceIter.hasNext()) {
			final Supplier<Color> rndColorGetter = () -> uniqueImgColors.get(rnd.nextInt(uniqueImgColors.size()));

			final Entry<String, URL> imgResource = imgResourceIter.next();
			final String imgName = imgResource.getKey();
			final List<Color> namedImgColors = new ArrayList<>(imgColorCount + 1);
			do {
				Color rndColor = rndColorGetter.get();
				// Ensure each color is added only once
				while (namedImgColors.contains(rndColor)) {
					rndColor = rndColorGetter.get();
				}
				namedImgColors.add(rndColor);
			} while (namedImgColors.size() < imgColorCount);
			namedImgColorLists.put(imgName, namedImgColors);
		}
		return namedImgColorLists.entrySet().stream().flatMap(namedImgColorList -> {
			final String imgName = namedImgColorList.getKey();
			return namedImgColorList.getValue().stream().map(color -> {
				final URL resourceLocator = imgResources.get(imgName);
				return new MutablePair<>(resourceLocator, color);
			});
		});
	}

}