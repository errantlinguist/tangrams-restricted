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
package se.kth.speech.coin.tangrams.content;

import java.awt.Color;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Random;
import java.util.function.Function;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 7 Mar 2017
 *
 */
final class RandomPieceImageDataFactory implements Function<Random, Stream<ImageDatum>> {

	private static final List<ImageSize> DEFAULT_IMG_SIZES = Arrays.asList(ImageSize.values());

	private static final List<Color> DEFAULT_UNIQUE_IMG_COLORS = Arrays.asList(Color.RED, Color.YELLOW, Color.GREEN,
			Color.BLUE);

	private static final Logger LOGGER = LoggerFactory.getLogger(RandomPieceImageDataFactory.class);

	/**
	 * A mapping of icon names mapped to their corresponding image {@link URL}
	 * resource locators.
	 */
	private final Map<String, URL> imgResources;

	/**
	 * The maximum number of attributes an image may share with another image.
	 */
	private final int maxSharedAttrCount;

	/**
	 * An ordered sequence of the {@link ImageSize sizes} to present each image
	 * in. <strong>NOTE:</strong> This is ordered so that the iteration order of
	 * image data is stable across invocations.
	 */
	private final List<ImageSize> sizes;

	/**
	 * An ordered sequence of the unique {@link Color} instances to use for
	 * coloring the icon images. <strong>NOTE:</strong> This is ordered so that
	 * the iteration order of image data is stable across invocations.
	 */
	private final List<Color> uniqueImgColors;

	/**
	 *
	 * @param imgResources
	 *            A mapping of icon names mapped to their corresponding image
	 *            {@link URL} resource locators.
	 * @param maxSharedAttrCount
	 *            The maximum number of attributes an image may share with
	 *            another image.
	 */
	public RandomPieceImageDataFactory(final Map<String, URL> imgResources, final int maxSharedAttrCount) {
		this(imgResources, DEFAULT_UNIQUE_IMG_COLORS, DEFAULT_IMG_SIZES, maxSharedAttrCount);
	}

	/**
	 *
	 * @param imgResources
	 *            A mapping of icon names mapped to their corresponding image
	 *            {@link URL} resource locators.
	 * @param uniqueImgColors
	 *            An ordered sequence of the unique {@link Color} instances to
	 *            use for coloring the icon images. <strong>NOTE:</strong> This
	 *            is ordered so that the iteration order of image data is stable
	 *            across invocations.
	 * @param sizes
	 *            An ordered sequence of the {@link ImageSize sizes} to present
	 *            each image in. <strong>NOTE:</strong> This is ordered so that
	 *            the iteration order of image data is stable across
	 *            invocations.
	 * @param maxSharedAttrCount
	 *            The maximum number of attributes an image may share with
	 *            another image.
	 */
	public RandomPieceImageDataFactory(final Map<String, URL> imgResources, final List<Color> uniqueImgColors,
			final List<ImageSize> sizes, final int maxSharedAttrCount) {
		this.imgResources = imgResources;
		this.uniqueImgColors = uniqueImgColors;
		this.sizes = sizes;
		this.maxSharedAttrCount = maxSharedAttrCount;
	}

	/**
	 *
	 * @param imgResources
	 *            A mapping of icon names mapped to their corresponding image
	 *            {@link URL} resource locators.
	 * @param sizes
	 *            An ordered sequence of the {@link ImageSize sizes} to present
	 *            each image in.
	 * @param maxSharedAttrCount
	 *            The maximum number of attributes an image may share with
	 *            another image.
	 */
	public RandomPieceImageDataFactory(final Map<String, URL> imgResources, final List<ImageSize> sizes,
			final int maxSharedAttrCount) {
		this(imgResources, DEFAULT_UNIQUE_IMG_COLORS, sizes, maxSharedAttrCount);
	}

	/**
	 *
	 * @param rnd
	 *            The {@link Random} instance to use for randomization.
	 * @return A {@link Stream} of {@link Entry} objects with the image resource
	 *         {@link URL} and respective {@link Color} to use for filtering it.
	 */
	@Override
	public Stream<ImageDatum> apply(final Random rnd) {
		final List<ImageDatum> imgData = createImageDatumEnumerationList();
		Collections.shuffle(imgData, rnd);

		final Multimap<URL, ImageSize> imgSizes = HashMultimap.create(imgData.size(), sizes.size());
		final Multimap<URL, Color> imgColors = HashMultimap.create(imgData.size(), uniqueImgColors.size());
		final Stream.Builder<ImageDatum> resultBuilder = Stream.builder();
		for (final ImageDatum imgDatum : imgData) {
			final URL resourceLoc = imgDatum.getResourceLoc();
			int commonAttrCount = 0;
			{
				// Check size
				final ImageSize size = imgDatum.getSize();
				if (imgSizes.put(resourceLoc, size)) {
					// The given size has never been used for this resource
					// before
					LOGGER.debug("Observed {} for resource \"{}\" for the first time.", size, resourceLoc);
				} else {
					// The given size has already been used at least once for
					// this resource
					commonAttrCount++;
					if (maxSharedAttrCount < commonAttrCount) {
						LOGGER.debug("Maximum number of common attributes reached for resource \"{}\"; Skipping.",
								resourceLoc);
						break;
					}
				}
			}
			{
				// Check color
				final Color color = imgDatum.getColor();
				if (imgColors.put(resourceLoc, color)) {
					// The given color has never been used for this resource
					// before
					LOGGER.debug("Observed {} for resource \"{}\" for the first time.", color, resourceLoc);
				} else {
					// The given color has already been used at least once for
					// this resource
					commonAttrCount++;
					if (maxSharedAttrCount < commonAttrCount) {
						LOGGER.debug("Maximum number of common attributes reached for resource \"{}\"; Skipping.",
								resourceLoc);
						break;
					}
				}
			}
			// If all attributes are okay, add it to the result stream
			resultBuilder.accept(imgDatum);
		}

		return resultBuilder.build();
	}

	private List<ImageDatum> createImageDatumEnumerationList() {
		final int colorSizeComboCount = uniqueImgColors.size() * sizes.size();
		final List<ImageDatum> result = new ArrayList<>(colorSizeComboCount * imgResources.size());
		for (final Entry<String, URL> namedImgResource : imgResources.entrySet()) {
			for (final Color color : uniqueImgColors) {
				for (final ImageSize size : sizes) {
					final ImageDatum newDatum = new ImageDatum(namedImgResource.getValue(), color, size);
					result.add(newDatum);
				}
			}
		}
		return result;
	}

}
