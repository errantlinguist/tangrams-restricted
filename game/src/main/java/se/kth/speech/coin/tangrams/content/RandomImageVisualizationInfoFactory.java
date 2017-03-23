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
import java.util.Collection;
import java.util.Collections;
import java.util.EnumMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.Random;
import java.util.Set;
import java.util.function.IntFunction;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Maps;

import se.kth.speech.ComparableValueMaps;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 7 Mar 2017
 *
 */
public final class RandomImageVisualizationInfoFactory implements IntFunction<ImageVisualizationInfo> {

	private static final Collection<URL> DEFAULT_IMG_RESOURCES = IconImages.getImageResources().values();

	private static final List<ImageSize> DEFAULT_IMG_SIZES = Arrays.asList(ImageSize.values());

	private static final Logger LOGGER = LoggerFactory.getLogger(RandomImageVisualizationInfoFactory.class);

	private final Map<Color, Integer> colorUsageCounts;

	private int createdInstanceCount;

	private final Iterator<ImageVisualizationInfo.Datum> datumIter = new Iterator<ImageVisualizationInfo.Datum>() {
		@Override
		public boolean hasNext() {
			return getCreatedInstanceCount() < combinationCount();
		}

		@Override
		public ImageVisualizationInfo.Datum next() {
			if (!hasNext()) {
				throw new NoSuchElementException(
						String.format("Created all %d possible combinations.", combinationCount()));
			}
			final ImageVisualizationInfo.Datum result;
			final Optional<URL> resourceLoc = nextImgResourceLocator();
			if (resourceLoc.isPresent()) {
				final URL r = resourceLoc.get();
				LOGGER.debug("Next img resource is {}.", r);
				final Optional<Color> color = nextColor();
				if (color.isPresent()) {
					final Color c = color.get();
					LOGGER.debug("Next color is {}.", c);
					final Optional<ImageSize> size = nextSize();
					if (size.isPresent()) {
						final ImageSize s = size.get();
						LOGGER.debug("Next size is {}.", s);
						result = createInstance(r, c, s);
					} else {
						throw new NoSuchElementException("No more sizes.");
					}
				} else {
					throw new NoSuchElementException("No more colors.");
				}
			} else {
				throw new NoSuchElementException("No more image resources.");
			}
			return result;
		}

		private ImageVisualizationInfo.Datum createInstance(final URL resourceLoc, final Color color,
				final ImageSize size) {
			LOGGER.debug("Creating instance number {}.", createdInstanceCount);
			final ImageVisualizationInfo.Datum result = new ImageVisualizationInfo.Datum(resourceLoc, color, size);
			incrementImageResourceCount(resourceLoc);
			incrementColorCount(color);
			incrementSizeCount(size);
			createdInstanceCount++;
			LOGGER.debug("size usages: {}", sizeUsageCounts);
			return result;
		}
	};

	private final Map<URL, Integer> imgResourceUsageCounts;

	private Color lastColor;

	private URL lastImgResource;

	private ImageSize lastSize;

	/**
	 * The {@link Random} instance to use for randomization.
	 */
	private final Random rnd;

	private final Map<ImageSize, Integer> sizeUsageCounts;

	/**
	 * @param maxSharedAttrCount
	 *            The maximum number of attributes an image may share with
	 *            another image.
	 * @param rnd
	 *            The {@link Random} instance to use for randomization.
	 *
	 */
	public RandomImageVisualizationInfoFactory(final Random rnd, final List<? extends Color> uniqueImgColors) {
		this(rnd, uniqueImgColors, DEFAULT_IMG_SIZES);
	}

	/**
	 *
	 * @param rnd
	 *            The {@link Random} instance to use for randomization.
	 * @param uniqueImgColors
	 *            An ordered sequence of the unique {@link Color} instances to
	 *            use for coloring the icon images. <strong>NOTE:</strong> This
	 *            is ordered so that the iteration order of image data is stable
	 *            across instances.
	 * @param imgResources
	 *            A mapping of icon names mapped to their corresponding image
	 *            {@link URL} resource locators.
	 */
	public RandomImageVisualizationInfoFactory(final Random rnd, final List<? extends Color> uniqueImgColors,
			final Collection<? extends URL> imgResources) {
		this(rnd, uniqueImgColors, DEFAULT_IMG_SIZES, imgResources);
	}

	/**
	 *
	 * @param rnd
	 *            The {@link Random} instance to use for randomization.
	 * @param uniqueImgColors
	 *            An ordered sequence of the unique {@link Color} instances to
	 *            use for coloring the icon images. <strong>NOTE:</strong> This
	 *            is ordered so that the iteration order of image data is stable
	 *            across instances.
	 * @param sizes
	 *            An ordered sequence of the {@link ImageSize sizes} to present
	 *            each image in. <strong>NOTE:</strong> This is ordered so that
	 *            the iteration order of image data is stable across
	 *            invocations.
	 */
	public RandomImageVisualizationInfoFactory(final Random rnd, final List<? extends Color> uniqueImgColors,
			final List<ImageSize> sizes) {
		this(rnd, uniqueImgColors, sizes, DEFAULT_IMG_RESOURCES);
	}

	/**
	 *
	 * @param rnd
	 *            The {@link Random} instance to use for randomization.
	 * @param uniqueImgColors
	 *            An ordered sequence of the unique {@link Color} instances to
	 *            use for coloring the icon images. <strong>NOTE:</strong> This
	 *            is ordered so that the iteration order of image data is stable
	 *            across instances.
	 * @param sizes
	 *            An ordered sequence of the {@link ImageSize sizes} to present
	 *            each image in. <strong>NOTE:</strong> This is ordered so that
	 *            the iteration order of image data is stable across
	 *            invocations.
	 * @param imgResources
	 *            A mapping of icon names mapped to their corresponding image
	 *            {@link URL} resource locators.
	 */
	public RandomImageVisualizationInfoFactory(final Random rnd, final List<? extends Color> uniqueImgColors,
			final List<ImageSize> sizes, final Collection<? extends URL> imgResources) {
		this.rnd = rnd;
		createdInstanceCount = 0;
		imgResourceUsageCounts = Maps.newLinkedHashMapWithExpectedSize(imgResources.size());
		imgResources.forEach(loc -> imgResourceUsageCounts.put(loc, 0));
		colorUsageCounts = Maps.newLinkedHashMapWithExpectedSize(uniqueImgColors.size());
		uniqueImgColors.forEach(color -> colorUsageCounts.put(color, 0));
		sizeUsageCounts = new EnumMap<>(ImageSize.class);
		sizes.forEach(size -> sizeUsageCounts.put(size, 0));
	}

	@Override
	public ImageVisualizationInfo apply(final int pieceCount) {
		final Stream<ImageVisualizationInfo.Datum> imgVisualizationInfoData = Stream.generate(datumIter::next)
				.limit(pieceCount);
		final List<ImageVisualizationInfo.Datum> imgVisualizationInfoDataList = imgVisualizationInfoData
				.collect(Collectors.toCollection(() -> new ArrayList<>(pieceCount)));
		return new ImageVisualizationInfo(imgVisualizationInfoDataList, imgResourceUsageCounts.size());
	}

	public int combinationCount() {
		final int colorSizeComboCount = colorUsageCounts.keySet().size() * sizeUsageCounts.keySet().size();
		return imgResourceUsageCounts.keySet().size() * colorSizeComboCount;
	}

	/**
	 * @return the colorUsageCounts
	 */
	public Map<Color, Integer> getColorUsageCounts() {
		return Collections.unmodifiableMap(colorUsageCounts);
	}

	/**
	 * @return the createdInstanceCount
	 */
	public int getCreatedInstanceCount() {
		return createdInstanceCount;
	}

	/**
	 * @return the imgResourceUsageCounts
	 */
	public Map<URL, Integer> getImgResourceUsageCounts() {
		return Collections.unmodifiableMap(imgResourceUsageCounts);
	}

	/**
	 * @return the sizeUsageCounts
	 */
	public Map<ImageSize, Integer> getSizeUsageCounts() {
		return Collections.unmodifiableMap(sizeUsageCounts);
	}

	/**
	 * @param createdInstanceCount
	 *            the createdInstanceCount to set
	 */
	public void setCreatedInstanceCount(final int createdInstanceCount) {
		this.createdInstanceCount = createdInstanceCount;
	}

	private void incrementColorCount(final Color key) {
		ComparableValueMaps.incrementCount(colorUsageCounts, key);
	}

	private void incrementImageResourceCount(final URL key) {
		ComparableValueMaps.incrementCount(imgResourceUsageCounts, key);
	}

	private void incrementSizeCount(final ImageSize key) {
		ComparableValueMaps.incrementCount(sizeUsageCounts, key);
	}

	private Optional<Color> nextColor() {
		final Map.Entry<Set<Color>, Integer> c = ComparableValueMaps.findMinValues(colorUsageCounts);
		final List<Color> keys = new ArrayList<>(c.getKey());
		Collections.shuffle(keys, rnd);
		LOGGER.debug("Min colors: {}; value: {}", keys, c.getValue());
		final Optional<Color> result = keys.stream().filter(key -> !key.equals(lastColor)).findFirst();
		if (result.isPresent()) {
			lastColor = result.get();
		}
		return result;
	}

	private Optional<URL> nextImgResourceLocator() {
		final Entry<Set<URL>, Integer> i = ComparableValueMaps.findMinValues(imgResourceUsageCounts);
		final List<URL> keys = new ArrayList<>(i.getKey());
		Collections.shuffle(keys, rnd);
		LOGGER.debug("Min img resources: {}; value: {}", keys, i.getValue());
		final Optional<URL> result = keys.stream().filter(key -> !key.equals(lastImgResource)).findFirst();
		if (result.isPresent()) {
			lastImgResource = result.get();
		}
		return result;
	}

	private Optional<ImageSize> nextSize() {
		final Entry<Set<ImageSize>, Integer> s = ComparableValueMaps.findMinValues(sizeUsageCounts);
		final List<ImageSize> keys = new ArrayList<>(s.getKey());
		Collections.shuffle(keys, rnd);
		LOGGER.debug("Min sizes: {}; value: {}", keys, s.getValue());
		final Optional<ImageSize> result = keys.stream().filter(key -> !key.equals(lastSize)).findFirst();
		if (result.isPresent()) {
			lastSize = result.get();
		}
		return result;
	}

}
