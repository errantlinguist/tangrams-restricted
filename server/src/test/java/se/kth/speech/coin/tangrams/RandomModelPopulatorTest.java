/*
 *  This file is part of se.kth.speech.coin.tangrams.server.
 *
 *  se.kth.speech.coin.tangrams.server is free software: you can redistribute it and/or modify
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
package se.kth.speech.coin.tangrams;

import java.awt.Image;
import java.awt.Toolkit;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Random;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.Assert;
import org.junit.Test;
import org.junit.experimental.theories.DataPoints;
import org.junit.experimental.theories.Theories;
import org.junit.runner.RunWith;

import com.google.common.collect.Maps;
import com.google.common.collect.Sets;

import se.kth.speech.RandomCollections;
import se.kth.speech.SpatialMap;
import se.kth.speech.SpatialMatrix;
import se.kth.speech.coin.tangrams.content.ImageLoadingImageViewInfoFactory;
import se.kth.speech.coin.tangrams.content.ImageSize;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfoFactory;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 20 Feb 2017
 *
 */
@RunWith(Theories.class)
public final class RandomModelPopulatorTest {

	@DataPoints("seeds")
	public static final long[] TEST_SEEDS;

	@DataPoints("imgPlacementCounts")
	public static final int[] TEST_IMG_PLACEMENT_COUNTS;

	@DataPoints("gridDims")
	public static final Set<int[]> TEST_GRID_DIMS;

	@DataPoints("imgVisInfoFactories")
	public static final Collection<Entry<Random, ImageVisualizationInfoFactory>> TEST_IMG_VIS_INFO_FACTORIES;

	static {

		final Random rnd = new Random();

		TEST_SEEDS = rnd.longs().distinct().limit(10).toArray();
		final Map<Random, ImageVisualizationInfoFactory> testImgVisInfoFactories = Maps
				.newHashMapWithExpectedSize(TEST_SEEDS.length);
		Arrays.stream(TEST_SEEDS).forEach(testSeed -> {
			final Random factoryRnd = new Random(testSeed);
			final ImageVisualizationInfoFactory factory = new ImageVisualizationInfoFactory(factoryRnd);
			testImgVisInfoFactories.put(factoryRnd, factory);
		});
		TEST_IMG_VIS_INFO_FACTORIES = testImgVisInfoFactories.entrySet();

		final int maxImgPlacementCount = TEST_IMG_VIS_INFO_FACTORIES.stream().map(Entry::getValue)
				.mapToInt(ImageVisualizationInfoFactory::combinationCount).min().getAsInt();
		TEST_IMG_PLACEMENT_COUNTS = rnd.ints().filter(val -> val <= maxImgPlacementCount).distinct().limit(10)
				.toArray();
		final int[] testDimLengths = rnd.ints(1, 21).distinct().limit(10).toArray();
		final int testGridDimCount = 5;

		// TODO: Make sure that the grid is big enough to add all the pieces
		TEST_GRID_DIMS = Sets.newHashSetWithExpectedSize(testGridDimCount);
		do {
			final int x = RandomCollections.getRandomElement(testDimLengths, rnd);
			final int y = RandomCollections.getRandomElement(testDimLengths, rnd);
			TEST_GRID_DIMS.add(new int[] { x, y });
		} while (TEST_GRID_DIMS.size() < testGridDimCount);
	}

	private static final BiFunction<Image, Toolkit, Image> DEFAULT_POST_COLORING_IMG_TRANSFORMER = new BiFunction<Image, Toolkit, Image>() {

		@Override
		public Image apply(final Image img, final Toolkit toolkit) {
			// Do nothing
			return img;
		}

	};

	private static final double DEFAULT_OCCUPIED_GRID_AREA = 0.75;

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.RandomModelPopulator#accept(java.util.Random)}.
	 */
	@Test
	public final void testAccept() {
		final Random rnd = new Random();
		final ImageVisualizationInfoFactory imgDataFactory = new ImageVisualizationInfoFactory(rnd);
		final int maxImgVisualizationInfoDatumCount = imgDataFactory.combinationCount();
		// final int imgPlacementCount = rnd.ints().filter(val -> val <=
		// maxImgVisualizationInfoDatumCount).findAny().getAsInt();
		final int imgPlacementCount = 2;
		// Sanity check to ensure that the test hasn't been constructed wrong
		Assert.assertTrue(imgPlacementCount <= maxImgVisualizationInfoDatumCount);
		final int[] gridSize = new int[] { 20, 20 };
		final SpatialMatrix<Integer> result = new SpatialMatrix<>(gridSize, new SpatialMap<>(imgPlacementCount));

		final Map<Integer, Image> pieceImgs = Maps.newHashMapWithExpectedSize(imgPlacementCount);
		final ImageLoadingImageViewInfoFactory imgViewInfoFactory = new ImageLoadingImageViewInfoFactory(
				Toolkit.getDefaultToolkit(), DEFAULT_POST_COLORING_IMG_TRANSFORMER,
				Maps.newHashMapWithExpectedSize(imgPlacementCount));
		final List<ImageVisualizationInfo> imgVisualizationInfo = Stream.generate(imgDataFactory::next)
				.limit(imgPlacementCount).collect(Collectors.toList());
		// Sort the list so that the biggest images come first
		imgVisualizationInfo
				.sort(Comparator.comparing(ImageVisualizationInfo::getSize, ImageSize.getSizeComparator().reversed()));

		final RandomModelPopulator modelPopulator = new RandomModelPopulator(result, imgVisualizationInfo,
				DEFAULT_OCCUPIED_GRID_AREA, false, imgPlacementCount, pieceImgs::put, imgViewInfoFactory);
		modelPopulator.accept(rnd);

		Assert.assertEquals(imgPlacementCount, result.createValidMoveMap().size());
		final Set<Integer> pieceIds = result.getCells().filter(Objects::nonNull)
				.collect(Collectors.toCollection(() -> Sets.newHashSetWithExpectedSize(imgPlacementCount)));
		Assert.assertEquals(imgPlacementCount, pieceIds.size());

		final SpatialMap<Integer> posMap = result.getElementPlacements();
		Assert.assertEquals(imgPlacementCount, posMap.getAllElements().size());
		Assert.assertEquals(imgPlacementCount, posMap.getMinimalRegionElements().size());
		Assert.assertEquals(imgPlacementCount, posMap.getMinimalRegions().size());
	}

}
