/*
 *  This file is part of game.
 *
 *  game is free software: you can redistribute it and/or modify
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
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.google.common.collect.Maps;

import se.kth.speech.SpatialMap;
import se.kth.speech.SpatialMatrix;
import se.kth.speech.coin.tangrams.content.ImageLoadingImageViewInfoFactory;
import se.kth.speech.coin.tangrams.content.ImageSize;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 20 Mar 2017
 *
 */
final class RandomPopulatedModelFactory implements Function<Random, SpatialMatrix<Integer>> {

	private final boolean allowFailedPlacements;

	private final int[] gridSize;

	private final Function<? super Random, ? extends Iterator<ImageVisualizationInfo>> imgVisualizationInfoIterFactory;

	private final double occupiedGridArea;

	private final int piecePlacementCount;

	private final BiFunction<? super Image, ? super Toolkit, ? extends Image> postColoringImgTransformer;

	private final Toolkit toolkit;

	RandomPopulatedModelFactory(final int[] gridSize,
			final Function<? super Random, ? extends Iterator<ImageVisualizationInfo>> imgVisualizationInfoIterFactory,
			final Toolkit toolkit, final int piecePlacementCount, final double occupiedGridArea,
			final BiFunction<? super Image, ? super Toolkit, ? extends Image> postColoringImgTransformer,
			final boolean allowFailedPlacements) {
		this.gridSize = gridSize;
		this.imgVisualizationInfoIterFactory = imgVisualizationInfoIterFactory;
		this.toolkit = toolkit;
		this.piecePlacementCount = piecePlacementCount;
		this.occupiedGridArea = occupiedGridArea;
		this.postColoringImgTransformer = postColoringImgTransformer;
		this.allowFailedPlacements = allowFailedPlacements;
	}

	@Override
	public SpatialMatrix<Integer> apply(final Random rnd) {
		final SpatialMatrix<Integer> result = new SpatialMatrix<>(gridSize, new SpatialMap<>(piecePlacementCount));

		final Iterator<ImageVisualizationInfo> imgDataIter = imgVisualizationInfoIterFactory.apply(rnd);
		final Map<Integer, Image> pieceImgs = Maps.newHashMapWithExpectedSize(piecePlacementCount);
		final ImageLoadingImageViewInfoFactory imgViewInfoFactory = new ImageLoadingImageViewInfoFactory(toolkit,
				postColoringImgTransformer, Maps.newHashMapWithExpectedSize(piecePlacementCount));
		final List<ImageVisualizationInfo> imgVisualizationInfo = Stream.generate(imgDataIter::next)
				.limit(piecePlacementCount).collect(Collectors.toList());
		// Sort the list so that the biggest images come first
		imgVisualizationInfo
				.sort(Comparator.comparing(ImageVisualizationInfo::getSize, ImageSize.getSizeComparator().reversed()));

		final RandomModelPopulator modelPopulator = new RandomModelPopulator(result, imgVisualizationInfo,
				occupiedGridArea, allowFailedPlacements, piecePlacementCount, pieceImgs::put, imgViewInfoFactory);
		modelPopulator.accept(rnd);
		return result;
	}

}
