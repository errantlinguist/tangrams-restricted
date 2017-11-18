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
package se.kth.speech.coin.tangrams.game;

import java.awt.Image;
import java.awt.Toolkit;
import java.util.Random;
import java.util.function.BiFunction;
import java.util.function.Function;

import se.kth.speech.SpatialMap;
import se.kth.speech.SpatialMatrix;
import se.kth.speech.coin.tangrams.content.ImageLoadingImageViewInfoFactory;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 20 Mar 2017
 *
 */
final class RandomPopulatedModelFactory implements Function<Random, SpatialMatrix<Integer>> {

	private final boolean allowFailedPlacements;

	private final int[] gridSize;

	private final ImageVisualizationInfo imgVisualizationInfo;

	private final double occupiedGridArea;

	private final BiFunction<? super Image, ? super Toolkit, ? extends Image> postColoringImgTransformer;

	private final Toolkit toolkit;

	RandomPopulatedModelFactory(final int[] gridSize, final ImageVisualizationInfo imgVisualizationInfo,
			final Toolkit toolkit, final double occupiedGridArea,
			final BiFunction<? super Image, ? super Toolkit, ? extends Image> postColoringImgTransformer,
			final boolean allowFailedPlacements) {
		this.gridSize = gridSize;
		this.imgVisualizationInfo = imgVisualizationInfo;
		this.toolkit = toolkit;
		this.occupiedGridArea = occupiedGridArea;
		this.postColoringImgTransformer = postColoringImgTransformer;
		this.allowFailedPlacements = allowFailedPlacements;
	}

	@Override
	public SpatialMatrix<Integer> apply(final Random rnd) {
		final int piecePlacementCount = imgVisualizationInfo.getData().size();
		final SpatialMatrix<Integer> result = SpatialMatrix.Factory.STABLE_ITER_ORDER.create(gridSize,
				SpatialMap.Factory.STABLE_ITER_ORDER.create(piecePlacementCount));
		final ImageLoadingImageViewInfoFactory imgViewInfoFactory = new ImageLoadingImageViewInfoFactory(toolkit,
				postColoringImgTransformer, imgVisualizationInfo.getUniqueImageResourceCount());
		final RandomModelPopulator modelPopulator = new RandomModelPopulator(result, imgVisualizationInfo,
				occupiedGridArea, allowFailedPlacements, imgViewInfoFactory);
		modelPopulator.accept(rnd);
		result.compact();
		return result;
	}

}
