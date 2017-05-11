/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.analysis.
 *
 *  se.kth.speech.coin.tangrams-restricted.analysis is free software: you can redistribute it and/or modify
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
package se.kth.speech.coin.tangrams.analysis.features;

import java.util.Map.Entry;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.ToIntFunction;

import se.kth.speech.IntArrays;
import se.kth.speech.MutablePair;
import se.kth.speech.SpatialMatrix;
import se.kth.speech.SpatialRegion;
import se.kth.speech.coin.tangrams.analysis.GameContext;
import se.kth.speech.coin.tangrams.iristk.events.ImageVisualizationInfoDescription;
import weka.core.Instance;

public final class SelectedEntityFeatureExtractor implements GameContextFeatureExtractor {

	private final Function<? super GameContext, Optional<Integer>> entityIdGetter;

	private final EntityFeature.Extractor extractor;

	private final Function<? super GameContext, SpatialMatrix<Integer>> gameModelFactory;

	private final ToIntFunction<? super String> namedResourceEdgeCountFactory;

	public SelectedEntityFeatureExtractor(final EntityFeature.Extractor extractor,
			final Function<? super GameContext, Optional<Integer>> entityIdGetter,
			final Function<? super GameContext, SpatialMatrix<Integer>> gameModelFactory,
			final ToIntFunction<? super String> namedResourceEdgeCountFactory) {
		this.extractor = extractor;
		this.entityIdGetter = entityIdGetter;
		this.gameModelFactory = gameModelFactory;
		this.namedResourceEdgeCountFactory = namedResourceEdgeCountFactory;
	}

	@Override
	public void accept(final GameContext context, final Instance vals) {
		final SpatialMatrix<Integer> model = gameModelFactory.apply(context);
		createSelectedEntityDescription(context, model).ifPresent(entityData -> {
			final int[] modelDims = model.getDimensions();
			final double modelArea = IntArrays.product(modelDims);
			extractor.setVals(vals, entityData.getKey(), entityData.getValue(), modelDims, modelArea,
					namedResourceEdgeCountFactory);
		});
	}

	public Optional<Object> getVal(final GameContext context, final EntityFeature feature) {
		final SpatialMatrix<Integer> model = gameModelFactory.apply(context);
		return createSelectedEntityDescription(context, model).map(entityData -> {
			final int[] modelDims = model.getDimensions();
			final double modelArea = IntArrays.product(modelDims);
			return extractor.getVal(feature, entityData.getKey(), entityData.getValue(), modelDims, modelArea,
					namedResourceEdgeCountFactory);
		});
	}

	private Optional<Entry<ImageVisualizationInfoDescription.Datum, SpatialRegion>> createSelectedEntityDescription(
			final GameContext context, final SpatialMatrix<Integer> model) {
		return entityIdGetter.apply(context).map(entityId -> {
			final ImageVisualizationInfoDescription.Datum imgVizInfoDatum = context
					.getEntityVisualizationInfo(entityId);
			final SpatialRegion region = model.getElementPlacements().getElementMinimalRegions().get(entityId);
			return new MutablePair<>(imgVizInfoDatum, region);
		});
	}

}