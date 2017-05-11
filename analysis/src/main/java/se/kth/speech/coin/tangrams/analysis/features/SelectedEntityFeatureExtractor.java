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

import java.util.function.Function;
import java.util.function.ToIntFunction;

import se.kth.speech.IntArrays;
import se.kth.speech.SpatialMatrix;
import se.kth.speech.SpatialRegion;
import se.kth.speech.coin.tangrams.analysis.GameContext;
import se.kth.speech.coin.tangrams.iristk.events.ImageVisualizationInfoDescription;
import weka.core.Instance;

public final class SelectedEntityFeatureExtractor implements GameContextFeatureExtractor {

	private final Function<? super GameContext, Integer> entityIdGetter;

	private final EntityFeature.Extractor extractor;

	private final Function<? super GameContext, SpatialMatrix<Integer>> gameModelFactory;

	private final ToIntFunction<? super String> namedResourceEdgeCountFactory;

	public SelectedEntityFeatureExtractor(final EntityFeature.Extractor extractor,
			final Function<? super GameContext, Integer> entityIdGetter,
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
		final EntityFeature.Extractor.Context entityData = createExtractionContext(context, model);
		extractor.accept(vals, entityData);
	}

	public Object getVal(final EntityFeature feature, final GameContext context) {
		final SpatialMatrix<Integer> model = gameModelFactory.apply(context);
		final EntityFeature.Extractor.Context entityData = createExtractionContext(context, model);
		return extractor.getVal(feature, entityData);
	}

	private EntityFeature.Extractor.Context createExtractionContext(final GameContext context,
			final SpatialMatrix<Integer> model) {
		final Integer entityId = entityIdGetter.apply(context);
		final ImageVisualizationInfoDescription.Datum imgVizInfoDatum = context.getEntityVisualizationInfo(entityId);
		final SpatialRegion region = model.getElementPlacements().getElementMinimalRegions().get(entityId);
		final int[] modelDims = model.getDimensions();
		final double modelArea = IntArrays.product(modelDims);
		return new EntityFeature.Extractor.Context(imgVizInfoDatum, region, modelDims, modelArea,
				namedResourceEdgeCountFactory);
	}

}