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
import java.util.function.ToDoubleFunction;
import java.util.stream.DoubleStream;
import java.util.stream.Stream;

import se.kth.speech.IntArrays;
import se.kth.speech.MutablePair;
import se.kth.speech.SpatialMatrix;
import se.kth.speech.SpatialRegion;
import se.kth.speech.coin.tangrams.analysis.GameContext;
import se.kth.speech.coin.tangrams.iristk.events.GameStateDescription;
import se.kth.speech.coin.tangrams.iristk.events.ImageVisualizationInfoDescription;

final class SelectedEntityFeatureExtractor implements GameContextFeatureExtractor {

	private final Function<? super GameContext, SpatialMatrix<Integer>> gameModelFactory;

	private final ToDoubleFunction<? super String> namedResourceEdgeCountFactory;

	SelectedEntityFeatureExtractor(final Function<? super GameContext, SpatialMatrix<Integer>> gameModelFactory,
			final ToDoubleFunction<? super String> namedResourceEdgeCountFactory) {
		this.gameModelFactory = gameModelFactory;
		this.namedResourceEdgeCountFactory = namedResourceEdgeCountFactory;
	}

	@Override
	public void accept(final GameContext context, final DoubleStream.Builder vals) {
		final SpatialMatrix<Integer> model = gameModelFactory.apply(context);
		final Optional<Integer> lastSelectedEntityId = context.findLastSelectedEntityId();
		final Optional<Entry<ImageVisualizationInfoDescription.Datum, SpatialRegion>> entityData = lastSelectedEntityId
				.map(entityId -> {
					final ImageVisualizationInfoDescription.Datum imgVizInfoDatum = context
							.getEntityVisualizationInfo(entityId);
					final SpatialRegion region = model.getElementPlacements().getElementMinimalRegions().get(entityId);
					return new MutablePair<>(imgVizInfoDatum, region);
				});
		extractFeatures(model, entityData, vals);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see se.kth.speech.coin.tangrams.analysis.FeatureExtractor#
	 * createFeatureDescriptions(se.kth.speech.coin.tangrams.analysis.
	 * GameContext)
	 */
	@Override
	public Stream<String> createFeatureDescriptions(final GameStateDescription initialState) {
		return EntityFeature.getOrdering().stream().map(Enum::toString);
	}

	private void extractFeatures(final SpatialMatrix<Integer> model,
			final Optional<Entry<ImageVisualizationInfoDescription.Datum, SpatialRegion>> entityData,
			final DoubleStream.Builder vals) {
		final int[] modelDims = model.getDimensions();
		final double modelArea = IntArrays.product(modelDims);
		EntityFeature.setVals(vals, entityData, modelDims, modelArea, namedResourceEdgeCountFactory);
	}
}