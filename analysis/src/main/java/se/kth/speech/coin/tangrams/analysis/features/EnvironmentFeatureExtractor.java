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

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.function.Function;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Maps;

import iristk.system.Event;
import se.kth.speech.Matrix;
import se.kth.speech.SpatialMatrix;
import se.kth.speech.SpatialMatrixRegionElementMover;
import se.kth.speech.SpatialRegion;
import se.kth.speech.coin.tangrams.AreaSpatialRegionFactory;
import se.kth.speech.coin.tangrams.analysis.GameContext;
import se.kth.speech.coin.tangrams.analysis.GameHistory;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;
import se.kth.speech.coin.tangrams.iristk.events.GameStateDescription;
import se.kth.speech.coin.tangrams.iristk.events.GameStateUnmarshalling;
import se.kth.speech.coin.tangrams.iristk.events.ImageVisualizationInfoDescription;
import se.kth.speech.coin.tangrams.iristk.events.ModelDescription;
import se.kth.speech.coin.tangrams.iristk.events.Move;
import weka.core.Instance;

final class EnvironmentFeatureExtractor implements GameContextFeatureExtractor {

	private enum EnvironmentFeature {
		COL_COUNT, ENTITY_COUNT, ROW_COUNT;

		private static final List<EnvironmentFeature> ORDERING;

		static {
			ORDERING = Arrays.asList(ROW_COUNT, COL_COUNT, ENTITY_COUNT);
			assert ORDERING.size() == EnvironmentFeature.values().length;
		}

//		private static void setVals(final Instance vals, final int[] modelDims, final int entityCount) {
//			for (final EnvironmentFeature feature : ORDERING) {
//				switch (feature) {
//				case COL_COUNT:
//					vals.accept(modelDims[1]);
//					break;
//				case ENTITY_COUNT:
//					vals.accept(entityCount);
//					break;
//				case ROW_COUNT:
//					vals.accept(modelDims[0]);
//					break;
//				default: {
//					throw new AssertionError("Missing enum-handling logic.");
//				}
//				}
//			}
//		}
	}

	private static final Logger LOGGER = LoggerFactory.getLogger(EnvironmentFeatureExtractor.class);

	private static void applyEvent(final SpatialMatrix<Integer> model, final Event event) {
		final Move move = (Move) event.get(GameManagementEvent.Attribute.MOVE.toString());
		if (move == null) {
			LOGGER.debug("Event has no move attribute; Ignoring.");
		} else {
			applyMove(model, move);
		}
	}

	private static void applyMove(final SpatialMatrix<Integer> model, final Move move) {
		final AreaSpatialRegionFactory areaRegionFactory = new AreaSpatialRegionFactory(model);
		final SpatialMatrixRegionElementMover<Integer> pieceUpdater = new SpatialMatrixRegionElementMover<>(model);
		final SpatialRegion source = areaRegionFactory.apply(move.getSource());
		final SpatialRegion target = areaRegionFactory.apply(move.getTarget());
		pieceUpdater.accept(source, target);
	}

	private static SpatialMatrix<Integer> copyInitialModel(final Matrix<Integer> copyee) {
		final List<Integer> copiedVals = new ArrayList<>(copyee.getValues());
		final Matrix<Integer> deepCopy = new Matrix<>(copiedVals, copyee.getDimensions()[1]);
		return SpatialMatrix.Factory.UNSTABLE_ITER_ORDER.create(deepCopy);
	}

	private static SpatialMatrix<Integer> copyInitialModel(final SpatialMatrix<Integer> copyee) {
		return copyInitialModel(copyee.getPositionMatrix());
	}

	private static void updateToTime(final SpatialMatrix<Integer> model,
			final NavigableMap<LocalDateTime, List<Event>> timestampedEvents, final LocalDateTime time) {
		final NavigableMap<LocalDateTime, List<Event>> timestampedEventsToApply = timestampedEvents.headMap(time, true);
		final Stream<Event> eventsToApply = timestampedEventsToApply.values().stream().flatMap(List::stream);
		eventsToApply.forEachOrdered(event -> applyEvent(model, event));
	}

	private final Function<ModelDescription, SpatialMatrix<Integer>> initialGameModelFactory;

	EnvironmentFeatureExtractor(final int expectedUniqueModelDescriptionCount) {
		final Map<ModelDescription, SpatialMatrix<Integer>> gameModels = Maps
				.newHashMapWithExpectedSize(expectedUniqueModelDescriptionCount);
		initialGameModelFactory = modelDesc -> gameModels.computeIfAbsent(modelDesc,
				k -> GameStateUnmarshalling.createModel(k, SpatialMatrix.Factory.UNSTABLE_ITER_ORDER));
	}

	@Override
	public void accept(final GameContext context, final Instance vals) {
//		extractFeatures(context.getHistory(), context.getTime(), vals);
	}

//	/*
//	 * (non-Javadoc)
//	 *
//	 * @see se.kth.speech.coin.tangrams.analysis.FeatureExtractor#
//	 * createFeatureDescriptions(se.kth.speech.coin.tangrams.analysis.
//	 * GameContext)
//	 */
//	@Override
//	public Stream<String> createFeatureDescriptions(final GameStateDescription initialState) {
//		return createFeatureDescriptions(initialState.getImageVisualizationInfoDescription().getData().size());
//	}

//	private Stream<String> createFeatureDescriptions(final int entityCount) {
//		return EnvironmentFeature.ORDERING.stream().map(Enum::toString);
//	}

//	private void extractFeatures(final GameHistory history, final LocalDateTime time, final Instance vals) {
//		final GameStateDescription initialState = history.getInitialState();
//		final SpatialMatrix<Integer> model = copyInitialModel(
//				initialGameModelFactory.apply(initialState.getModelDescription()));
//		final NavigableMap<LocalDateTime, List<Event>> events = history.getEvents();
//		updateToTime(model, events, time);
//		extractFeatures(model, initialState.getImageVisualizationInfoDescription().getData(), vals);
//	}

//	private void extractFeatures(final SpatialMatrix<Integer> model,
//			final List<ImageVisualizationInfoDescription.Datum> imgVizInfoData, final Instance vals) {
//		final int[] modelDims = model.getDimensions();
//		final int entityCount = imgVizInfoData.size();
//		EnvironmentFeature.setVals(vals, modelDims, entityCount);
//	}
}