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
package se.kth.speech.coin.tangrams.analysis;

import java.awt.Color;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.NavigableMap;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Maps;

import iristk.system.Event;
import it.unimi.dsi.fastutil.objects.Object2DoubleMap;
import it.unimi.dsi.fastutil.objects.Object2DoubleOpenHashMap;
import se.kth.speech.IntArrays;
import se.kth.speech.Matrix;
import se.kth.speech.SpatialMap;
import se.kth.speech.SpatialMatrix;
import se.kth.speech.SpatialMatrixRegionElementMover;
import se.kth.speech.SpatialRegion;
import se.kth.speech.coin.tangrams.AreaSpatialRegionFactory;
import se.kth.speech.coin.tangrams.content.IconImages;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;
import se.kth.speech.coin.tangrams.iristk.events.GameStateUnmarshalling;
import se.kth.speech.coin.tangrams.iristk.events.ImageVisualizationInfoDescription;
import se.kth.speech.coin.tangrams.iristk.events.ModelDescription;
import se.kth.speech.coin.tangrams.iristk.events.Move;

final class GameStateFeatureVectorFactory implements BiFunction<GameStateChangeData, Timestamp, double[]> {

	private enum EntityFeature {
		COLOR, POSITION_X, POSITION_Y, SHAPE, SIZE;

		private static final List<EntityFeature> ORDERING;

		static {
			ORDERING = Arrays.asList(SHAPE, COLOR, SIZE, POSITION_X, POSITION_Y);
			assert ORDERING.size() == EntityFeature.values().length;
		}

		private static int setVals(final double[] vals, int currentFeatureIdx,
				final ImageVisualizationInfoDescription.Datum pieceImgVizInfoDatum, final SpatialRegion pieceRegion,
				final int[] modelDims, final double modelArea) {
			for (final EntityFeature feature : ORDERING) {
				switch (feature) {
				case COLOR:
					final float colorFeatureVal = createColorFeatureVal(pieceImgVizInfoDatum);
					vals[currentFeatureIdx] = colorFeatureVal;
					break;
				case POSITION_X: {
					final double centerX = pieceRegion.getXLowerBound() + pieceRegion.getLengthX() / 2.0;
					final double posX = centerX / modelDims[0];
					vals[currentFeatureIdx] = posX;
					break;
				}
				case POSITION_Y: {
					final double centerY = pieceRegion.getYLowerBound() + pieceRegion.getLengthY() / 2.0;
					final double posY = centerY / modelDims[1];
					vals[currentFeatureIdx] = posY;
					break;
				}
				case SHAPE:
					final double shapeFeatureVal = getShapeFeatureVal(pieceImgVizInfoDatum);
					vals[currentFeatureIdx] = shapeFeatureVal;
					break;
				case SIZE:
					final int pieceArea = IntArrays.product(pieceRegion.getDimensions());
					final double sizeFeatureVal = pieceArea / modelArea;
					vals[currentFeatureIdx] = sizeFeatureVal;
					break;
				default: {
					throw new AssertionError("Missing enum-handling logic.");
				}
				}
				currentFeatureIdx++;
			}
			return currentFeatureIdx;
		}
	}

	private enum EnvironmentFeature {
		COL_COUNT, ENTITY_COUNT, ROW_COUNT;

		private static final List<EnvironmentFeature> ORDERING;

		static {
			ORDERING = Arrays.asList(ROW_COUNT, COL_COUNT, ENTITY_COUNT);
			assert ORDERING.size() == EnvironmentFeature.values().length;
		}

		private static int setVals(final double[] vals, int currentFeatureIdx, final int[] modelDims,
				final int pieceCount) {
			for (final EnvironmentFeature feature : ORDERING) {
				switch (feature) {
				case COL_COUNT:
					vals[currentFeatureIdx] = modelDims[1];
					break;
				case ENTITY_COUNT:
					vals[currentFeatureIdx] = pieceCount;
					break;
				case ROW_COUNT:
					vals[currentFeatureIdx] = modelDims[0];
					break;
				default: {
					throw new AssertionError("Missing enum-handling logic.");
				}
				}
				currentFeatureIdx++;
			}
			return currentFeatureIdx;
		}
	}

	private static final Logger LOGGER = LoggerFactory.getLogger(GameStateFeatureVectorFactory.class);

	private static final Object2DoubleMap<String> SHAPE_FEATURE_VALS = createShapeFeatureValueMap();

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

	private static float createColorFeatureVal(final ImageVisualizationInfoDescription.Datum pieceImgVizInfoDatum) {
		final Color color = pieceImgVizInfoDatum.getColor();
		final float[] hsbVals = Color.RGBtoHSB(color.getRed(), color.getGreen(), color.getBlue(), null);
		return hsbVals[0];
	}

	private static <K> Object2DoubleMap<K> createEnumeratedKeyFeatureValMap(final Collection<? extends K> keys) {
		final Object2DoubleMap<K> result = new Object2DoubleOpenHashMap<>(keys.size());
		for (final K key : keys) {
			final double featureVal = result.size();
			result.put(key, featureVal);
		}
		return result;
	}

	private static Object2DoubleMap<String> createShapeFeatureValueMap() {
		final Set<String> possibleShapeStrValues = IconImages.getImageResources().keySet();
		return createEnumeratedKeyFeatureValMap(possibleShapeStrValues);
	}

	private static double getShapeFeatureVal(final ImageVisualizationInfoDescription.Datum pieceImgVizInfoDatum) {
		final String strVal = pieceImgVizInfoDatum.getResourceName();
		return SHAPE_FEATURE_VALS.getDouble(strVal);
	}

	private static void updateToTime(final SpatialMatrix<Integer> model,
			final NavigableMap<Timestamp, List<Event>> timestampedEvents, final Timestamp time) {
		final NavigableMap<Timestamp, List<Event>> timestampedEventsToApply = timestampedEvents.headMap(time, true);
		final Stream<Event> eventsToApply = timestampedEventsToApply.values().stream().flatMap(List::stream);
		eventsToApply.forEach(event -> applyEvent(model, event));
	}

	private final Function<ModelDescription, SpatialMatrix<Integer>> initialGameModelFactory;

	GameStateFeatureVectorFactory(final int expectedUniqueModelDescriptionCount) {
		final Map<ModelDescription, SpatialMatrix<Integer>> gameModels = Maps
				.newHashMapWithExpectedSize(expectedUniqueModelDescriptionCount);
		initialGameModelFactory = modelDesc -> gameModels.computeIfAbsent(modelDesc,
				k -> GameStateUnmarshalling.createModel(k, SpatialMatrix.Factory.UNSTABLE_ITER_ORDER));
	}

	@Override
	public double[] apply(final GameStateChangeData gameStateChangeData, final Timestamp time) {
		final SpatialMatrix<Integer> model = copyInitialModel(
				initialGameModelFactory.apply(gameStateChangeData.getInitialState().getModelDescription()));
		final NavigableMap<Timestamp, List<Event>> events = gameStateChangeData.getEvents();
		updateToTime(model, events, time);
		return createFeatureVector(model,
				gameStateChangeData.getInitialState().getImageVisualizationInfoDescription().getData());
	}

	private double[] createFeatureVector(final SpatialMatrix<Integer> model,
			final List<ImageVisualizationInfoDescription.Datum> imgVizInfoData) {
		final int[] modelDims = model.getDimensions();
		final int pieceCount = imgVizInfoData.size();
		final double[] result = new double[EnvironmentFeature.values().length
				+ pieceCount * EntityFeature.values().length];

		int currentFeatureIdx = EnvironmentFeature.setVals(result, 0, modelDims, pieceCount);

		final double modelArea = IntArrays.product(modelDims);
		final SpatialMap<Integer> piecePlacements = model.getElementPlacements();
		for (final ListIterator<ImageVisualizationInfoDescription.Datum> imgVizInfoDataIter = imgVizInfoData
				.listIterator(); imgVizInfoDataIter.hasNext();) {
			final int pieceId = imgVizInfoDataIter.nextIndex();
			final ImageVisualizationInfoDescription.Datum pieceImgVizInfoDatum = imgVizInfoDataIter.next();
			final SpatialRegion pieceRegion = piecePlacements.getElementMinimalRegions().get(pieceId);
			currentFeatureIdx = EntityFeature.setVals(result, currentFeatureIdx, pieceImgVizInfoDatum, pieceRegion,
					modelDims, modelArea);
		}
		return result;
	}
}