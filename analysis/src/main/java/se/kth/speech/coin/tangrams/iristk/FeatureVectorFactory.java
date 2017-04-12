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
package se.kth.speech.coin.tangrams.iristk;

import java.awt.Color;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.NavigableMap;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
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
import se.kth.speech.TimestampArithmetic;
import se.kth.speech.coin.tangrams.AreaSpatialRegionFactory;
import se.kth.speech.coin.tangrams.content.IconImages;
import se.kth.speech.coin.tangrams.game.PlayerRole;
import se.kth.speech.coin.tangrams.iristk.events.Move;
import se.kth.speech.hat.xsd.Annotation.Segments.Segment;
import se.kth.speech.hat.xsd.Transcription.T;

public final class FeatureVectorFactory implements Function<Segment, double[][]> {

	private enum EntityFeature {
		COLOR, POSITION_X, POSITION_Y, SELECTED, SHAPE, SIZE;

		private static final List<FeatureVectorFactory.EntityFeature> ORDERING;

		static {
			ORDERING = Arrays.asList(SHAPE, COLOR, SIZE, POSITION_X, POSITION_Y, SELECTED);
			assert ORDERING.size() == EntityFeature.values().length;
		}
	}

	private enum EnvironmentFeature {
		COL_COUNT, ENTITY_COUNT, ROW_COUNT, SPEAKING_PLAYER_ROLE;

		private static final List<FeatureVectorFactory.EnvironmentFeature> ORDERING;

		static {
			ORDERING = Arrays.asList(ROW_COUNT, COL_COUNT, ENTITY_COUNT, SPEAKING_PLAYER_ROLE);
			assert ORDERING.size() == EnvironmentFeature.values().length;
		}
	}

	private static class TimestampedUtterance {

		private final float endTime;

		private final String segmentId;

		private final float startTime;

		private final List<String> tokens;

		private TimestampedUtterance(final String segmentId, final List<String> tokens, final float startTime,
				final float endTime) {
			this.segmentId = segmentId;
			this.tokens = tokens;
			this.startTime = startTime;
			this.endTime = endTime;
		}
	}

	private static final float DEFAULT_MIN_SEGMENT_SPACING;

	private static final Logger LOGGER = LoggerFactory.getLogger(FeatureVectorFactory.class);

	private static final List<PlayerRole> PLAYER_ROLE_FEATURE_ORDERING;

	private static final Object2DoubleMap<PlayerRole> PLAYER_ROLE_FEATURE_VALS;

	private static final int SEGMENT_TIME_TO_MILLS_FACTOR;

	private static final Object2DoubleMap<String> SHAPE_FEATURE_VALS = createShapeFeatureValueMap();

	static {
		SEGMENT_TIME_TO_MILLS_FACTOR = 1000;
		DEFAULT_MIN_SEGMENT_SPACING = 1.0f / SEGMENT_TIME_TO_MILLS_FACTOR;
	}

	static {
		PLAYER_ROLE_FEATURE_ORDERING = createPlayerRoleFeatureOrderingList();
		final List<PlayerRole> nullablePlayerRoleFeatureOrdering = new ArrayList<>(
				PLAYER_ROLE_FEATURE_ORDERING.size() + 1);
		nullablePlayerRoleFeatureOrdering.add(null);
		nullablePlayerRoleFeatureOrdering.addAll(PLAYER_ROLE_FEATURE_ORDERING);
		PLAYER_ROLE_FEATURE_VALS = createEnumeratedKeyFeatureValMap(nullablePlayerRoleFeatureOrdering);
	}

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

	private static List<PlayerRole> createPlayerRoleFeatureOrderingList() {
		final List<PlayerRole> result = Arrays.asList(PlayerRole.MOVE_SUBMISSION, PlayerRole.SELECTING,
				PlayerRole.SELECTION_CONFIRMATION, PlayerRole.WAITING_FOR_NEXT_MOVE, PlayerRole.WAITING_FOR_SELECTION,
				PlayerRole.WAITING_FOR_SELECTION_CONFIRMATION);
		assert result.size() == PlayerRole.values().length;
		return result;
	}

	private static Object2DoubleMap<String> createShapeFeatureValueMap() {
		final Set<String> possibleShapeStrValues = IconImages.getImageResources().keySet();
		return createEnumeratedKeyFeatureValMap(possibleShapeStrValues);
	}

	private static TimestampedUtterance createTimestampedUtterance(final String segmentId, final List<T> tokens,
			final float previousUttEndTime, final float nextUttStartTime) {
		final Float firstTokenStartTime = tokens.get(0).getStart();
		final float seqStartTime = firstTokenStartTime == null ? previousUttEndTime + DEFAULT_MIN_SEGMENT_SPACING
				: firstTokenStartTime;
		final Float lastTokenEndTime = tokens.get(tokens.size() - 1).getEnd();
		final float seqEndTime = lastTokenEndTime == null ? nextUttStartTime - DEFAULT_MIN_SEGMENT_SPACING
				: lastTokenEndTime;
		final List<String> tokenForms = tokens.stream().map(T::getContent)
				.collect(Collectors.toCollection(() -> new ArrayList<>(tokens.size())));
		return new TimestampedUtterance(segmentId, tokenForms, seqStartTime, seqEndTime);
	}

	private static List<TimestampedUtterance> createTimestampedUtterances(final Segment segment) {
		final List<TimestampedUtterance> result = new ArrayList<>();
		final List<Object> children = segment.getTranscription().getSegmentOrT();
		final Float initialPrevUttEndTime = segment.getStart();
		assert initialPrevUttEndTime != null;
		{
			float prevUttEndTime = initialPrevUttEndTime;
			List<T> currentTokenSeq = new ArrayList<>();
			final String parentSegmentId = segment.getId();
			for (final Object child : children) {
				if (child instanceof Segment) {
					final List<TimestampedUtterance> childUtts = createTimestampedUtterances((Segment) child);
					// If there was a contiguous sequence of terminal tokens
					// preceding this segment, finish building it
					if (!currentTokenSeq.isEmpty()) {
						final float nextUttStartTime = childUtts.get(0).startTime;
						result.add(createTimestampedUtterance(parentSegmentId, currentTokenSeq, prevUttEndTime,
								nextUttStartTime));
						currentTokenSeq = new ArrayList<>();
					}
					// Add the newly-created child utterances after adding the
					// now-completed terminal sequence
					result.addAll(childUtts);
					prevUttEndTime = childUtts.get(childUtts.size() - 1).endTime;
				} else if (child instanceof T) {
					currentTokenSeq.add((T) child);
				} else {
					throw new IllegalArgumentException(String.format("Could not parse child annotation of type \"%s\".",
							child.getClass().getName()));
				}
			}
			if (!currentTokenSeq.isEmpty()) {
				// Add the last token sequence
				final Float uttEndTime = segment.getEnd();
				assert uttEndTime != null;
				result.add(createTimestampedUtterance(parentSegmentId, currentTokenSeq, prevUttEndTime, uttEndTime));
			}
		}

		return result;
	}

	private static double getShapeFeatureVal(final ImageVisualizationInfoDescription.Datum pieceImgVizInfoDatum) {
		final String strVal = pieceImgVizInfoDatum.getResourceName();
		return SHAPE_FEATURE_VALS.getDouble(strVal);
	}

	private static int setEntityFeatureVals(final double[] vals, int currentFeatureIdx,
			final ImageVisualizationInfoDescription.Datum pieceImgVizInfoDatum, final SpatialRegion pieceRegion,
			final int[] modelDims, final double modelArea, final boolean isEntitySelected) {
		for (final FeatureVectorFactory.EntityFeature feature : EntityFeature.ORDERING) {
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
			case SELECTED: {
				final double val = isEntitySelected ? 1.0 : 0.0;
				vals[currentFeatureIdx] = val;
				break;
			}
			default: {
				throw new AssertionError("Logic error");
			}
			}
			currentFeatureIdx++;
		}
		return currentFeatureIdx;
	}

	private static int setEnvironmentFeatureVals(final double[] vals, int currentFeatureIdx, final int[] modelDims,
			final int pieceCount, final PlayerRole speakingPlayerRole) {
		for (final FeatureVectorFactory.EnvironmentFeature feature : EnvironmentFeature.ORDERING) {
			switch (feature) {
			case COL_COUNT:
				vals[currentFeatureIdx] = modelDims[1];
				break;
			case ENTITY_COUNT:
				vals[currentFeatureIdx] = pieceCount;
				break;
			case SPEAKING_PLAYER_ROLE: {
				vals[currentFeatureIdx] = PLAYER_ROLE_FEATURE_VALS.get(speakingPlayerRole);
				break;
			}
			case ROW_COUNT:
				vals[currentFeatureIdx] = modelDims[0];
				break;
			default: {
				throw new AssertionError("Logic error");
			}
			}
			currentFeatureIdx++;
		}
		return currentFeatureIdx;
	}

	private static void updateToTime(final SpatialMatrix<Integer> model,
			final NavigableMap<Timestamp, List<Event>> timestampedEvents, final Timestamp time) {
		final NavigableMap<Timestamp, List<Event>> timestampedEventsToApply = timestampedEvents.headMap(time, true);
		final Stream<Event> eventsToApply = timestampedEventsToApply.values().stream().flatMap(List::stream);
		eventsToApply.forEach(event -> applyEvent(model, event));
	}

	private final  Function<ModelDescription, SpatialMatrix<Integer>> initialGameModelFactory;

	private final double[] initialGameStateFeatures;

	private final Map<String, GameStateChangeData> playerStateChangeData;

	private final Map<String, String> sourceIdPlayerIds;

	public FeatureVectorFactory(final Map<String, String> sourceIdPlayerIds,
			final Map<String, GameStateChangeData> playerStateChangeData) {
		this.sourceIdPlayerIds = sourceIdPlayerIds;
		this.playerStateChangeData = playerStateChangeData;

		final Map<ModelDescription, SpatialMatrix<Integer>> gameModels = Maps
				.newHashMapWithExpectedSize(playerStateChangeData.values().size());
		initialGameModelFactory = modelDesc -> gameModels.computeIfAbsent(modelDesc,
				k -> GameStateUnmarshalling.createModel(k, SpatialMatrix.Factory.UNSTABLE_ITER_ORDER));
		initialGameStateFeatures = createInitialFeatureVector(playerStateChangeData.values());
	}

	@Override
	public double[][] apply(final Segment segment) {
		final List<TimestampedUtterance> utts = createTimestampedUtterances(segment);
		final String sourceId = segment.getSource();
		// Get the player ID associated with the given audio source
		final String playerId = sourceIdPlayerIds.get(sourceId);
		final GameStateChangeData gameStateChangeData = playerStateChangeData.get(playerId);
		final Stream<double[]> featureVectors = utts.stream().map(utt -> createFeatureVector(utt, gameStateChangeData));
		return featureVectors.toArray(double[][]::new);
	}

	private double[] createFeatureVector(final Stream<String> uttTokenForms,
			final GameStateChangeData gameStateChangeData, final Timestamp gameTime) {
		final SpatialMatrix<Integer> model = copyInitialModel(
				initialGameModelFactory.apply(gameStateChangeData.getInitialState().getModelDescription()));
		final NavigableMap<Timestamp, List<Event>> events = gameStateChangeData.getEvents();
		updateToTime(model, events, gameTime);
		final double[] result = Arrays.copyOf(initialGameStateFeatures, initialGameStateFeatures.length);
		// TODO: Update e.g. piece position and selection features, player role
		// feature
		return result;
	}

	private double[] createFeatureVector(final TimestampedUtterance utt,
			final GameStateChangeData gameStateChangeData) {
		final NavigableMap<Timestamp, List<Event>> events = gameStateChangeData.getEvents();
		final float uttStartMills = utt.startTime * SEGMENT_TIME_TO_MILLS_FACTOR;
		final Timestamp gameStartTime = gameStateChangeData.getStartTime();
		final Timestamp uttStartTimestamp = TimestampArithmetic.createOffsetTimestamp(gameStartTime, uttStartMills);

		final float uttEndMills = utt.endTime * SEGMENT_TIME_TO_MILLS_FACTOR;
		final Timestamp uttEndTimestamp = TimestampArithmetic.createOffsetTimestamp(gameStartTime, uttEndMills);
		final NavigableMap<Timestamp, List<Event>> eventsDuringUtt = events.tailMap(uttStartTimestamp, true)
				.headMap(uttEndTimestamp, true);
		final List<String> tokenForms = utt.tokens;
		if (!eventsDuringUtt.isEmpty()) {
			final List<Event> allEventsDuringUtt = eventsDuringUtt.values().stream().flatMap(Collection::stream)
					.collect(Collectors.toList());
			if (LOGGER.isWarnEnabled()) {
				final String delim = System.lineSeparator() + '\t';
				final String uttRepr = allEventsDuringUtt.stream().map(Event::toString)
						.collect(Collectors.joining(delim));
				LOGGER.warn("Found {} event(s) during utterance \"{}\" subsequence: \"{}\"" + delim + "{}",
						new Object[] { allEventsDuringUtt.size(), utt.segmentId,
								tokenForms.stream().collect(Collectors.joining(" ")), uttRepr });

			}
			// TODO: estimate partitions for utterance: By phones?
		}

		return createFeatureVector(tokenForms.stream(), gameStateChangeData, uttStartTimestamp);
	}

	private double[] createInitialFeatureVector(final Collection<GameStateChangeData> gameStateData) {
		final List<double[]> playerInitialGameStateFeatureVectors = new ArrayList<>(gameStateData.size());
		for (final GameStateChangeData gameStateDatum : gameStateData) {
			final GameStateDescription gameDesc = gameStateDatum.getInitialState();
			final double[] initialGameStateFeatures = createInitialFeatureVector(gameDesc);
			playerInitialGameStateFeatureVectors.add(initialGameStateFeatures);
		}
		double[] result = null;
		// Sanity check: All initial game feature vectors for all the
		// different players' event files should be equal
		final Iterator<double[]> playerInitialGameStateFeatureVectorIter = playerInitialGameStateFeatureVectors
				.iterator();
		if (playerInitialGameStateFeatureVectorIter.hasNext()) {
			final double[] first = playerInitialGameStateFeatureVectorIter.next();
			while (playerInitialGameStateFeatureVectorIter.hasNext()) {
				final double[] second = playerInitialGameStateFeatureVectorIter.next();
				if (!Arrays.equals(first, second)) {
					throw new IllegalArgumentException("Different initial game states for different players.");
				}
			}
			result = first;
		} else {
			throw new IllegalArgumentException("Player game state change data map is empty.");
		}
		return result;
	}

	private double[] createInitialFeatureVector(final GameStateDescription initialStateDesc) {
		final SpatialMatrix<Integer> model = initialGameModelFactory.apply(initialStateDesc.getModelDescription());
		final int[] modelDims = model.getDimensions();
		final double modelArea = IntArrays.product(modelDims);

		final SpatialMap<Integer> piecePlacements = model.getElementPlacements();
		final ImageVisualizationInfoDescription imgVizInfoDataDesc = initialStateDesc
				.getImageVisualizationInfoDescription();
		final List<ImageVisualizationInfoDescription.Datum> imgVizInfoData = imgVizInfoDataDesc.getData();
		final int pieceCount = imgVizInfoData.size();
		final double[] result = new double[EnvironmentFeature.values().length
				+ pieceCount * EntityFeature.values().length];

		// The initial feature vector is player-agnostic, so the "speaking
		// player role" feature is not set
		final PlayerRole speakingPlayerRole = null;
		int currentFeatureIdx = setEnvironmentFeatureVals(result, 0, modelDims, pieceCount, speakingPlayerRole);
		// No entity is selected at the beginning of the game
		final boolean isEntitySelected = false;

		for (final ListIterator<ImageVisualizationInfoDescription.Datum> imgVizInfoDataIter = imgVizInfoData
				.listIterator(); imgVizInfoDataIter.hasNext();) {
			final int pieceId = imgVizInfoDataIter.nextIndex();
			final ImageVisualizationInfoDescription.Datum pieceImgVizInfoDatum = imgVizInfoDataIter.next();
			final SpatialRegion pieceRegion = piecePlacements.getElementMinimalRegions().get(pieceId);
			currentFeatureIdx = setEntityFeatureVals(result, currentFeatureIdx, pieceImgVizInfoDatum, pieceRegion,
					modelDims, modelArea, isEntitySelected);
		}
		return result;
	}
}