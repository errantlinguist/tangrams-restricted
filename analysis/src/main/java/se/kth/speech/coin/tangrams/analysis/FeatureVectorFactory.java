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

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NavigableMap;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Lists;

import iristk.system.Event;
import it.unimi.dsi.fastutil.objects.Object2DoubleMap;
import it.unimi.dsi.fastutil.objects.Object2DoubleOpenHashMap;
import se.kth.speech.TimestampArithmetic;
import se.kth.speech.coin.tangrams.iristk.EventSubmittingPlayerMatcher;
import se.kth.speech.coin.tangrams.iristk.EventTypeMatcher;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;
import se.kth.speech.coin.tangrams.iristk.events.GameStateDescription;
import se.kth.speech.hat.xsd.Annotation.Segments.Segment;
import se.kth.speech.hat.xsd.Transcription.T;

final class FeatureVectorFactory implements Function<Segment, double[][]> {

	private enum PlayerFeature {
		LAST_MOVE_SUBMISSION_DISTANCE, LAST_SPEAKING_PLAYER_MOVE_SUBMISSION_DISTANCE, LAST_SUBMITTED_EVENT_TYPE;

		private static final List<GameManagementEvent> EVENT_TYPE_FEATURE_ORDERING;

		private static final Object2DoubleMap<GameManagementEvent> EVENT_TYPE_FEATURE_VALS;

		private static final List<PlayerFeature> ORDERING;

		static {
			ORDERING = Arrays.asList(LAST_MOVE_SUBMISSION_DISTANCE, LAST_SPEAKING_PLAYER_MOVE_SUBMISSION_DISTANCE, LAST_SUBMITTED_EVENT_TYPE);
			assert ORDERING.size() == PlayerFeature.values().length;
		}

		static {
			EVENT_TYPE_FEATURE_ORDERING = Arrays.asList(GameManagementEvent.COMPLETED_TURN_REQUEST,
					GameManagementEvent.GAME_READY_RESPONSE, GameManagementEvent.NEXT_TURN_REQUEST,
					GameManagementEvent.PLAYER_JOIN_REQUEST, GameManagementEvent.PLAYER_JOIN_RESPONSE,
					GameManagementEvent.SELECTION_REJECTION, GameManagementEvent.SELECTION_REQUEST);
			assert EVENT_TYPE_FEATURE_ORDERING.size() == GameManagementEvent.values().length;
			EVENT_TYPE_FEATURE_VALS = new Object2DoubleOpenHashMap<>(EVENT_TYPE_FEATURE_ORDERING.size() + 1);
			FeatureMaps.putOrdinalFeatureVals(EVENT_TYPE_FEATURE_VALS, EVENT_TYPE_FEATURE_ORDERING);
			EVENT_TYPE_FEATURE_VALS.put(null, -1.0);
		}

		private static int findNearestEventDistance(
				final Iterable<? extends Entry<Timestamp, ? extends List<? extends Event>>> timedEventLists,
				final Predicate<? super Event> eventMatcher) {
			int result = -1;
			int currentDistance = 0;
			for (final Entry<Timestamp, ? extends List<? extends Event>> timedEventList : timedEventLists) {
				LOGGER.debug("Checking events at time \"{}\".", timedEventList.getKey());
				final List<? extends Event> eventsReversed = Lists.reverse(timedEventList.getValue());
				for (final Event event : eventsReversed) {
					if (eventMatcher.test(event)) {
						LOGGER.debug("Found matching event at \"{}\".", timedEventList.getKey());
						result = currentDistance;
						break;
					}
					currentDistance++;
				}
			}
			return result;
		}

		private static int getTotalFeatureCount() {
			return PlayerFeature.values().length;
		}

		private static int setVals(final double[] vals, int currentFeatureIdx, final GameStateChangeData gameData,
				final Timestamp time, final String speakingPlayerId) {
			for (final PlayerFeature feature : ORDERING) {
				switch (feature) {
				case LAST_MOVE_SUBMISSION_DISTANCE: {
					//FIXME: Distance just keeps increasing
					final NavigableMap<Timestamp, List<Event>> timedEventsBeforeUtt = gameData.getEvents().headMap(time,
							true);
					// Look for the last time a move was submitted, iterating backwards
					final Predicate<Event> lastMoveSubmissionMatcher = new EventTypeMatcher(
							GameManagementEvent.NEXT_TURN_REQUEST);
					final int lastMoveSubmissionDistance = findNearestEventDistance(
							timedEventsBeforeUtt.descendingMap().entrySet(), lastMoveSubmissionMatcher);
					vals[currentFeatureIdx] = lastMoveSubmissionDistance;
					break;
				}
				case LAST_SPEAKING_PLAYER_MOVE_SUBMISSION_DISTANCE: {
					//FIXME: Distance just keeps increasing
					final NavigableMap<Timestamp, List<Event>> timedEventsBeforeUtt = gameData.getEvents().headMap(time,
							true);
					// Look for the last time the speaking player submitted a
					// move, iterating backwards
					final Predicate<Event> lastMoveSubmissionByPlayerMatcher = new EventTypeMatcher(
							GameManagementEvent.NEXT_TURN_REQUEST)
									.and(new EventSubmittingPlayerMatcher(speakingPlayerId));
					final int lastMoveSubmissionDistance = findNearestEventDistance(
							timedEventsBeforeUtt.descendingMap().entrySet(), lastMoveSubmissionByPlayerMatcher);
					vals[currentFeatureIdx] = lastMoveSubmissionDistance;
					break;
				}
				case LAST_SUBMITTED_EVENT_TYPE: {
					final NavigableMap<Timestamp, List<Event>> timedEventsBeforeUtt = gameData.getEvents().headMap(time,
							true);
					// Look for the last time the speaking player submitted an
					// event, iterating backwards
					final Stream<Event> eventsReversed = timedEventsBeforeUtt.descendingMap().values().stream()
							.map(Lists::reverse).flatMap(List::stream);
					final Predicate<Event> lastSubmittedByPlayerMatcher = new EventSubmittingPlayerMatcher(
							speakingPlayerId);
					final Optional<Event> lastEventSubmittedByPlayer = eventsReversed
							.filter(lastSubmittedByPlayerMatcher).findFirst();
					final GameManagementEvent lastEventType = lastEventSubmittedByPlayer.isPresent()
							? GameManagementEvent.getEventType(lastEventSubmittedByPlayer.get()) : null;
					final double val = EVENT_TYPE_FEATURE_VALS.get(lastEventType);
					vals[currentFeatureIdx] = val;
					break;
				}
				default: {
					throw new AssertionError("Missing enum-handling logic.");
				}
				}
				currentFeatureIdx++;
			}
			return currentFeatureIdx;
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

	private static final int SEGMENT_TIME_TO_MILLS_FACTOR;

	static {
		SEGMENT_TIME_TO_MILLS_FACTOR = 1000;
		DEFAULT_MIN_SEGMENT_SPACING = 1.0f / SEGMENT_TIME_TO_MILLS_FACTOR;
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

	private final GameStateFeatureVectorFactory gameStateFeatureVectorFactory;

	private final int nonGameStateFeatureCount;

	private final Map<String, GameStateChangeData> playerStateChangeData;

	private final Map<String, String> sourceIdPlayerIds;

	public FeatureVectorFactory(final Map<String, String> sourceIdPlayerIds,
			final Map<String, GameStateChangeData> playerStateChangeData) {
		this.sourceIdPlayerIds = sourceIdPlayerIds;
		this.playerStateChangeData = playerStateChangeData;

		nonGameStateFeatureCount = PlayerFeature.getTotalFeatureCount();
		gameStateFeatureVectorFactory = new GameStateFeatureVectorFactory(playerStateChangeData.values().size(),
				nonGameStateFeatureCount);
	}

	@Override
	public double[][] apply(final Segment segment) {
		final List<TimestampedUtterance> utts = createTimestampedUtterances(segment);
		final String sourceId = segment.getSource();
		// Get the player ID associated with the given audio source
		final String playerId = sourceIdPlayerIds.get(sourceId);
		final Stream<double[]> featureVectors = utts.stream().map(utt -> createFeatureVector(utt, playerId));
		return featureVectors.toArray(double[][]::new);
	}

	public Stream<String> createFeatureDescriptions(final GameStateDescription initialState) {
		final Stream<String> gameStateFeatureDescs = gameStateFeatureVectorFactory
				.createFeatureDescriptions(initialState);
		final Stream<String> playerFeatureDescs = PlayerFeature.ORDERING.stream().map(Enum::toString);
		return Stream.concat(gameStateFeatureDescs, playerFeatureDescs);
	}

	private double[] createFeatureVector(final TimestampedUtterance utt, final String playerId) {
		final GameStateChangeData gameStateChangeData = playerStateChangeData.get(playerId);
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

		final double[] gameStateFeatures = gameStateFeatureVectorFactory.apply(gameStateChangeData, uttStartTimestamp);
		int currentFeatureIdx = gameStateFeatures.length - nonGameStateFeatureCount;
		currentFeatureIdx = PlayerFeature.setVals(gameStateFeatures, currentFeatureIdx, gameStateChangeData,
				uttStartTimestamp, playerId);

		final double[] result = Arrays.copyOf(gameStateFeatures, gameStateFeatures.length);
		// TODO: Update e.g. piece position and selection features, player role
		// feature
		return result;
	}
}