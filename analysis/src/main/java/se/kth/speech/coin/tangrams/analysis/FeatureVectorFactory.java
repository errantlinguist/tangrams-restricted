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
import java.util.NavigableMap;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import iristk.system.Event;
import it.unimi.dsi.fastutil.objects.Object2DoubleMap;
import it.unimi.dsi.fastutil.objects.Object2DoubleOpenHashMap;
import se.kth.speech.TimestampArithmetic;
import se.kth.speech.coin.tangrams.game.PlayerRole;
import se.kth.speech.hat.xsd.Annotation.Segments.Segment;
import se.kth.speech.hat.xsd.Transcription.T;

final class FeatureVectorFactory implements Function<Segment, double[][]> {

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

	private final Map<String, GameStateChangeData> playerStateChangeData;

	private final Map<String, String> sourceIdPlayerIds;

	public FeatureVectorFactory(final Map<String, String> sourceIdPlayerIds,
			final Map<String, GameStateChangeData> playerStateChangeData) {
		this.sourceIdPlayerIds = sourceIdPlayerIds;
		this.playerStateChangeData = playerStateChangeData;

		gameStateFeatureVectorFactory = new GameStateFeatureVectorFactory(playerStateChangeData.values().size());
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
		final double[] gameStateFeatures = gameStateFeatureVectorFactory.apply(gameStateChangeData, gameTime);
		final double[] result = Arrays.copyOf(gameStateFeatures, gameStateFeatures.length);
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
}