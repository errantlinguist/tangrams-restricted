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
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.DoubleStream;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import iristk.system.Event;
import se.kth.speech.TimestampArithmetic;
import se.kth.speech.hat.xsd.Annotation.Segments.Segment;

final class SegmentFeatureVectorFactory implements Function<Segment, Stream<DoubleStream>> {

	private static final Logger LOGGER = LoggerFactory.getLogger(SegmentFeatureVectorFactory.class);

	private static final Function<Segment, List<Utterance>> SEG_UTT_FACTORY;

	private static final int SEGMENT_TIME_TO_MILLS_FACTOR;

	private static final Collector<CharSequence, ?, String> TOKEN_FORM_JOINER = Collectors.joining(" ");

	static {
		SEGMENT_TIME_TO_MILLS_FACTOR = 1000;
		SEG_UTT_FACTORY = new SegmentUtteranceFactory(1.0f / SEGMENT_TIME_TO_MILLS_FACTOR);
	}

	private final Map<String, GameHistory> playerStateChangeData;

	private final Map<String, String> sourceIdPlayerIds;

	private final List<? extends BiConsumer<? super GameContext, ? super DoubleStream.Builder>> contextFeatureExtractors;

	private final List<? extends BiConsumer<? super Utterance, ? super DoubleStream.Builder>> uttFeatureExtractors;

	public SegmentFeatureVectorFactory(final Map<String, String> sourceIdPlayerIds,
			final Map<String, GameHistory> playerStateChangeData,
			final List<? extends BiConsumer<? super GameContext, ? super DoubleStream.Builder>> contextFeatureExtractors,
			final List<? extends BiConsumer<? super Utterance, ? super DoubleStream.Builder>> uttFeatureExtractors) {
		this.sourceIdPlayerIds = sourceIdPlayerIds;
		this.playerStateChangeData = playerStateChangeData;
		this.contextFeatureExtractors = contextFeatureExtractors;
		this.uttFeatureExtractors = uttFeatureExtractors;
	}

	@Override
	public Stream<DoubleStream> apply(final Segment segment) {
		final List<Utterance> utts = SEG_UTT_FACTORY.apply(segment);
		final String sourceId = segment.getSource();
		// Get the player ID associated with the given audio source
		final String playerId = sourceIdPlayerIds.get(sourceId);
		return utts.stream().map(utt -> {
			final GameContext context = createContext(utt, playerId);
			final DoubleStream.Builder featureVectorBuilder = DoubleStream.builder();
			contextFeatureExtractors.forEach(extractor -> extractor.accept(context, featureVectorBuilder));
			uttFeatureExtractors.forEach(extractor -> extractor.accept(utt, featureVectorBuilder));
			return featureVectorBuilder.build();
		});
	}

	private GameContext createContext(final Utterance utt, final String playerId) {
		final GameHistory history = playerStateChangeData.get(playerId);
		final NavigableMap<Timestamp, List<Event>> events = history.getEvents();
		final float uttStartMills = utt.getStartTime() * SEGMENT_TIME_TO_MILLS_FACTOR;
		final Timestamp gameStartTime = history.getStartTime();
		final Timestamp uttStartTimestamp = TimestampArithmetic.createOffsetTimestamp(gameStartTime, uttStartMills);

		final float uttEndMills = utt.getEndTime() * SEGMENT_TIME_TO_MILLS_FACTOR;
		final Timestamp uttEndTimestamp = TimestampArithmetic.createOffsetTimestamp(gameStartTime, uttEndMills);
		final NavigableMap<Timestamp, List<Event>> eventsDuringUtt = events.subMap(uttStartTimestamp, true,
				uttEndTimestamp, true);
		final List<String> tokenForms = utt.getTokens();
		if (!eventsDuringUtt.isEmpty()) {
			final List<Event> allEventsDuringUtt = eventsDuringUtt.values().stream().flatMap(Collection::stream)
					.collect(Collectors.toList());
			if (LOGGER.isDebugEnabled()) {
				final String delim = System.lineSeparator() + '\t';
				final String uttRepr = allEventsDuringUtt.stream().map(Event::toString)
						.collect(Collectors.joining(delim));
				LOGGER.debug("Found {} event(s) during utterance \"{}\" subsequence: \"{}\"" + delim + "{}",
						new Object[] { allEventsDuringUtt.size(), utt.getSegmentId(),
								tokenForms.stream().collect(TOKEN_FORM_JOINER), uttRepr });
			}
			// TODO: estimate partitions for utterance: By phones?
		}
		return new GameContext(history, uttStartTimestamp, playerId);
	}

}