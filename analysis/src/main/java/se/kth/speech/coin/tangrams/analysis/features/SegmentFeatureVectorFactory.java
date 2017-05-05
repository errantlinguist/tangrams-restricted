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

import java.util.List;
import java.util.Map;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.stream.DoubleStream;
import java.util.stream.Stream;

import se.kth.speech.coin.tangrams.analysis.GameContext;
import se.kth.speech.coin.tangrams.analysis.SegmentUtteranceFactory;
import se.kth.speech.coin.tangrams.analysis.Utterance;
import se.kth.speech.coin.tangrams.analysis.TemporalGameContextFactory;
import se.kth.speech.hat.xsd.Annotation.Segments.Segment;

final class SegmentFeatureVectorFactory implements Function<Segment, Stream<DoubleStream>> {

	private static final SegmentUtteranceFactory SEG_UTT_FACTORY = new SegmentUtteranceFactory();

	private final List<? extends BiConsumer<? super GameContext, ? super DoubleStream.Builder>> contextFeatureExtractors;

	private final Map<String, String> sourceIdPlayerIds;

	private final TemporalGameContextFactory uttContextFactory;

	private final List<? extends BiConsumer<? super Utterance, ? super DoubleStream.Builder>> uttFeatureExtractors;

	public SegmentFeatureVectorFactory(final Map<String, String> sourceIdPlayerIds,
			final TemporalGameContextFactory uttContextFactory,
			final List<? extends BiConsumer<? super GameContext, ? super DoubleStream.Builder>> contextFeatureExtractors,
			final List<? extends BiConsumer<? super Utterance, ? super DoubleStream.Builder>> uttFeatureExtractors) {
		this.sourceIdPlayerIds = sourceIdPlayerIds;
		this.uttContextFactory = uttContextFactory;
		this.contextFeatureExtractors = contextFeatureExtractors;
		this.uttFeatureExtractors = uttFeatureExtractors;
	}

	@Override
	public Stream<DoubleStream> apply(final Segment segment) {
		final List<Utterance> utts = SEG_UTT_FACTORY.create(segment);
		final String sourceId = segment.getSource();
		// Get the player ID associated with the given audio source
		final String playerId = sourceIdPlayerIds.get(sourceId);
		final Stream<Stream<DoubleStream>> cxtVectors = utts.stream().map(utt -> {
			final Stream<GameContext> uttContexts = uttContextFactory.apply(utt.getStartTime(), utt.getEndTime(),
					playerId);
			return uttContexts.map(uttContext -> {
				final DoubleStream.Builder featureVectorBuilder = DoubleStream.builder();
				contextFeatureExtractors.forEach(extractor -> extractor.accept(uttContext, featureVectorBuilder));
				uttFeatureExtractors.forEach(extractor -> extractor.accept(utt, featureVectorBuilder));
				return featureVectorBuilder.build();
			});
		});
		return cxtVectors.flatMap(Function.identity());
	}

}