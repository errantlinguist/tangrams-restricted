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

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

import se.kth.speech.hat.xsd.Annotation.Segments.Segment;
import se.kth.speech.hat.xsd.Transcription.T;

public final class SegmentUtteranceFactory implements Function<Segment, List<Utterance>> {

	private static final float DEFAULT_MIN_SEG_SPACING;

	static {
		DEFAULT_MIN_SEG_SPACING = 1.0f / SegmentTimes.TIME_TO_MILLS_FACTOR;
	}

	private final float minSegmentSpacing;

	public SegmentUtteranceFactory() {
		this(DEFAULT_MIN_SEG_SPACING);
	}

	public SegmentUtteranceFactory(final float minSegmentSpacing) {
		this.minSegmentSpacing = minSegmentSpacing;
	}

	@Override
	public List<Utterance> apply(final Segment segment) {
		final List<Utterance> result = new ArrayList<>();
		final List<Object> children = segment.getTranscription().getSegmentOrT();
		final Float initialPrevUttEndTime = segment.getStart();
		assert initialPrevUttEndTime != null;
		{
			float prevUttEndTime = initialPrevUttEndTime;
			List<T> currentTokenSeq = new ArrayList<>();
			final String parentSegmentId = segment.getId();
			for (final Object child : children) {
				if (child instanceof Segment) {
					final List<Utterance> childUtts = apply((Segment) child);
					// If there was a contiguous sequence of terminal tokens
					// preceding this segment, finish building it
					if (!currentTokenSeq.isEmpty()) {
						final float nextUttStartTime = childUtts.get(0).getStartTime();
						result.add(createUtterance(parentSegmentId, currentTokenSeq, prevUttEndTime, nextUttStartTime));
						currentTokenSeq = new ArrayList<>();
					}
					// Add the newly-created child utterances after adding the
					// now-completed terminal sequence
					result.addAll(childUtts);
					prevUttEndTime = childUtts.get(childUtts.size() - 1).getEndTime();
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
				result.add(createUtterance(parentSegmentId, currentTokenSeq, prevUttEndTime, uttEndTime));
			}
		}

		return result;
	}

	private Utterance createUtterance(final String segmentId, final List<T> tokens, final float previousUttEndTime,
			final float nextUttStartTime) {
		final Float firstTokenStartTime = tokens.get(0).getStart();
		final float seqStartTime = firstTokenStartTime == null ? previousUttEndTime + minSegmentSpacing
				: firstTokenStartTime;
		final Float lastTokenEndTime = tokens.get(tokens.size() - 1).getEnd();
		final float seqEndTime = lastTokenEndTime == null ? nextUttStartTime - minSegmentSpacing : lastTokenEndTime;
		final List<String> tokenForms = tokens.stream().map(T::getContent)
				.collect(Collectors.toCollection(() -> new ArrayList<>(tokens.size())));
		return new Utterance(segmentId, tokenForms, seqStartTime, seqEndTime);
	}

}