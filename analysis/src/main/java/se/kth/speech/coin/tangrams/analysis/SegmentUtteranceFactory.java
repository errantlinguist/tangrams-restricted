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
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.higgins._2005.annotation.Annotation.Segments.Segment;
import se.kth.speech.higgins._2005.annotation.Transcription;
import se.kth.speech.higgins._2005.annotation.Transcription.T;

public final class SegmentUtteranceFactory {

	private static final Function<Segment, String> DEFAULT_SEG_SPEAKER_ID_FACTORY = seg -> seg.getSource().intern();

	private static final List<T> EMPTY_TOKEN_LIST = Collections.emptyList();

	private static final int EXPECTED_UNIQUE_TOKEN_SEQUENCES = 2048;

	private static final Logger LOGGER = LoggerFactory.getLogger(SegmentUtteranceFactory.class);

	private static final Comparator<Segment> TEMPORAL_SEGMENT_COMPARATOR = Comparator.comparingDouble(Segment::getStart)
			.thenComparingDouble(Segment::getEnd).thenComparing(Segment::getId);

	private static void addSegmentTokens(final ArrayList<? super T> tokens, final Collection<Object> children) {
		tokens.ensureCapacity(tokens.size() + children.size());
		for (final Object child : children) {
			if (child instanceof Segment) {
				addSegmentTokens(tokens, ((Segment) child).getTranscription().getSegmentOrT());
			} else {
				tokens.add((T) child);
			}
		}
	}

	private static void addSegmentTokens(final ArrayList<? super T> tokens, final Segment segment) {
		final Transcription transcription = segment.getTranscription();
		if (transcription == null) {
			LOGGER.warn("Segment ID \"{}\" has no {} element.", segment.getId(), Transcription.class.getSimpleName());
		} else {
			addSegmentTokens(tokens, transcription.getSegmentOrT());
		}
	}

	private static int estimateTokenCount(final List<Object> children) {
		return Math.max(children.size(), 16);
	}

	static List<T> createSegmentTokenList(final Segment segment) {
		final Transcription transcription = segment.getTranscription();
		final List<T> result;
		if (transcription == null) {
			LOGGER.warn("Segment ID \"{}\" has no {} element.", segment.getId(), Transcription.class.getSimpleName());
			result = EMPTY_TOKEN_LIST;
		} else {
			final List<Object> children = transcription.getSegmentOrT();
			final ArrayList<T> resultImpl = new ArrayList<>(estimateTokenCount(children));
			addSegmentTokens(resultImpl, children);
			result = resultImpl;
		}
		return result;
	}

	private final Function<? super Segment, String> segmentSpeakerIdFactory;

	private final Function<? super String[], List<String>> tokenListSingletonFactory;

	public SegmentUtteranceFactory() {
		this(DEFAULT_SEG_SPEAKER_ID_FACTORY);
	}

	public SegmentUtteranceFactory(final Function<? super Segment, String> segmentSpeakerIdFactory) {
		this(segmentSpeakerIdFactory, new TokenListSingletonFactory(EXPECTED_UNIQUE_TOKEN_SEQUENCES));
	}

	public SegmentUtteranceFactory(final Function<? super Segment, String> segmentSpeakerIdFactory,
			final Function<? super String[], List<String>> tokenListSingletonFactory) {
		this.segmentSpeakerIdFactory = segmentSpeakerIdFactory;
		this.tokenListSingletonFactory = tokenListSingletonFactory;
	}

	public List<Utterance> create(final Segment segment) {
		final List<Utterance> result = new ArrayList<>(1);
		final Transcription transcription = segment.getTranscription();
		if (transcription == null) {
			LOGGER.warn("Segment ID \"{}\" has no {} element.", segment.getId(), Transcription.class.getSimpleName());
		} else {
			final List<Object> children = transcription.getSegmentOrT();
			// TODO: make this recursively create individual Utterances for each
			// child Segment
			final Float segStartTime = segment.getStart();
			assert segStartTime != null;
			final Float segEndTime = segment.getEnd();
			assert segEndTime != null;
			{
				final ArrayList<T> tokens = new ArrayList<>(estimateTokenCount(children));
				final String parentSegmentId = segment.getId();
				final String speakerId = segmentSpeakerIdFactory.apply(segment);
				for (final Object child : children) {
					if (child instanceof Segment) {
						LOGGER.warn(
								"Segment ID \"{}\" contains child {} instances; Multi-level transcriptions not (yet) supported.",
								segment.getId(), Segment.class.getSimpleName());
						final Segment childSeg = (Segment) child;
						addSegmentTokens(tokens, childSeg);
					} else {
						tokens.add((T) child);
					}
				}
				final String[] contentTokens = tokens.stream().map(T::getContent).map(String::trim)
						.filter(token -> !token.isEmpty()).toArray(String[]::new);
				if (contentTokens.length < 1) {
					LOGGER.debug("Segment ID \"{}\" does not have any content tokens; Ignoring.", segment.getId());
				} else {
					final Utterance utt = new Utterance(parentSegmentId, speakerId,
							tokenListSingletonFactory.apply(contentTokens), segStartTime, segEndTime);
					result.add(utt);
				}
			}
		}

		return result;
	}

	public Stream<List<Utterance>> create(final Stream<Segment> segments) {
		return segments.sorted(TEMPORAL_SEGMENT_COMPARATOR).map(this::create);
	}

}