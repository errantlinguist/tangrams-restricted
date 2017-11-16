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

import java.io.IOException;
import java.io.UncheckedIOException;
import java.lang.ref.Reference;
import java.lang.ref.SoftReference;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.github.errantlinguist.ClassProperties;

import it.unimi.dsi.fastutil.objects.ObjectOpenHashSet;
import it.unimi.dsi.fastutil.objects.ObjectSet;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.hat.xsd.Annotation.Segments.Segment;
import se.kth.speech.hat.xsd.Transcription;
import se.kth.speech.hat.xsd.Transcription.T;

public final class SegmentUtteranceFactory {

	private static class TokenListSingletonFactory implements Function<String[], List<String>> {

		private final ConcurrentMap<List<String>, Reference<List<String>>> singletonInstances;

		private TokenListSingletonFactory(final int expectedUniqueTokenSequenceCount) {
			singletonInstances = new ConcurrentHashMap<>(expectedUniqueTokenSequenceCount);
		}

		@Override
		public List<String> apply(final String[] tokens) {
			return apply(Arrays.asList(tokens));
		}

		private List<String> apply(final List<String> tokens) {
			return singletonInstances.compute(tokens, (key, oldValue) -> {
				final Reference<List<String>> newValue;
				if (oldValue == null || oldValue.get() == null) {
					final List<String> internedTokens = Arrays
							.asList(key.stream().map(String::intern).toArray(String[]::new));
					newValue = new SoftReference<>(Collections.unmodifiableList(internedTokens));
				} else {
					newValue = oldValue;
				}
				return newValue;
			}).get();
		}

	}

	private static final Function<Segment, String> DEFAULT_SEG_SPEAKER_ID_FACTORY = seg -> seg.getSource().intern();

	private static final Predicate<String> DEFAULT_TOKEN_FILTER = token -> !isMetaLanguageToken(token);

	private static final List<T> EMPTY_TOKEN_LIST = Collections.emptyList();

	private static final int EXPECTED_UNIQUE_TOKEN_SEQUENCES = 2048;

	private static final Logger LOGGER = LoggerFactory.getLogger(SegmentUtteranceFactory.class);

	private static final ObjectSet<String> META_LANGUAGE_TOKENS;

	private static final Comparator<Segment> TEMPORAL_SEGMENT_COMPARATOR = Comparator.comparingDouble(Segment::getStart)
			.thenComparingDouble(Segment::getEnd).thenComparing(Segment::getId);

	static {
		try {
			META_LANGUAGE_TOKENS = readMetaLanguageTokenSet();
		} catch (final IOException e) {
			throw new UncheckedIOException(e);
		}
	}

	public static boolean isMetaLanguageToken(final String token) {
		return META_LANGUAGE_TOKENS.contains(token);
	}

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

	private static ObjectSet<String> readMetaLanguageTokenSet() throws IOException {
		final Properties props = ClassProperties.load(SegmentUtteranceFactory.class);
		final String metaLangTokenStr = props.getProperty("metaLanguageTokens");
		final String[] metaLangTokenArr = metaLangTokenStr.split(",");
		final ObjectOpenHashSet<String> result = new ObjectOpenHashSet<>(metaLangTokenArr.length, 1.0f);
		for (final String token : metaLangTokenArr) {
			result.add(token);
		}
		result.trim();
		return result;
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

	/**
	 * @return the metaLanguageTokens
	 */
	static Set<String> getMetaLanguageTokens() {
		return META_LANGUAGE_TOKENS;
	}

	private final Function<? super Segment, String> segmentSpeakerIdFactory;

	private final Predicate<? super String> tokenFilter;

	private final Function<? super String[], List<String>> tokenListSingletonFactory;

	public SegmentUtteranceFactory() {
		this(DEFAULT_SEG_SPEAKER_ID_FACTORY);
	}

	public SegmentUtteranceFactory(final Function<? super Segment, String> segmentSpeakerIdFactory) {
		this(segmentSpeakerIdFactory, DEFAULT_TOKEN_FILTER);
	}

	public SegmentUtteranceFactory(final Function<? super Segment, String> segmentSpeakerIdFactory,
			final Predicate<? super String> tokenFilter) {
		this(segmentSpeakerIdFactory, tokenFilter, new TokenListSingletonFactory(EXPECTED_UNIQUE_TOKEN_SEQUENCES));
	}

	public SegmentUtteranceFactory(final Function<? super Segment, String> segmentSpeakerIdFactory,
			final Predicate<? super String> tokenFilter,
			final Function<? super String[], List<String>> tokenListSingletonFactory) {
		this.segmentSpeakerIdFactory = segmentSpeakerIdFactory;
		this.tokenFilter = tokenFilter;
		this.tokenListSingletonFactory = tokenListSingletonFactory;
	}

	public SegmentUtteranceFactory(final Predicate<? super String> tokenFilter) {
		this(DEFAULT_SEG_SPEAKER_ID_FACTORY, tokenFilter);
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
						.filter(token -> !token.isEmpty()).filter(tokenFilter).toArray(String[]::new);
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