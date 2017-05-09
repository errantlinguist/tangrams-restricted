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
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.regex.Pattern;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.hat.xsd.Annotation.Segments.Segment;
import se.kth.speech.hat.xsd.Transcription;
import se.kth.speech.hat.xsd.Transcription.T;

public final class SegmentUtteranceFactory {

	private static final Logger LOGGER = LoggerFactory.getLogger(SegmentUtteranceFactory.class);

	private static final Set<String> META_LANGUAGE_TOKENS = new HashSet<>(
			Arrays.asList("BREATH", "CLICK", "COUGH", "LAUGHTER", "META", "NOISE", "SNIFF", "SWEDISH", "UNKNOWN"));

	private static final Comparator<Segment> TEMPORAL_SEGMENT_COMPARATOR = Comparator.comparingDouble(Segment::getStart)
			.thenComparingDouble(Segment::getEnd).thenComparing(seg -> seg.getTranscription().getSegmentOrT().size()).thenComparing(Segment::getSource).thenComparing(Segment::getTrack)
			.thenComparing(Segment::getId);

	private static final Collector<CharSequence, ?, String> TOKEN_JOINING_COLLECTOR = Collectors.joining(" ");

	// private static Function<String, List<String>> createDefaultTokenizer() {
	// final Predicate<String> nonMetaLanguagePredicate = token ->
	// !META_LANGUAGE_TOKENS.contains(token);
	// final StanfordNLPTokenizer stanfordTokenizer = new
	// StanfordNLPTokenizer(nonMetaLanguagePredicate);
	// return stanfordTokenizer;
	// }

	private static final Pattern WHITESPACE_PATTERN = Pattern.compile("\\s+");

	// private static Function<String, Stream<String>> createDefaultTokenizer()
	// {
	// final Analyzer analyer = new BlacklistingAnalyzer(new
	// CharArraySet(META_LANGUAGE_TOKENS, false));
	// final StringTokenizer tokenizer = new StringTokenizer(analyer);
	// return str -> tokenizer.apply(null, str).collect(Collectors.toList());
	// }

	private static Function<String, List<String>> createDefaultTokenizer() {
		final Predicate<String> nonMetaLanguagePredicate = token -> !META_LANGUAGE_TOKENS.contains(token);
		return createWhitelistingTokenizer(nonMetaLanguagePredicate);
	}

	private static Function<String, List<String>> createWhitelistingTokenizer(
			final Predicate<String> whitelistingPredicate) {
		return str -> Arrays
				.asList(WHITESPACE_PATTERN.splitAsStream(str).filter(whitelistingPredicate).toArray(String[]::new));
	}

	static void addSegmentTokens(final ArrayList<? super T> tokens, final Collection<Object> children) {
		tokens.ensureCapacity(tokens.size() + children.size());
		for (final Object child : children) {
			if (child instanceof Segment) {
				addSegmentTokens(tokens, ((Segment) child).getTranscription().getSegmentOrT());
			} else {
				tokens.add((T) child);
			}
		}
	}

	static List<T> createSegmentTokenList(final Segment seg) {
		final List<Object> children = seg.getTranscription().getSegmentOrT();
		final ArrayList<T> result = new ArrayList<>(Math.max(children.size(), 16));
		addSegmentTokens(result, children);
		return result;
	}

	private final Function<? super String, ? extends List<String>> tokenizer;

	public SegmentUtteranceFactory() {
		this(createDefaultTokenizer());
	}

	public SegmentUtteranceFactory(final Function<? super String, ? extends List<String>> tokenizer) {
		this.tokenizer = tokenizer;
	}

	public SegmentUtteranceFactory(final Predicate<String> whitelistingPredicate) {
		this(createWhitelistingTokenizer(whitelistingPredicate));
	}

	public List<Utterance> create(final Segment segment) {
		final List<Utterance> result = new ArrayList<>(1);
		final Transcription transcription = segment.getTranscription();
		if (transcription == null) {
			LOGGER.debug("Segment \"{}\" has no {} element.",
					new Object[] { segment.getId(), Transcription.class.getSimpleName() });
		} else {
			final List<Object> children = transcription.getSegmentOrT();
			// TODO: make this recursive
			children.stream().filter(child -> child instanceof Segment).findAny().ifPresent(childSeg -> {
				LOGGER.warn(
						"Segment \"{}\" contains child {} instances; Multi-level transcriptions not (yet) supported.",
						new Object[] { segment.getId(), Segment.class.getSimpleName() });
			});
			final Float segStartTime = segment.getStart();
			assert segStartTime != null;
			final Float segEndTime = segment.getEnd();
			assert segEndTime != null;
			{
				final ArrayList<T> tokens = new ArrayList<>(Math.max(children.size(), 16));
				final String parentSegmentId = segment.getId();
				for (final Object child : children) {
					if (child instanceof Segment) {
						final Segment childSeg = (Segment) child;
						addSegmentTokens(tokens, childSeg.getTranscription().getSegmentOrT());
					} else {
						tokens.add((T) child);
					}
				}
				final String uttRepr = tokens.stream().map(T::getContent).collect(TOKEN_JOINING_COLLECTOR);
				final List<String> tokenForms = tokenizer.apply(uttRepr);
				if (tokenForms.isEmpty()){
					// Do nothing
				} else {
					final Utterance utt = new Utterance(parentSegmentId, tokenForms, segStartTime.doubleValue(),
							segEndTime.doubleValue());
					result.add(utt);
				}
			}
		}

		return result;
	}

	public Stream<List<Utterance>> create(final Stream<Segment> segments) {
		return segments.sorted(TEMPORAL_SEGMENT_COMPARATOR).map(this::create);
	}

	// private Optional<Utterance> create(final String segmentId, final List<T>
	// tokenAnnots,
	// final double previousUttEndTime, final double nextUttStartTime) {
	// final Float firstTokenStartTime = tokenAnnots.get(0).getStart();
	// final double seqStartTime = firstTokenStartTime == null ?
	// previousUttEndTime : firstTokenStartTime;
	// final Float lastTokenEndTime = tokenAnnots.get(tokenAnnots.size() -
	// 1).getEnd();
	// final double seqEndTime = lastTokenEndTime == null ? nextUttStartTime :
	// lastTokenEndTime;
	// final Stream<String> tokenForms =
	// tokenAnnots.stream().map(T::getContent);
	// final List<String> tokens =
	// tokenizer.apply(tokenForms.collect(TOKEN_JOINING_COLLECTOR));
	// return tokens.isEmpty() ? Optional.empty()
	// : Optional.of(new Utterance(segmentId, tokens, seqStartTime,
	// seqEndTime));
	// }

}