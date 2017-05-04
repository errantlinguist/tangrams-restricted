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
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
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

public final class SegmentUtteranceFactory implements Function<Segment, List<Utterance>> {

	private static final float DEFAULT_MIN_SEG_SPACING;

	private static final Logger LOGGER = LoggerFactory.getLogger(SegmentUtteranceFactory.class);

	private static final Set<String> META_LANGUAGE_TOKENS = new HashSet<>(
			Arrays.asList("BREATH", "CLICK", "COUGH", "LAUGHTER", "META", "NOISE", "SNIFF", "SWEDISH"));

	private static final Collector<CharSequence, ?, String> TOKEN_JOINING_COLLECTOR = Collectors.joining(" ");

	static {
		DEFAULT_MIN_SEG_SPACING = 1.0f / SegmentTimes.TIME_TO_MILLS_FACTOR;
	}

	// private static Function<String, List<String>> createDefaultTokenizer() {
	// final Predicate<String> nonMetaLanguagePredicate = token ->
	// !META_LANGUAGE_TOKENS.contains(token);
	// final StanfordNLPTokenizer stanfordTokenizer = new
	// StanfordNLPTokenizer(nonMetaLanguagePredicate);
	// return stanfordTokenizer;
	// }

	private static final Pattern WHITESPACE_PATTERN = Pattern.compile("\\s+");

	private static Function<String, List<String>> createDefaultTokenizer() {
		final Predicate<String> nonMetaLanguagePredicate = token -> !META_LANGUAGE_TOKENS.contains(token);
		return str -> Arrays
				.asList(WHITESPACE_PATTERN.splitAsStream(str).filter(nonMetaLanguagePredicate).toArray(String[]::new));
	}

	// private static Function<String, Stream<String>> createDefaultTokenizer()
	// {
	// final Analyzer analyer = new BlacklistingAnalyzer(new
	// CharArraySet(META_LANGUAGE_TOKENS, false));
	// final StringTokenizer tokenizer = new StringTokenizer(analyer);
	// return str -> tokenizer.apply(null, str).collect(Collectors.toList());
	// }

	private final float minSegmentSpacing;

	private final Function<? super String, ? extends List<String>> tokenizer;

	public SegmentUtteranceFactory() {
		this(createDefaultTokenizer());
	}

	public SegmentUtteranceFactory(final Function<? super String, ? extends List<String>> tokenizer) {
		this(tokenizer, DEFAULT_MIN_SEG_SPACING);
	}

	public SegmentUtteranceFactory(final Function<? super String, ? extends List<String>> tokenizer,
			final float minSegmentSpacing) {
		this.tokenizer = tokenizer;
		this.minSegmentSpacing = minSegmentSpacing;
	}

	@Override
	public List<Utterance> apply(final Segment segment) {
		final List<Utterance> result;
		final Transcription transcription = segment.getTranscription();
		if (transcription == null) {
			result = Collections.emptyList();
			LOGGER.debug("Segment \"{}\" has no {} element: {}",
					new Object[] { segment.getId(), Transcription.class.getSimpleName() });
		} else {
			final List<Object> children = transcription.getSegmentOrT();
			result = new ArrayList<>(Math.max(children.size(), 16));
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
							createUtterance(parentSegmentId, currentTokenSeq, prevUttEndTime, nextUttStartTime)
									.ifPresent(result::add);
							currentTokenSeq = new ArrayList<>();
						}
						// Add the newly-created child utterances after adding
						// the
						// now-completed terminal sequence
						result.addAll(childUtts);
						prevUttEndTime = childUtts.get(childUtts.size() - 1).getEndTime();
					} else if (child instanceof T) {
						currentTokenSeq.add((T) child);
					} else {
						throw new IllegalArgumentException(String.format(
								"Could not parse child annotation of type \"%s\".", child.getClass().getName()));
					}
				}
				if (!currentTokenSeq.isEmpty()) {
					// Add the last token sequence
					final Float uttEndTime = segment.getEnd();
					assert uttEndTime != null;
					createUtterance(parentSegmentId, currentTokenSeq, prevUttEndTime, uttEndTime)
							.ifPresent(result::add);
				}
			}
		}

		return result;
	}

	private Optional<Utterance> createUtterance(final String segmentId, final List<T> tokenAnnots,
			final float previousUttEndTime, final float nextUttStartTime) {
		final Float firstTokenStartTime = tokenAnnots.get(0).getStart();
		final float seqStartTime = firstTokenStartTime == null ? previousUttEndTime + minSegmentSpacing
				: firstTokenStartTime;
		final Float lastTokenEndTime = tokenAnnots.get(tokenAnnots.size() - 1).getEnd();
		final float seqEndTime = lastTokenEndTime == null ? nextUttStartTime - minSegmentSpacing : lastTokenEndTime;
		final Stream<String> tokenForms = tokenAnnots.stream().map(T::getContent);
		final List<String> tokens = tokenizer.apply(tokenForms.collect(TOKEN_JOINING_COLLECTOR));
		return tokens.isEmpty() ? Optional.empty()
				: Optional.of(new Utterance(segmentId, tokens, seqStartTime, seqEndTime));
	}

}