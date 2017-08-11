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
package se.kth.speech.coin.tangrams.analysis.dialogues;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collector;
import java.util.stream.Collectors;

import org.apache.commons.lang3.tuple.Pair;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 19 May 2017
 *
 */
public final class UtteranceDialogueRepresentationStringFactory implements Function<Iterator<Utterance>, String> {

	private static final Collector<CharSequence, ?, String> DEFAULT_SENTENCE_JOINER = Collectors.joining(" ");

	private static final Collector<CharSequence, ?, String> DEFAULT_WORD_JOINER = Collectors.joining(" ");

	private static final char DIALOGUE_TURN_DELIMITER = '"';

	private static void append(final StringBuilder sb, final Entry<String, String> speakerTurnRepr) {
		sb.append(createSpeakerUttPrefix(speakerTurnRepr.getKey()));
		sb.append(DIALOGUE_TURN_DELIMITER);
		sb.append(speakerTurnRepr.getValue());
		sb.append(DIALOGUE_TURN_DELIMITER);
	}

	private static String createSpeakerUttPrefix(final String speakerId) {
		return "**" + speakerId + ":** ";
	}

	private final Collector<? super CharSequence, ?, String> sentenceJoiner;

	private final Locale uttLocale;

	private final Collector<? super CharSequence, ?, String> wordJoiner;

	public UtteranceDialogueRepresentationStringFactory(final Locale uttLocale) {
		this(uttLocale, DEFAULT_WORD_JOINER, DEFAULT_SENTENCE_JOINER);
	}

	private UtteranceDialogueRepresentationStringFactory(final Locale uttLocale,
			final Collector<? super CharSequence, ?, String> wordJoiner,
			final Collector<? super CharSequence, ?, String> sentenceJoiner) {
		this.uttLocale = uttLocale;
		this.wordJoiner = wordJoiner;
		this.sentenceJoiner = sentenceJoiner;
	}

	@Override
	public String apply(final Iterator<Utterance> uttIter) {
		final StringBuilder sb = new StringBuilder(128);
		final List<Entry<String, String>> diagTurnReprs = createDialogTurnReprs(uttIter);
		final Iterator<Entry<String, String>> diagTurnReprIter = diagTurnReprs.iterator();
		if (diagTurnReprIter.hasNext()) {
			append(sb, diagTurnReprIter.next());

			while (diagTurnReprIter.hasNext()) {
				sb.append(' ');
				append(sb, diagTurnReprIter.next());
			}
		}
		return sb.toString();
	}

	/**
	 * @return the sentenceJoiner
	 */
	public Collector<? super CharSequence, ?, String> getSentenceJoiner() {
		return sentenceJoiner;
	}

	/**
	 * @return the wordJoiner
	 */
	public Collector<? super CharSequence, ?, String> getWordJoiner() {
		return wordJoiner;
	}

	private String capitalizeFirstChar(final String str) {
		// http://stackoverflow.com/a/3904607/1391325
		return str.substring(0, 1).toUpperCase(uttLocale) + str.substring(1);
	}

	private List<Entry<String, String>> createDialogTurnReprs(final Iterator<Utterance> uttIter) {
		final List<Entry<String, String>> result = new ArrayList<>();
		if (uttIter.hasNext()) {
			final Utterance firstUtt = uttIter.next();

			String speakerId = firstUtt.getSpeakerId();
			List<String> diagTurnUttReprs = new ArrayList<>();
			diagTurnUttReprs.add(capitalizeFirstChar(firstUtt.getTokens().stream().collect(wordJoiner)) + '.');

			while (uttIter.hasNext()) {
				final Utterance nextUtt = uttIter.next();
				final String nextSpeakerId = nextUtt.getSpeakerId();
				if (!Objects.equals(speakerId, nextSpeakerId)) {
					result.add(Pair.of(speakerId, diagTurnUttReprs.stream().collect(sentenceJoiner)));
					speakerId = nextSpeakerId;
					diagTurnUttReprs = new ArrayList<>();
				}
				diagTurnUttReprs.add(capitalizeFirstChar(nextUtt.getTokens().stream().collect(wordJoiner)) + '.');
			}

			if (!diagTurnUttReprs.isEmpty()) {
				result.add(Pair.of(speakerId, diagTurnUttReprs.stream().collect(sentenceJoiner)));
			}

		}
		return result;
	}

}
