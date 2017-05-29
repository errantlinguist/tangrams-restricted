/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.analysis.
 *
 *  tangrams is free software: you can redistribute it and/or modify
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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags;

import java.util.List;
import java.util.function.Function;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.coin.tangrams.analysis.Utterance;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since May 27, 2017
 *
 */
public final class FallbackTokenizingEventDialogueTransformer
		extends AbstractUtteranceTransformingEventDialogueTransformer {

	private static final Logger LOGGER = LoggerFactory.getLogger(FallbackTokenizingEventDialogueTransformer.class);

	private final Function<? super String, ? extends List<String>> fallbackTokenizer;

	private final Function<? super String, ? extends List<String>> tokenizer;

	public FallbackTokenizingEventDialogueTransformer(final Function<? super String, ? extends List<String>> tokenizer,
			final Function<? super String, ? extends List<String>> fallbackTokenizer) {
		this.tokenizer = tokenizer;
		this.fallbackTokenizer = fallbackTokenizer;
	}

	@Override
	protected Stream<Utterance> transformUtt(final Utterance utt) {
		final String tokenStr = utt.getTokenStr();
		final List<String> resultTokens;
		{
			final List<String> newTokens = tokenizer.apply(tokenStr);
			if (newTokens.isEmpty() && !utt.getTokens().isEmpty()) {
				LOGGER.info("No tokens found for {}; Using fallback tokenizer.", utt);
				resultTokens = fallbackTokenizer.apply(tokenStr);
			} else {
				resultTokens = newTokens;
			}
		}
		return resultTokens.isEmpty() ? Stream.empty()
				: Stream.of(new Utterance(utt.getSegmentId(), utt.getSpeakerId(), resultTokens, utt.getStartTime(),
						utt.getEndTime()));
	}

}
