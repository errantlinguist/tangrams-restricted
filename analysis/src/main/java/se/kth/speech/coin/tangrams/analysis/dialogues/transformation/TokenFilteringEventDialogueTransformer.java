/*
 *  This file is part of Tangrams-restricted.
 *
 *  Tangrams-restricted is free software: you can redistribute it and/or modify
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
package se.kth.speech.coin.tangrams.analysis.dialogues.transformation;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Stream;

import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;

/**
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since May 27, 2017
 *
 */
public final class TokenFilteringEventDialogueTransformer
		extends AbstractUtteranceTransformingEventDialogueTransformer {

	private final Predicate<? super String> positiveTokenFilter;

	public TokenFilteringEventDialogueTransformer(final Collection<? super String> tokenBlacklist) {
		this(token -> !tokenBlacklist.contains(token));
	}

	public TokenFilteringEventDialogueTransformer(final Predicate<? super String> positiveTokenFilter) {
		this.positiveTokenFilter = positiveTokenFilter;
	}

	@Override
	protected Stream<Utterance> transformUtt(final Utterance utt) {
		final List<String> oldTokens = utt.getTokens();
		final Stream<String> newTokens = oldTokens.stream().filter(positiveTokenFilter);
		final String[] newTokenArr = newTokens.toArray(String[]::new);
		return newTokenArr.length < 1 ? Stream.empty()
				: Stream.of(new Utterance(utt.getSegmentId(), utt.getSpeakerId(), Arrays.asList(newTokenArr),
						utt.getStartTime(), utt.getEndTime()));
	}

}
