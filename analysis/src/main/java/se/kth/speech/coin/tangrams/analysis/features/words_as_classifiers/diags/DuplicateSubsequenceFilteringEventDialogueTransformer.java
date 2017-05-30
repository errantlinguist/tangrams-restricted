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
import java.util.stream.Stream;

import se.kth.speech.ListSubsequences;
import se.kth.speech.coin.tangrams.analysis.Utterance;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since May 27, 2017
 *
 */
public final class DuplicateSubsequenceFilteringEventDialogueTransformer
		extends AbstractUtteranceTransformingEventDialogueTransformer {

	@Override
	protected Stream<Utterance> transformUtt(final Utterance utt) {
		final List<String> oldTokens = utt.getTokens();
		final List<String> newTokens = ListSubsequences.createDeduplicatedAdjacentSubsequenceList(oldTokens);
		assert oldTokens.isEmpty() == newTokens.isEmpty();
		return Stream.of(
				new Utterance(utt.getSegmentId(), utt.getSpeakerId(), newTokens, utt.getStartTime(), utt.getEndTime()));
	}

}
