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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues;

import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 14 Nov 2017
 *
 */
public final class MappingEventDialogueTransformer extends AbstractUtteranceTransformingEventDialogueTransformer {

	private final Map<? super List<String>, ? extends List<String>> tokenSeqTransformations;

	public MappingEventDialogueTransformer(
			final Map<? super List<String>, ? extends List<String>> tokenSeqTransformations) {
		this.tokenSeqTransformations = tokenSeqTransformations;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.
	 * dialogues.AbstractUtteranceTransformingEventDialogueTransformer#
	 * transformUtt(se.kth.speech.coin.tangrams.analysis.dialogues.Utterance)
	 */
	@Override
	protected Stream<Utterance> transformUtt(final Utterance utt) {
		final List<String> origTokenSeq = utt.getTokens();
		final List<String> transformedTokenSeq = tokenSeqTransformations.get(origTokenSeq);
		if (transformedTokenSeq == null) {
			throw new IllegalArgumentException("No mapping for token sequence: " + origTokenSeq);
		} else {
			return Stream.of(new Utterance(utt.getSegmentId(), utt.getSpeakerId(), transformedTokenSeq,
					utt.getStartTime(), utt.getEndTime()));
		}
	}

}