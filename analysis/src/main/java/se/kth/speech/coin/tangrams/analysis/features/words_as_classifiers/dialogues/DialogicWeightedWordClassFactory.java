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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues;

import java.util.Collection;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Stream;

import it.unimi.dsi.fastutil.objects.Object2DoubleMap;
import it.unimi.dsi.fastutil.objects.Object2DoubleOpenHashMap;
import se.kth.speech.coin.tangrams.analysis.Utterance;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Jul 15, 2017
 *
 */
public final class DialogicWeightedWordClassFactory
		implements Function<Collection<UtteranceRelation>, EntityReferringLanguageWordClasses> {

	private static Stream<String> getWordClasses(final List<Utterance> utts) {
		return utts.stream().flatMap(DialogicWeightedWordClassFactory::getWordClasses);
	}

	private static Stream<String> getWordClasses(final Utterance utt) {
		return utt.getTokens().stream();
	}

	private final double instrUttObservationWeight;

	private final double otherUttObsevationWeight;

	public DialogicWeightedWordClassFactory(final double instrUttObservationWeight,
			final double otherUttObsevationWeight) {
		this.instrUttObservationWeight = instrUttObservationWeight;
		this.otherUttObsevationWeight = otherUttObsevationWeight;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Function#apply(java.lang.Object)
	 */
	@Override
	public EntityReferringLanguageWordClasses apply(final Collection<UtteranceRelation> uttRels) {
		final int etimatedWordClassPerUttCount = 10;
		final int estimatedExampleSetCount = (uttRels.size() / 3 + 1) * etimatedWordClassPerUttCount;
		final Object2DoubleMap<String> refPosExamples = new Object2DoubleOpenHashMap<>(estimatedExampleSetCount);
		final Object2DoubleMap<String> refNegExamples = new Object2DoubleOpenHashMap<>(estimatedExampleSetCount);
		final Object2DoubleMap<String> otherEntityNegativeExamples = new Object2DoubleOpenHashMap<>(
				estimatedExampleSetCount);
		for (final UtteranceRelation uttRel : uttRels) {
			// Add all language from the instructor
			getWordClasses(uttRel.getAcceptanceUtt()).forEach(wordClass -> {
				// For each entity which is selected, process a
				// positive example for this observation: The
				// utterance being processed DOES correspond to
				// the selected entity
				refPosExamples.put(wordClass, refPosExamples.getDouble(wordClass) + instrUttObservationWeight);
				// For each entity which is NOT selected, process a
				// negative example for this observation: The
				// utterance being processed explicitly DOES NOT correspond to
				// the non-selected entity
				otherEntityNegativeExamples.put(wordClass,
						otherEntityNegativeExamples.getDouble(wordClass) + instrUttObservationWeight);
			});

			// Process non-instructor language
			final double acceptanceValue = uttRel.getAcceptanceValue();
			if (acceptanceValue < 0) {
				// Use the other player's utterances which came
				// before this instructor utterance as negative
				// examples for the referent but not anything for the
				// non-referent entities because it can't
				// be determined what the language actually refers to
				getWordClasses(uttRel.getPrevUtts()).forEach(wordClass -> refNegExamples.put(wordClass,
						refNegExamples.getDouble(wordClass) + otherUttObsevationWeight));
			} else if (acceptanceValue > 0) {
				// Use the other player's utterances which came
				// before this instructor utterance as positive
				// examples
				getWordClasses(uttRel.getPrevUtts()).forEach(wordClass -> {
					// For each entity which is selected, process a
					// positive example for this observation: The
					// utterance being processed DOES correspond to
					// the selected entity
					refPosExamples.put(wordClass, refPosExamples.getDouble(wordClass) + otherUttObsevationWeight);
					// For each entity which is NOT selected, process a
					// negative example for this observation: The
					// utterance being processed explicitly DOES NOT correspond
					// to
					// the non-selected entity
					otherEntityNegativeExamples.put(wordClass,
							otherEntityNegativeExamples.getDouble(wordClass) + otherUttObsevationWeight);
				});
			}
		}

		return new EntityReferringLanguageWordClasses(refPosExamples, refNegExamples, otherEntityNegativeExamples);
	}

}
