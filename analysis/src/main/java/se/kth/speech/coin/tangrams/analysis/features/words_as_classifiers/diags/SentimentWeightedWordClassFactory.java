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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Stream;

import se.kth.speech.coin.tangrams.analysis.Utterance;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.WeightedWordClass;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Jul 15, 2017
 *
 */
public final class SentimentWeightedWordClassFactory
		implements Function<Collection<UtteranceRelation>, EntityReferringLanguageWordClasses> {

	private static Stream<String> getWordClasses(final List<Utterance> utts) {
		return utts.stream().flatMap(SentimentWeightedWordClassFactory::getWordClasses);
	}

	private static Stream<String> getWordClasses(final Utterance utt) {
		return utt.getTokens().stream();
	}

	private final double instrUttObservationWeight;

	private final double otherUttObsevationWeight;

	public SentimentWeightedWordClassFactory(final double instrUttObservationWeight,
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
		final List<WeightedWordClass> refPosExamples = new ArrayList<>(estimatedExampleSetCount);
		final List<WeightedWordClass> refNegExamples = new ArrayList<>(estimatedExampleSetCount);
		final List<WeightedWordClass> otherEntityNegativeExamples = new ArrayList<>(estimatedExampleSetCount);
		final Consumer<WeightedWordClass> refPosExampleHandler = ex -> {
			// For each entity which is selected, process a
			// positive example for this observation: The
			// utterance being processed DOES correspond to
			// the selected entity
			refPosExamples.add(ex);
			// For each entity which is NOT selected, process a
			// negative example for this observation: The
			// utterance being processed explicitly DOES NOT correspond to
			// the non-selected entity
			otherEntityNegativeExamples.add(ex);
		};
		// For language which is explicitly not referring to the referent, add a
		// negative example but no positive example for others becuse it can't
		// be determined what the language actually refers to
		final Consumer<WeightedWordClass> refNegExampleHandler = refNegExamples::add;
		for (final UtteranceRelation uttRel : uttRels) {
			// Add all language from the instructor
			getWordClasses(uttRel.getSentimentUtt())
					.map(token -> new WeightedWordClass(token, instrUttObservationWeight))
					.forEach(refPosExampleHandler);

			// Process non-instructor language
			final double sentValue = uttRel.getSentimentValue();
			if (sentValue < 0) {
				// Use the other player's utterances which came
				// before this instructor utterance as negative
				// examples
				getWordClasses(uttRel.getPrevUtts())
						.map(token -> new WeightedWordClass(token, otherUttObsevationWeight))
						.forEach(refNegExampleHandler);
			} else if (sentValue > 0) {
				// Use the other player's utterances which came
				// before this instructor utterance as positive
				// examples
				getWordClasses(uttRel.getPrevUtts())
						.map(token -> new WeightedWordClass(token, otherUttObsevationWeight))
						.forEach(refPosExampleHandler);
			}
		}

		return new EntityReferringLanguageWordClasses(refPosExamples, refNegExamples, otherEntityNegativeExamples);
	}

}
