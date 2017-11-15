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

import java.math.BigDecimal;
import java.util.Collection;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Stream;

import it.unimi.dsi.fastutil.objects.Object2DoubleMap;
import it.unimi.dsi.fastutil.objects.Object2DoubleOpenHashMap;
import it.unimi.dsi.fastutil.objects.Object2IntMap;
import it.unimi.dsi.fastutil.objects.Object2IntOpenHashMap;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.math.NumberTypeConversions;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Jul 15, 2017
 *
 */
public final class DialogicWeightedWordClassFactory
		implements Function<Collection<UtteranceRelation>, EntityReferringLanguageWordClasses> {

	private interface InstructorUtteranceObservationPutter {
		void accept(final Utterance acceptanceUtt, final Object2DoubleMap<String> refPosExamples,
				final Object2DoubleMap<String> otherEntityNegativeExamples,
				final Object2IntMap<String> wordClassObsevationCounts);
	}

	private static class LinearWeightCombiningInstructorUtteranceObservationPutter
			implements InstructorUtteranceObservationPutter {

		private final double weight;

		private LinearWeightCombiningInstructorUtteranceObservationPutter(final double weight) {
			this.weight = weight;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see
		 * se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.
		 * dialogues.DialogicWeightedWordClassFactory.
		 * InstructorObservationPutter#accept(se.kth.speech.coin
		 * .tangrams.analysis.dialogues.Utterance,
		 * it.unimi.dsi.fastutil.objects.Object2DoubleMap,
		 * it.unimi.dsi.fastutil.objects.Object2DoubleMap,
		 * it.unimi.dsi.fastutil.objects.Object2IntMap)
		 */
		@Override
		public void accept(final Utterance acceptanceUtt, final Object2DoubleMap<String> refPosExamples,
				final Object2DoubleMap<String> otherEntityNegativeExamples,
				final Object2IntMap<String> wordClassObsevationCounts) {
			getWordClasses(acceptanceUtt).forEach(wordClass -> {
				// For each entity which is selected, process a
				// positive example for this observation: The
				// utterance being processed DOES correspond to
				// the selected entity
				refPosExamples.put(wordClass, refPosExamples.getDouble(wordClass) + weight);
				// For each entity which is NOT selected, process a
				// negative example for this observation: The
				// utterance being processed explicitly DOES NOT correspond
				// to the non-selected entity
				otherEntityNegativeExamples.put(wordClass, otherEntityNegativeExamples.getDouble(wordClass) + weight);
				wordClassObsevationCounts.put(wordClass, wordClassObsevationCounts.getInt(wordClass) + 1);
			});
		}

	}

	private static class LinearWeightCombiningOtherUtteranceObservationPutter
			implements OtherUtteranceObservationPutter {

		private final double weight;

		private LinearWeightCombiningOtherUtteranceObservationPutter(final double weight) {
			this.weight = weight;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see
		 * se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.
		 * dialogues.DialogicWeightedWordClassFactory.
		 * NonInstructorObservationPutter#accept(java.util. List, double,
		 * it.unimi.dsi.fastutil.objects.Object2DoubleMap,
		 * it.unimi.dsi.fastutil.objects.Object2DoubleMap,
		 * it.unimi.dsi.fastutil.objects.Object2DoubleMap,
		 * it.unimi.dsi.fastutil.objects.Object2IntMap)
		 */
		@Override
		public void accept(final List<Utterance> prevUtts, final double acceptanceValue,
				final Object2DoubleMap<String> refPosExamples, final Object2DoubleMap<String> refNegExamples,
				final Object2DoubleMap<String> otherEntityNegativeExamples,
				final Object2IntMap<String> wordClassObsevationCounts) {
			if (acceptanceValue < 0) {
				// Use the other player's utterances which came
				// before this instructor utterance as negative
				// examples for the referent but not anything for the
				// non-referent entities because it can't
				// be determined what the language actually refers to
				getWordClasses(prevUtts).forEach(wordClass -> {
					refNegExamples.put(wordClass, refNegExamples.getDouble(wordClass) + weight);
					wordClassObsevationCounts.put(wordClass, wordClassObsevationCounts.getInt(wordClass) + 1);
				});
			} else if (acceptanceValue > 0) {
				// Use the other player's utterances which came
				// before this instructor utterance as positive
				// examples
				getWordClasses(prevUtts).forEach(wordClass -> {
					// For each entity which is selected, process a
					// positive example for this observation: The
					// utterance being processed DOES correspond to
					// the selected entity
					refPosExamples.put(wordClass, refPosExamples.getDouble(wordClass) + weight);
					// For each entity which is NOT selected, process a
					// negative example for this observation: The
					// utterance being processed explicitly DOES NOT correspond
					// to
					// the non-selected entity
					otherEntityNegativeExamples.put(wordClass,
							otherEntityNegativeExamples.getDouble(wordClass) + weight);
					wordClassObsevationCounts.put(wordClass, wordClassObsevationCounts.getInt(wordClass) + 1);
				});
			}
			// If the acceptance value is 0, do nothing because the
			// non-instructor
			// language cannot be reasoned about
		}

	}

	private interface OtherUtteranceObservationPutter {
		void accept(final List<Utterance> prevUtts, final double acceptanceValue,
				final Object2DoubleMap<String> refPosExamples, final Object2DoubleMap<String> refNegExamples,
				final Object2DoubleMap<String> otherEntityNegativeExamples,
				final Object2IntMap<String> wordClassObsevationCounts);
	}

	private static InstructorUtteranceObservationPutter createInstructorObservationPutter(final BigDecimal weight) {
		final InstructorUtteranceObservationPutter result;
		final double doubleWeight = NumberTypeConversions.nonInfiniteDoubleValueExact(weight);
		final int cmp = weight.compareTo(BigDecimal.ZERO);
		if (cmp > 0) {
			result = new LinearWeightCombiningInstructorUtteranceObservationPutter(doubleWeight);
		} else if (cmp == 0) {
			result = (acceptanceUtt, refPosExamples, otherEntityNegativeExamples, wordClassObsevationCounts) -> {
				// Do nothing because Weka cannot handle weights of "0"
			};
		} else {
			throw new IllegalArgumentException(String.format("Weight was %s but must be non-negative.", weight));
		}
		return result;
	}

	private static OtherUtteranceObservationPutter createNonInstructorObservationPutter(final BigDecimal weight) {
		final OtherUtteranceObservationPutter result;
		final double doubleWeight = NumberTypeConversions.nonInfiniteDoubleValueExact(weight);
		final int cmp = weight.compareTo(BigDecimal.ZERO);
		if (cmp > 0) {
			result = new LinearWeightCombiningOtherUtteranceObservationPutter(doubleWeight);
		} else if (cmp == 0) {
			result = (prevUtts, acceptanceValue, refPosExamples, refNegExamples, otherEntityNegativeExamples,
					wordClassObsevationCounts) -> {
				// Do nothing because Weka cannot handle weights of "0"
			};
		} else {
			throw new IllegalArgumentException(String.format("Weight was %s but must be non-negative.", weight));
		}
		return result;
	}

	private static Stream<String> getWordClasses(final List<Utterance> utts) {
		return utts.stream().flatMap(DialogicWeightedWordClassFactory::getWordClasses);
	}

	private static Stream<String> getWordClasses(final Utterance utt) {
		return utt.getTokens().stream();
	}

	private final InstructorUtteranceObservationPutter instrUttObsPutter;

	private final OtherUtteranceObservationPutter otherUttObsPutter;

	public DialogicWeightedWordClassFactory(final BigDecimal instrUttObservationWeight,
			final BigDecimal otherUttObsevationWeight) {
		instrUttObsPutter = createInstructorObservationPutter(instrUttObservationWeight);
		otherUttObsPutter = createNonInstructorObservationPutter(otherUttObsevationWeight);
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
		final Object2IntMap<String> wordClassObsevationCounts = new Object2IntOpenHashMap<>(estimatedExampleSetCount);
		for (final UtteranceRelation uttRel : uttRels) {
			uttRel.getAcceptanceUtt().ifPresent(acceptanceUtt -> {
				// Add all language from the instructor
				instrUttObsPutter.accept(acceptanceUtt.getUtterance(), refPosExamples, otherEntityNegativeExamples,
						wordClassObsevationCounts);
				// Process non-instructor language
				final double acceptanceValue = acceptanceUtt.getWeight();
				otherUttObsPutter.accept(uttRel.getPrevUtts(), acceptanceValue, refPosExamples, refNegExamples,
						otherEntityNegativeExamples, wordClassObsevationCounts);
			});
			// If there was no instructor utterance found which could be used to
			// calculate
			// an acceptance score, do nothing because the non-instructor
			// language cannot be reasoned about and there is no instructor
			// utterance to use
		}

		return new EntityReferringLanguageWordClasses(refPosExamples, refNegExamples, otherEntityNegativeExamples,
				wordClassObsevationCounts);
	}

}
