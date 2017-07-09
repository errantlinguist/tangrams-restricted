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
import java.util.ListIterator;
import java.util.function.Predicate;
import java.util.function.ToDoubleFunction;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.Iterators;
import se.kth.speech.coin.tangrams.analysis.Utterance;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Jul 9, 2017
 *
 */
public final class EventDialogueUtteranceSentimentSorter {

	public static interface ExampleHandler {

		public void accept(String wordClass, double weight);
	}

	private static final Logger LOGGER = LoggerFactory.getLogger(EventDialogueUtteranceSentimentSorter.class);

	private final ExampleHandler referentNegativeExampleHandler;

	private final ExampleHandler referentPositiveExampleHandler;

	private final ExampleHandler otherEntityNegativeExampleHandler;

	private final ToDoubleFunction<? super Utterance> uttSentimentRanker;

	public EventDialogueUtteranceSentimentSorter(final ToDoubleFunction<? super Utterance> uttSentimentRanker,
			final ExampleHandler referentPositiveExampleHandler, final ExampleHandler referentNegativeExampleHandler,
			final ExampleHandler otherEntityNegativeExampleHandler) {
		this.uttSentimentRanker = uttSentimentRanker;
		this.referentPositiveExampleHandler = referentPositiveExampleHandler;
		this.referentNegativeExampleHandler = referentNegativeExampleHandler;
		this.otherEntityNegativeExampleHandler = otherEntityNegativeExampleHandler;
	}

	public void accept(final ListIterator<Utterance> uttIter, final Predicate<Utterance> instructorUttMatcher) {
		while (uttIter.hasNext()) {
			final java.util.Map.Entry<Stream<Utterance>, Utterance> preInstructorUtts = Iterators
					.findElementsBeforeDelimiter(uttIter, instructorUttMatcher);
			final Utterance firstInstructorUtt = preInstructorUtts.getValue();
			{
				// Handle next instructor utterance
				// final double instructorObservationWeight =
				// firstInstructorUttSentimentRank <= 0.0 ? 1.0
				// : firstInstructorUttSentimentRank;
				final double instructorObservationWeight = 1;
				firstInstructorUtt.getTokens().stream().forEach(wordClass -> {
					LOGGER.debug(
							"Processing positive observation of word class \"{}\" from instructor utterance with weight {}.",
							wordClass, instructorObservationWeight);
					referentPositiveExampleHandler.accept(wordClass, instructorObservationWeight);
					otherEntityNegativeExampleHandler.accept(wordClass, instructorObservationWeight);
				});
			}

			{
				// Handle preceding non-instructor utterances
				final double firstInstructorUttSentimentRank = uttSentimentRanker.applyAsDouble(firstInstructorUtt);
				if (firstInstructorUttSentimentRank < 0) {
					// Use the other player's utterances which came
					// before this instructor utterance as negative
					// examples
					// final double weight =
					// Math.abs(firstInstructorUttSentimentRank);
					final double weight = 1;
					final Stream<String> preInstructorWordClasses = preInstructorUtts.getKey().map(Utterance::getTokens)
							.flatMap(List::stream);
					preInstructorWordClasses.forEach(wordClass -> {
						LOGGER.debug(
								"Processing negative observation of word class \"{}\" from non-instructor utterance with weight {}.",
								wordClass, weight);
						// For each entity which is selected, process a
						// negative example for this observation: The
						// utterance being processed does NOT correspond
						// to the selected entity
						referentNegativeExampleHandler.accept(wordClass, weight);
					});
				} else if (firstInstructorUttSentimentRank > 0) {
					// Use the other player's utterances which came
					// before this instructor utterance as positive
					// examples
					final Stream<String> preInstructorWordClasses = preInstructorUtts.getKey().map(Utterance::getTokens)
							.flatMap(List::stream);
					// final double weight =
					// firstInstructorUttSentimentRank;
					final double weight = 1;
					preInstructorWordClasses.forEach(wordClass -> {
						LOGGER.debug(
								"Processing positive observation of word class \"{}\" from non-instructor utterance with weight {}.",
								wordClass, firstInstructorUttSentimentRank);
						// For each entity which is selected, process a
						// positive example for this observation: The
						// utterance being processed DOES correspond to
						// the selected entity
						referentPositiveExampleHandler.accept(wordClass, weight);
						otherEntityNegativeExampleHandler.accept(wordClass, weight);
					});
				}
			}
		}
	}

}
