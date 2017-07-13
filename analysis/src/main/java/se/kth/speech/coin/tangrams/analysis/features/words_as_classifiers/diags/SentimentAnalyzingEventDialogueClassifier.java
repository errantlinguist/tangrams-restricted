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
import java.util.List;
import java.util.Optional;
import java.util.function.ToDoubleFunction;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import iristk.system.Event;
import it.unimi.dsi.fastutil.ints.Int2DoubleMap;
import se.kth.speech.coin.tangrams.analysis.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.GameContext;
import se.kth.speech.coin.tangrams.analysis.Utterance;
import se.kth.speech.coin.tangrams.analysis.features.ClassificationException;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.ReferentConfidenceMapFactory;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.WeightedWordClass;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Jul 9, 2017
 *
 */
public final class SentimentAnalyzingEventDialogueClassifier implements EventDialogueClassifier {

	private static final Logger LOGGER = LoggerFactory.getLogger(SentimentAnalyzingEventDialogueClassifier.class);

	private final ReferentConfidenceMapFactory referentConfidenceMapFactory;

	private final ToDoubleFunction<? super Utterance> uttSentimentRanker;

	public SentimentAnalyzingEventDialogueClassifier(final ToDoubleFunction<? super Utterance> uttSentimentRanker,
			final ReferentConfidenceMapFactory referentConfidenceMapFactory) {
		this.uttSentimentRanker = uttSentimentRanker;
		this.referentConfidenceMapFactory = referentConfidenceMapFactory;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.
	 * EventDialogueClassifier#apply(se.kth.speech.coin.tangrams.analysis.
	 * EventDialogue, se.kth.speech.coin.tangrams.analysis.GameContext)
	 */
	@Override
	public Optional<Int2DoubleMap> apply(final EventDialogue transformedDiag, final GameContext ctx)
			throws ClassificationException {
		final Optional<Int2DoubleMap> result;
		final Optional<Event> optFirstEvent = transformedDiag.getFirstEvent();
		if (optFirstEvent.isPresent()) {
			final Event event = optFirstEvent.get();
			LOGGER.debug("Classifying utterances for event: {}", event);
			final List<Utterance> allUtts = transformedDiag.getUtts();
			if (allUtts.isEmpty()) {
				LOGGER.debug("No utterances to classify for {}.", transformedDiag);
				result = Optional.empty();
			} else {
				final List<WeightedWordClass> weightedWordClasses = new ArrayList<>(allUtts.size() * 16);
				final SentimentAnalyzingEventDialogueUtteranceSorter uttSorter = new SentimentAnalyzingEventDialogueUtteranceSorter(
						uttSentimentRanker);
				final SentimentAnalyzingEventDialogueUtteranceSorter.Result sortedUtts = uttSorter.apply(allUtts,
						event);
				sortedUtts.getRefPosExamples().forEach(utt -> {
					LOGGER.debug("Processing referent positive example: {}", utt);
					utt.getTokens().stream().map(token -> new WeightedWordClass(token, 1.0))
							.forEach(weightedWordClasses::add);
				});

				result = Optional.of(referentConfidenceMapFactory.apply(weightedWordClasses, ctx));
			}
		} else

		{
			result = Optional.empty();
		}
		return result;
	}

}
