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

import java.math.BigDecimal;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Stream;

import it.unimi.dsi.fastutil.objects.Object2DoubleMap;
import it.unimi.dsi.fastutil.objects.Object2DoubleOpenHashMap;
import se.kth.speech.coin.tangrams.analysis.GameContext;
import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.coin.tangrams.analysis.dialogues.WeightedUtterance;
import se.kth.speech.coin.tangrams.analysis.features.ClassificationException;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.ReferentConfidenceData;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.ReferentConfidenceMapFactory;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.EventDialogueContextWordClassifierTrainer;
import weka.classifiers.Classifier;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 23 May 2017
 *
 */
public final class IsolatedUtteranceEventDialogueClassifier implements EventDialogueClassifier {

	private final Function<? super EventDialogue, Stream<WeightedUtterance>> diagUttWeighter;

	private final EventDialogueContextWordClassifierTrainer<?> diagWordClassifierFactory;

	private final ReferentConfidenceMapFactory referentConfidenceMapFactory;

	public IsolatedUtteranceEventDialogueClassifier(
			final EventDialogueContextWordClassifierTrainer<?> diagWordClassifierFactory,
			final ReferentConfidenceMapFactory referentConfidenceMapFactory, final BigDecimal instrUttObservationWeight,
			final BigDecimal otherUttObsevationWeight) {
		this.diagWordClassifierFactory = diagWordClassifierFactory;
		this.referentConfidenceMapFactory = referentConfidenceMapFactory;
		diagUttWeighter = new InstructorUtteranceWeighter(instrUttObservationWeight, otherUttObsevationWeight);
	}

	@Override
	public Optional<ReferentConfidenceData> apply(final EventDialogue diag, final GameContext ctx)
			throws ClassificationException {
		final Function<? super String, ? extends Classifier> wordClassifierGetter = diagWordClassifierFactory
				.apply(diag, ctx);
		final Stream<WeightedUtterance> weightedUtts = diagUttWeighter.apply(diag);
		final int estimatedTokenCount = diag.getUtterances().size() * 4;
		return createReferentConfidenceMap(weightedUtts, estimatedTokenCount, ctx, wordClassifierGetter);
	}

	/**
	 * Calculates the confidence of a given sequence of {@link Utterance
	 * utterances} referring to each referenceable entity given a particular
	 * {@link GameContext}.
	 *
	 * @param dialogueUtts
	 *            The sequence of utterances to classify.
	 * @param expectedTokenCount
	 *            The expected number of token observations for the given
	 *            {@link Stream} of dialogue {@link Utterance} instances.
	 * @param uttCtx
	 *            The {@link GameContext} instance representing the state of the
	 *            game at the time the given utterances were made.
	 * @param wordClassifierGetter
	 *            A {@link Function} returning the {@link Classifier} instance
	 *            to use for a particular token type encountered.
	 * @return A new {@link ReferentConfidenceData} mapping entity IDs to the
	 *         confidence measure of the entity with the given ID being referred
	 *         to by the given utterances.
	 * @throws ClassificationException
	 *             If an error occurs while
	 *             {@link ReferentConfidenceMapFactory#apply(Object2DoubleMap, GameContext, Function)
	 *             classifying} any individual entity.
	 */
	private Optional<ReferentConfidenceData> createReferentConfidenceMap(final Stream<WeightedUtterance> dialogueUtts,
			final int expectedTokenCount, final GameContext uttCtx,
			final Function<? super String, ? extends Classifier> wordClassifierGetter) throws ClassificationException {
		final Object2DoubleMap<String> diagTokens = new Object2DoubleOpenHashMap<>(expectedTokenCount, 1.0f);
		dialogueUtts.forEach(dialogueUtt -> {
			final double weight = dialogueUtt.getWeight();
			dialogueUtt.getUtterance().getTokens()
					.forEach(token -> diagTokens.put(token, diagTokens.getDouble(token) + weight));
		});
		return referentConfidenceMapFactory.apply(diagTokens, uttCtx, wordClassifierGetter);
	}

}
