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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags;

import java.util.List;
import java.util.Optional;

import it.unimi.dsi.fastutil.ints.Int2DoubleMap;
import it.unimi.dsi.fastutil.objects.Object2DoubleMap;
import it.unimi.dsi.fastutil.objects.Object2DoubleOpenHashMap;
import se.kth.speech.coin.tangrams.analysis.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.GameContext;
import se.kth.speech.coin.tangrams.analysis.Utterance;
import se.kth.speech.coin.tangrams.analysis.features.ClassificationException;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.ReferentConfidenceMapFactory;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 23 May 2017
 *
 */
public final class IsolatedUtteranceEventDialogueClassifier implements EventDialogueClassifier {

	private final ReferentConfidenceMapFactory referentConfidenceMapFactory;

	public IsolatedUtteranceEventDialogueClassifier(final ReferentConfidenceMapFactory referentConfidenceMapFactory) {
		this.referentConfidenceMapFactory = referentConfidenceMapFactory;
	}

	@Override
	public Optional<Int2DoubleMap> apply(final EventDialogue diag, final GameContext ctx)
			throws ClassificationException {
		final Optional<Int2DoubleMap> result;
		final List<Utterance> uttsToClassify = diag.getUtts();
		if (uttsToClassify.isEmpty()) {
			result = Optional.empty();
		} else {
			final Int2DoubleMap referentConfidenceVals = createReferentConfidenceMap(uttsToClassify, ctx);
			result = Optional.of(referentConfidenceVals);
		}
		return result;
	}

	/**
	 * Calculates the confidence of a given sequence of {@link Utterance
	 * utterances} referring to each referenceable entity given a particular
	 * {@link GameContext}.
	 *
	 * @param dialogueUtts
	 *            The sequence of utterances to classify.
	 * @param uttCtx
	 *            The {@link GameContext} instance representing the state of the
	 *            game at the time the given utterances were made.
	 * @return A new {@link Int2DoubleMap} mapping entity IDs to the confidence
	 *         measure of the entity with the given ID being referred to by the
	 *         given utterances.
	 * @throws ClassificationException
	 *             If an error occurs while classifying any individual entity.
	 */
	private Int2DoubleMap createReferentConfidenceMap(final List<Utterance> dialogueUtts, final GameContext uttCtx)
			throws ClassificationException {
		final Object2DoubleMap<String> diagTokens = new Object2DoubleOpenHashMap<>(dialogueUtts.size() * 4);
		for (final Utterance dialogUtt : dialogueUtts) {
			for (final String token : dialogUtt.getTokens()) {
				diagTokens.put(token, diagTokens.getDouble(token) + 1.0);
			}
		}
		return referentConfidenceMapFactory.apply(diagTokens, uttCtx);
	}

}
