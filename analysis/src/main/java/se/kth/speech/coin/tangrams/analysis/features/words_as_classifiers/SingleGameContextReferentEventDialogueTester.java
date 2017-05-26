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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers;

import java.util.List;
import java.util.Optional;
import java.util.function.Function;

import it.unimi.dsi.fastutil.ints.Int2DoubleMap;
import se.kth.speech.coin.tangrams.analysis.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.GameContext;
import se.kth.speech.coin.tangrams.analysis.GameHistory;
import se.kth.speech.coin.tangrams.analysis.Utterance;
import se.kth.speech.coin.tangrams.analysis.features.ClassificationException;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.EventDialogueClassifier;

/**
 * This {@link EventDialogueTester} tests classification using a single
 * {@link GameContext} for all the individual {@link Utterance utterances} in
 * the given dialogue &mdash; namely, the context of the first utterance to be
 * used for classification is used for all subsequent utterances as well.
 *
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 18 May 2017
 *
 */
public final class SingleGameContextReferentEventDialogueTester implements EventDialogueTester {

	private final EventDialogueClassifier diagClassifier;

	private final Function<? super EventDialogue, EventDialogue> diagTransformer;

	public SingleGameContextReferentEventDialogueTester(final EventDialogueClassifier diagClassifier,
			final Function<? super EventDialogue, EventDialogue> diagTransformer) {
		this.diagClassifier = diagClassifier;
		this.diagTransformer = diagTransformer;
	}

	@Override
	public Optional<Result> apply(final EventDialogue uttDiag, final GameHistory history)
			throws ClassificationException {
		final Optional<Result> result;
		final EventDialogue transformedDiag = diagTransformer.apply(uttDiag);

		final List<Utterance> allUtts = transformedDiag.getUtts();
		if (allUtts.isEmpty()) {
			result = Optional.empty();
		} else {
			// Just use the game context for the first utterance for all
			// utterances processed for the given dialogue
			final Utterance firstUtt = allUtts.get(0);
			final GameContext uttCtx = UtteranceGameContexts.createSingleContext(firstUtt, history);
			final Optional<Int2DoubleMap> optReferentConfidenceVals = diagClassifier.apply(transformedDiag, uttCtx);
			if (optReferentConfidenceVals.isPresent()) {
				final Int2DoubleMap referentConfidenceVals = optReferentConfidenceVals.get();
				result = uttCtx.findLastSelectedEntityId().map(goldStandardEntityId -> {
					return new Result(referentConfidenceVals, goldStandardEntityId, transformedDiag,
							uttDiag.getUtts().size());
				});
			} else {
				result = Optional.empty();
			}
		}

		return result;
	}

}
