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

import javax.inject.Inject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import it.unimi.dsi.fastutil.ints.Int2DoubleMap;
import se.kth.speech.coin.tangrams.analysis.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.GameContext;
import se.kth.speech.coin.tangrams.analysis.GameHistory;
import se.kth.speech.coin.tangrams.analysis.TemporalGameContexts;
import se.kth.speech.coin.tangrams.analysis.Utterance;
import se.kth.speech.coin.tangrams.analysis.features.ClassificationException;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.utts.EventDialogueUtteranceSequenceExtractor;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.utts.UtteranceSequenceClassifier;

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

	private static final Logger LOGGER = LoggerFactory.getLogger(SingleGameContextReferentEventDialogueTester.class);

	private static GameContext createGameContext(final Utterance dialogueUtt, final GameHistory history,
			final String perspectivePlayerId) {
		LOGGER.debug(
				"Creating a context based on the logged game history, which is then seen from the perspective of player \"{}\".",
				perspectivePlayerId);
		final GameContext[] ctxs = TemporalGameContexts
				.create(history, dialogueUtt.getStartTime(), dialogueUtt.getEndTime(), perspectivePlayerId)
				.toArray(GameContext[]::new);
		if (ctxs.length > 1) {
			LOGGER.warn("More than one game context found for {}; Only using the first one.", dialogueUtt);
		}
		return ctxs[0];
	}

	@Inject
	private EventDialogueUtteranceSequenceExtractor diagUttExtractor;

	private final UtteranceSequenceClassifier uttSeqClassifier;

	public SingleGameContextReferentEventDialogueTester(final UtteranceSequenceClassifier uttSeqClassifier) {
		this.uttSeqClassifier = uttSeqClassifier;
	}

	@Override
	public Optional<Result> apply(final EventDialogue uttDiag, final GameHistory history)
			throws ClassificationException {
		final Optional<Result> result;

		final Optional<List<Utterance>> optUttsToClassify = diagUttExtractor.apply(uttDiag, history);
		if (optUttsToClassify.isPresent()) {
			final List<Utterance> uttsToClassify = optUttsToClassify.get();
			if (uttsToClassify.isEmpty()) {
				result = Optional.empty();
			} else {
				// Just use the game context for the first utterance for all
				// utterances processed for the given dialogue
				final Utterance firstUtt = uttsToClassify.get(0);
				final String firstSpeakerId = firstUtt.getSpeakerId();
				LOGGER.debug("Creating game context from perspective of player \"{}\".", firstSpeakerId);
				final GameContext uttCtx = createGameContext(firstUtt, history, firstSpeakerId);
				final Int2DoubleMap referentConfidenceVals = uttSeqClassifier.apply(uttsToClassify, uttCtx);
				final int goldStandardEntityId = uttCtx.findLastSelectedEntityId().get();
				result = Optional.of(new Result(referentConfidenceVals, goldStandardEntityId, uttsToClassify,
						uttDiag.getUtts().size()));

			}
		} else {
			result = Optional.empty();
		}

		return result;
	}

}
