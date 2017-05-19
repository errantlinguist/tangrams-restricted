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

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import iristk.system.Event;
import it.unimi.dsi.fastutil.ints.Int2DoubleMap;
import se.kth.speech.coin.tangrams.analysis.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.GameContext;
import se.kth.speech.coin.tangrams.analysis.GameHistory;
import se.kth.speech.coin.tangrams.analysis.Utterance;
import se.kth.speech.coin.tangrams.analysis.features.ClassificationException;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 18 May 2017
 *
 */
public final class InstructorUtteranceReferentEventDialogueTester implements EventDialogueTester {

	private static final Logger LOGGER = LoggerFactory.getLogger(InstructorUtteranceReferentEventDialogueTester.class);

	private final UtteranceSequenceClassifier uttSeqClassifier;

	public InstructorUtteranceReferentEventDialogueTester(final UtteranceSequenceClassifier uttSeqClassifier) {
		this.uttSeqClassifier = uttSeqClassifier;
	}

	@Override
	public Optional<Result> apply(final EventDialogue uttDiag, final GameHistory history)
			throws ClassificationException {
		final Optional<Result> result;

		final Optional<Event> optLastEvent = uttDiag.getLastEvent();
		if (optLastEvent.isPresent()) {
			final Event event = optLastEvent.get();
			LOGGER.debug("Classifying referred entity for {}.", event);
			final String submittingPlayerId = event.getString(GameManagementEvent.Attribute.PLAYER_ID.toString());
			final List<Utterance> allUtts = uttDiag.getUtts();
			final List<Utterance> dialogueUttsFromInstructor = Arrays.asList(allUtts.stream().filter(dialogueUtt -> {
				final String uttPlayerId = dialogueUtt.getSpeakerId();
				return submittingPlayerId.equals(uttPlayerId);
			}).toArray(Utterance[]::new));

			if (dialogueUttsFromInstructor.isEmpty()) {
				result = Optional.empty();
			} else {
				// Just use the game context for the first utterance for all
				// utterances processed for the given dialogue
				final Utterance firstUtt = dialogueUttsFromInstructor.get(0);
				final GameContext uttCtx = UtteranceGameContexts.createGameContext(firstUtt, history,
						submittingPlayerId);
				final Int2DoubleMap referentConfidenceVals = uttSeqClassifier.apply(dialogueUttsFromInstructor, uttCtx);
				final int goldStandardEntityId = uttCtx.findLastSelectedEntityId().get();
				result = Optional.of(new Result(referentConfidenceVals, goldStandardEntityId,
						dialogueUttsFromInstructor, allUtts.size()));
			}
		} else {
			result = Optional.empty();
		}

		return result;
	}

}
