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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.utts;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.coin.tangrams.analysis.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.GameHistory;
import se.kth.speech.coin.tangrams.analysis.Utterance;
import se.kth.speech.coin.tangrams.game.PlayerRole;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;

/**
 * This {@link EventDialogueUtteranceSequenceExtractor} extracts all the
 * {@link Utterance utterances} made by the {@link PlayerRole#MOVE_SUBMISSION
 * instructor} of the {@link EventDialogue} used for classification.
 *
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 22 May 2017
 *
 */
public final class InstructorEventDialogueUtteranceExtractor implements EventDialogueUtteranceSequenceExtractor {

	private static final Logger LOGGER = LoggerFactory.getLogger(InstructorEventDialogueUtteranceExtractor.class);

	@Override
	public Optional<List<Utterance>> apply(final EventDialogue uttDiag, final GameHistory history) {
		return uttDiag.getLastEvent().map(event -> {
			LOGGER.debug("Classifying entity referred to by instructor references for {}.", event);
			final String submittingPlayerId = event.getString(GameManagementEvent.Attribute.PLAYER_ID.toString());
			final List<Utterance> allUtts = uttDiag.getUtts();
			return Arrays.asList(allUtts.stream().filter(dialogueUtt -> {
				final String uttPlayerId = dialogueUtt.getSpeakerId();
				return submittingPlayerId.equals(uttPlayerId);
			}).toArray(Utterance[]::new));
		});
	}

}
