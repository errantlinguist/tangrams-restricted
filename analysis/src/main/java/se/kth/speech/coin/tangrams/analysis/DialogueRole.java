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
package se.kth.speech.coin.tangrams.analysis;

import java.util.Objects;

import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.coin.tangrams.game.PlayerRole;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;

/**
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since 24 Nov 2017
 *
 */
public enum DialogueRole {
	INSTRUCTOR, MANIPULATOR;

	public static DialogueRole get(final PlayerRole role) {
		final DialogueRole result;
		switch (role) {
		case MOVE_SUBMISSION:
			result = INSTRUCTOR;
			break;
		case WAITING_FOR_NEXT_MOVE:
			result = MANIPULATOR;
			break;
		default:
			throw new IllegalArgumentException(String.format("No description for player role %s.", role));
		}
		return result;
	}

	public static DialogueRole get(final Utterance utt, final EventDialogue evtDiag) {
		final String speakerId = utt.getSpeakerId();
		final String submitterId = (String) evtDiag.getFirstEvent().get().getGameAttrs()
				.get(GameManagementEvent.Attribute.PLAYER_ID);
		return Objects.equals(speakerId, submitterId) ? INSTRUCTOR : MANIPULATOR;
	}
}