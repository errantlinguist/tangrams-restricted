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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.coin.tangrams.analysis.GameContext;
import se.kth.speech.coin.tangrams.analysis.GameHistory;
import se.kth.speech.coin.tangrams.analysis.TemporalGameContexts;
import se.kth.speech.coin.tangrams.analysis.Utterance;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since May 26, 2017
 *
 */
public final class UtteranceGameContexts {

	private static final Logger LOGGER = LoggerFactory.getLogger(UtteranceGameContexts.class);

	public static GameContext createSingleContext(final Utterance dialogueUtt, final GameHistory history,
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

	private UtteranceGameContexts() {

	}

}
