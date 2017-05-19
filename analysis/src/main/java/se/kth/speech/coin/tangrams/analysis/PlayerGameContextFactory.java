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

import java.util.function.Function;
import java.util.stream.Stream;

final class PlayerGameContextFactory {

	private final Function<? super String, GameHistory> playerGameHistoryGetter;

	public PlayerGameContextFactory(final Function<? super String, GameHistory> playerGameHistoryGetter) {
		this.playerGameHistoryGetter = playerGameHistoryGetter;
	}

	public Stream<GameContext> create(final double startTime, final double endTime,
			final String perspectivePlayerId) {
		UtteranceSelectedEntityDescriptionWriter.LOGGER.debug("Creating a context based on the logged game history from the perspective of player \"{}\".",
				perspectivePlayerId);
		final GameHistory history = playerGameHistoryGetter.apply(perspectivePlayerId);
		return TemporalGameContexts.create(history, startTime, endTime, perspectivePlayerId);
	}

}