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
package se.kth.speech.coin.tangrams.iristk;

import java.util.Iterator;

import se.kth.speech.coin.tangrams.iristk.events.HashableGameStateDescription;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 5 May 2017
 *
 */
public final class GameStateDescriptions {

	public static HashableGameStateDescription findAnyEquivalentGameState(
			final Iterator<HashableGameStateDescription> gameDescs) {
		final HashableGameStateDescription result = gameDescs.next();
		while (gameDescs.hasNext()) {
			// Sanity check to make sure that all players have
			// started with the same game setup
			final HashableGameStateDescription next = gameDescs.next();
			if (!result.equals(next)) {
				throw new IllegalArgumentException("Found non-equivalent initial states between players.");
			}
		}
		return result;
	}

	private GameStateDescriptions() {
	}

}
