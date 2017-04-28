/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.client.
 *
 *  se.kth.speech.coin.tangrams-restricted.client is free software: you can redistribute it and/or modify
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
package se.kth.speech.coin.tangrams.game;

import java.util.Map.Entry;
import java.util.Set;

import se.kth.speech.SpatialMatrix;
import se.kth.speech.SpatialRegion;
import se.kth.speech.coin.tangrams.iristk.events.Move;
import se.kth.speech.coin.tangrams.iristk.events.Selection;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 28 Apr 2017
 *
 */
public interface Controller {

	public interface Listener {

		void updateNextMove(Move move);

		void updatePlayerJoined(String joinedPlayerId, long time);

		void updatePlayerRole(PlayerRole newRole);

		void updatePlayerSelection(Integer pieceId, SpatialRegion region);

		void updateScore(int score);

		void updateSelectionRejected(Integer pieceId, SpatialRegion region);

		void updateTurnCompleted(Turn turn);

		/**
		 * A hook for updating listeners for a new turn count, i.e. the sequence
		 * number of the next turn to be completed.
		 *
		 * @param newCount
		 *            The new turn count.
		 */
		void updateTurnCount(int newCount);

	}

	Set<Listener> getListeners();

	SpatialMatrix<Integer> getModel();

	PlayerRole getRole();

	int getScore();

	int getTurnCount();

	void notifyNextMove(String submittingPlayerId, Move move);

	void notifyPlayerJoined(String joinedPlayerId, long time);

	void notifyPlayerSelection(String selectingPlayerId, Selection selection);

	void notifySelectionRejected(String rejectingPlayerId, Selection selection);

	void notifyTurnComplete(String submittingPlayerId, Move move);

	void submitNextMove(SpatialRegion sourceRegion, SpatialRegion targetRegion, Integer pieceId);

	/**
	 * @param key
	 */
	void submitSelection(Entry<Integer, SpatialRegion> pieceRegion);

	void submitSelectionRejection();

	void submitTurnComplete();

	boolean isSelectionCorrect();

	String getPlayerId();

}