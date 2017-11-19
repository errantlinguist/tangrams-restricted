/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.client.
 *
 *  client is free software: you can redistribute it and/or modify
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

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 21 Mar 2017
 *
 */
public enum PlayerRole {
	/**
	 * In this role, the player is to submit the next move to be played.
	 */
	MOVE_SUBMISSION,
	/**
	 * In this role, the player is to select a candidate piece for moving.
	 */
	SELECTING,
	/**
	 * In this role, the player is to confirm a previously-selected piece for
	 * moving in the turn.
	 */
	SELECTION_CONFIRMATION,
	/**
	 * In this role, the player is waiting for the other player to
	 * {@link #MOVE_SUBMISSION submit the next move to be played}.
	 */
	WAITING_FOR_NEXT_MOVE,
	/**
	 * In this role, the player is waiting for the other player to
	 * {@link #SELECTING select a candidate piece for moving}.
	 */
	WAITING_FOR_SELECTION,
	/**
	 * In this role, the player is waiting for the other player to
	 * {@link #SELECTION_CONFIRMATION confirm a previously-selected candidate
	 * piece for moving}.
	 */
	WAITING_FOR_SELECTION_CONFIRMATION;
}
