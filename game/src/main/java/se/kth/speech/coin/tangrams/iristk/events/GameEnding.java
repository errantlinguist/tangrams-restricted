/*
 *  This file is part of tangrams.
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
package se.kth.speech.coin.tangrams.iristk.events;

import iristk.util.Record;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 14 Dec 2016
 *
 */
public final class GameEnding extends Record {

	public enum Outcome {
		ABORT, WIN;
	}

	private int moveCount;

	/**
	 * This is a {@link String} instead of an {@link Outcome} reference in order
	 * to support (un-)marshalling.
	 */
	@RecordField(name = "outcome")
	private String outcome;

	private String playerId;

	public GameEnding() {
		// Default constructor is required for JSON (un-)marshalling
	}

	public GameEnding(final String playerId, final int moveCount, final Outcome outcome) {
		setPlayerId(playerId);
		setMoveCount(moveCount);
		setOutcome(outcome);
	}

	/**
	 * @return the moveCount
	 */
	@RecordField(name = "moveCount")
	public int getMoveCount() {
		return moveCount;
	}

	/**
	 * @return the outcome
	 */
	public Outcome getOutcome() {
		return Outcome.valueOf(outcome);
	}

	/**
	 * @return the playerId
	 */
	@RecordField(name = "playerId")
	public String getPlayerId() {
		return playerId;
	}

	/**
	 * @param moveCount
	 *            the moveCount to set
	 */
	@RecordField(name = "moveCount")
	public void setMoveCount(final int moveCount) {
		this.moveCount = moveCount;
	}

	/**
	 * @param outcome
	 *            the outcome to set
	 */
	public void setOutcome(final Outcome outcome) {
		this.outcome = outcome.toString();
	}

	/**
	 * @param playerId
	 *            the playerId to set
	 */
	@RecordField(name = "playerId")
	public void setPlayerId(final String playerId) {
		this.playerId = playerId;
	}

}
