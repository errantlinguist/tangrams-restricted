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
 * @since 12 Jan 2017
 *
 */
public final class Turn extends Record {

	private Move move;

	private String playerId;

	private int sequenceNumber;

	public Turn() {
		// Default constructor is required for JSON (un-)marshalling
	}

	public Turn(final String playerId, final Move move, final int sequenceNumber) {
		setPlayerId(playerId);
		setMove(move);
		setSequenceNumber(sequenceNumber);
	}

	public Turn(final Turn copyee) {
		super(copyee);
	}

	/**
	 * @return the move
	 */
	@RecordField(name = "move")
	public Move getMove() {
		return move;
	}

	/**
	 * @return the playerId
	 */
	@RecordField(name = "playerId")
	public String getPlayerId() {
		return playerId;
	}

	/**
	 * @return the sequenceNumber
	 */
	@RecordField(name = "sequenceNumber")
	public int getSequenceNumber() {
		return sequenceNumber;
	}

	/**
	 * @param move
	 *            the move to set
	 */
	@RecordField(name = "move")
	public void setMove(final Move move) {
		this.move = move;
	}

	/**
	 * @param playerId
	 *            the playerId to set
	 */
	@RecordField(name = "playerId")
	public void setPlayerId(final String playerId) {
		this.playerId = playerId;
	}

	/**
	 * @param sequenceNumber
	 *            the sequenceNumber to set
	 */
	@RecordField(name = "sequenceNumber")
	public void setSequenceNumber(final int sequenceNumber) {
		this.sequenceNumber = sequenceNumber;
	}

}
