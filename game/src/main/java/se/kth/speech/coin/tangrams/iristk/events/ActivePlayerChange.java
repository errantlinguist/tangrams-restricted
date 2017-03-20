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
public final class ActivePlayerChange extends Record {

	private String newInstructingPlayerId;

	private String oldInstructingPlayerId;

	public ActivePlayerChange() {
		// Default constructor is required for JSON (un-)marshalling
	}

	/**
	 * @param playerIdsToUndo
	 */
	public ActivePlayerChange(final ActivePlayerChange copyee) {
		super(copyee);
	}

	public ActivePlayerChange(final String oldInstructingPlayerId, final String newInstructingPlayerId) {
		setOldInstructingPlayerId(oldInstructingPlayerId);
		setNewInstructingPlayerId(newInstructingPlayerId);
	}

	/**
	 * @return the newInstructingPlayerId
	 */
	@RecordField(name = "newInstructingPlayerId")
	public String getNewInstructingPlayerId() {
		return newInstructingPlayerId;
	}

	/**
	 * @return the oldInstructingPlayerId
	 */
	@RecordField(name = "oldInstructingPlayerId")
	public String getOldInstructingPlayerId() {
		return oldInstructingPlayerId;
	}

	/**
	 * @param newInstructingPlayerId
	 *            the newInstructingPlayerId to set
	 */
	@RecordField(name = "newInstructingPlayerId")
	public void setNewInstructingPlayerId(final String newInstructingPlayerId) {
		this.newInstructingPlayerId = newInstructingPlayerId;
	}

	/**
	 * @param oldInstructingPlayerId
	 *            the oldInstructingPlayerId to set
	 */
	@RecordField(name = "oldInstructingPlayerId")
	public void setOldInstructingPlayerId(final String oldInstructingPlayerId) {
		this.oldInstructingPlayerId = oldInstructingPlayerId;
	}

}
