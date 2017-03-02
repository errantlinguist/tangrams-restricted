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

	private String newActivePlayerId;

	private String oldActivePlayerId;

	public ActivePlayerChange() {
		// Default constructor is required for JSON (un-)marshalling
	}

	/**
	 * @param playerIdsToUndo
	 */
	public ActivePlayerChange(final ActivePlayerChange copyee) {
		super(copyee);
	}

	public ActivePlayerChange(final String oldActivePlayerId, final String newActivePlayerId) {
		setOldActivePlayerId(oldActivePlayerId);
		setNewActivePlayerId(newActivePlayerId);
	}

	/**
	 * @return the newActivePlayerId
	 */
	@RecordField(name = "newActivePlayerId")
	public String getNewActivePlayerId() {
		return newActivePlayerId;
	}

	/**
	 * @return the oldActivePlayerId
	 */
	@RecordField(name = "oldActivePlayerId")
	public String getOldActivePlayerId() {
		return oldActivePlayerId;
	}

	/**
	 * @param newActivePlayerId
	 *            the newActivePlayerId to set
	 */
	@RecordField(name = "newActivePlayerId")
	public void setNewActivePlayerId(final String newActivePlayerId) {
		this.newActivePlayerId = newActivePlayerId;
	}

	/**
	 * @param oldActivePlayerId
	 *            the oldActivePlayerId to set
	 */
	@RecordField(name = "oldActivePlayerId")
	public void setOldActivePlayerId(final String oldActivePlayerId) {
		this.oldActivePlayerId = oldActivePlayerId;
	}

}
