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
public final class Selection extends Record {

	private CoordinatePoint2D coords;

	private String playerId;

	public Selection() {
		// Default constructor is required for JSON (un-)marshalling
	}

	public Selection(final String playerId, final CoordinatePoint2D coords) {
		setPlayerId(playerId);
		setCoords(coords);
	}

	/**
	 * @return the coords
	 */
	@RecordField(name = "coords")
	public CoordinatePoint2D getCoords() {
		return coords;
	}

	/**
	 * @return the playerId
	 */
	@RecordField(name = "playerId")
	public String getPlayerId() {
		return playerId;
	}

	/**
	 * @param coords
	 *            the coords to set
	 */
	@RecordField(name = "coords")
	public void setCoords(final CoordinatePoint2D coords) {
		this.coords = coords;
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
