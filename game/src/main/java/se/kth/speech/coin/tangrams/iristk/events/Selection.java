/*
 *  This file is part of Tangrams-restricted.
 *
 *  Tangrams-restricted is free software: you can redistribute it and/or modify
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
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since 12 Jan 2017
 *
 */
public final class Selection extends Record {

	private Area2D area;

	private Integer pieceId;

	public Selection() {
		// Default constructor is required for JSON (un-)marshalling
	}

	public Selection(final Integer pieceId, final Area2D area) {
		setPieceId(pieceId);
		setArea(area);
	}

	/**
	 * @return the area
	 */
	@RecordField(name = "area")
	public Area2D getArea() {
		return area;
	}

	/**
	 * @return the pieceId
	 */
	@RecordField(name = "pieceId")
	public Integer getPieceId() {
		return pieceId;
	}

	/**
	 * @param area
	 *            the area to set
	 */
	@RecordField(name = "area")
	public void setArea(final Area2D area) {
		this.area = area;
	}

	/**
	 * @param pieceId
	 *            the pieceId to set
	 */
	@RecordField(name = "pieceId")
	public void setPieceId(final Integer pieceId) {
		this.pieceId = pieceId;
	}

}
