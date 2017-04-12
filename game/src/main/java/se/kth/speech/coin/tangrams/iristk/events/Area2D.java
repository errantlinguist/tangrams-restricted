/*
 *  This file is part of game.
 *
 *  game is free software: you can redistribute it and/or modify
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
 * @since 17 Mar 2017
 *
 */
public final class Area2D extends Record {

	private CoordinatePoint2D end;

	private CoordinatePoint2D start;

	public Area2D() {
		// Default constructor is required for JSON (un-)marshalling
	}

	public Area2D(final CoordinatePoint2D start, final CoordinatePoint2D end) {
		setStart(start);
		setEnd(end);
	}

	/**
	 * @return the end
	 */
	@RecordField(name = "end")
	public CoordinatePoint2D getEnd() {
		return end;
	}

	/**
	 * @return the start
	 */
	@RecordField(name = "start")
	public CoordinatePoint2D getStart() {
		return start;
	}

	/**
	 * @param end
	 *            the end to set
	 */
	@RecordField(name = "end")
	public void setEnd(final CoordinatePoint2D end) {
		this.end = end;
	}

	/**
	 * @param start
	 *            the start to set
	 */
	@RecordField(name = "start")
	public void setStart(final CoordinatePoint2D start) {
		this.start = start;
	}

}
