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
 * @since 4 Jan 2017
 *
 */
public final class CoordinatePoint2D extends Record {

	private int x;

	private int y;

	public CoordinatePoint2D() {
		// Default constructor is required for JSON (un-)marshalling
	}

	public CoordinatePoint2D(final int x, final int y) {
		setX(x);
		setY(y);
	}

	public CoordinatePoint2D(final int[] coords) {
		this(coords[0], coords[1]);
	}

	public int[] getCoords() {
		return new int[] { getX(), getY() };
	}

	/**
	 * @return the x
	 */
	@RecordField(name = "x")
	public int getX() {
		return x;
	}

	/**
	 * @return the y
	 */
	@RecordField(name = "y")
	public int getY() {
		return y;
	}

	/**
	 * @param x
	 *            the x to set
	 */
	@RecordField(name = "x")
	public void setX(final int x) {
		this.x = x;
	}

	/**
	 * @param y
	 *            the y to set
	 */
	@RecordField(name = "y")
	public void setY(final int y) {
		this.y = y;
	}

}
