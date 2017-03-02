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
public final class Move extends Record {

	private CoordinatePoint source;

	private CoordinatePoint target;

	public Move() {
		// Default constructor is required for JSON (un-)marshalling
	}

	public Move(final CoordinatePoint source, final CoordinatePoint target) {
		setSource(source);
		setTarget(target);
	}

	public Move(final Move copyee) {
		super(copyee);
	}

	/**
	 * @return the source
	 */
	@RecordField(name = "source")
	public CoordinatePoint getSource() {
		return source;
	}

	/**
	 * @return the target
	 */
	@RecordField(name = "target")
	public CoordinatePoint getTarget() {
		return target;
	}

	/**
	 * @param source
	 *            the source to set
	 */
	@RecordField(name = "source")
	public void setSource(final CoordinatePoint source) {
		this.source = source;
	}

	/**
	 * @param target
	 *            the target to set
	 */
	@RecordField(name = "target")
	public void setTarget(final CoordinatePoint target) {
		this.target = target;
	}

}
