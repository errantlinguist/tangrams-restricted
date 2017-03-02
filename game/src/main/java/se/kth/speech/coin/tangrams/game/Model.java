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
package se.kth.speech.coin.tangrams.game;

import java.util.Arrays;
import java.util.Objects;

import se.kth.speech.Matrix;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 12 Nov 2016
 *
 */
public final class Model<T> {

	private final Matrix<T> coordOccupants;

	public Model(final Matrix<T> coordOccupants) {
		this.coordOccupants = coordOccupants;
	}

	public Model(final Model<T> copyee) {
		this(copyee.getCoordinateOccupants());
	}

	/**
	 * Checks if one set of coordinates is considered adjacent to the other in
	 * regards to moving the piece at the source coordinates to the target
	 * coordinates.
	 * 
	 * <strong>NOTE:</strong> The current implementation means that all
	 * coordinates on either the same <em>x</em> or <em>y</em> axis are
	 * considered &ldquo;adjacent&rdquo;.
	 * 
	 * @param sourceCoords
	 *            The coordinates to move the piece from.
	 * @param targetCoords
	 *            The coordinates to move the piece to.
	 * @return <code>true</code> iff the piece at the source coordinates can
	 *         potentially be moved to the target coordinates.
	 */
	public boolean areCoordinatesAdjacent(final int[] sourceCoords, final int[] targetCoords) {
		final boolean result;

		final int rowDiff = Math.abs(targetCoords[0] - sourceCoords[0]);
		final int colDiff = Math.abs(targetCoords[1] - sourceCoords[1]);
		if (rowDiff > 0) {
			if (colDiff > 0) {
				result = false;
			} else {
				result = true;
			}
		} else {
			if (colDiff > 0) {
				result = true;
			} else {
				result = false;
			}
		}

		return result;
	}

	public boolean areCoordinatesOccupied(final int[] coords) {
		final T occupant = getCoordinateOccupant(coords);
		return occupant != null;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(final Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (!(obj instanceof Model)) {
			return false;
		}
		final Model<?> other = (Model<?>) obj;
		if (coordOccupants == null) {
			if (other.coordOccupants != null) {
				return false;
			}
		} else if (!coordOccupants.equals(other.coordOccupants)) {
			return false;
		}
		return true;
	}

	public int[] getCoordinateDimensions() {
		return coordOccupants.getDimensions();
	}

	public T getCoordinateOccupant(final int[] coords) {
		return coordOccupants.getValue(coords);
	}

	public Matrix<T> getCoordinateOccupants() {
		return coordOccupants;
	}

	public int[] getCoordinatePoint(final int coordOccupantArrayIdx) {
		return coordOccupants.getMatrixIndices(coordOccupantArrayIdx);
	}

	public int getOccupiedCoordinateCount() {
		return (int) Arrays.stream(getCoordinateOccupants().getValues()).filter(Objects::nonNull).count();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (coordOccupants == null ? 0 : coordOccupants.hashCode());
		return result;
	}

	public T setCoordinateOccupant(final int[] coords, final T occupant) {
		return coordOccupants.setValue(coords, occupant);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder();
		builder.append("Model [coordOccupants=");
		builder.append(coordOccupants);
		builder.append("]");
		return builder.toString();
	}

}
