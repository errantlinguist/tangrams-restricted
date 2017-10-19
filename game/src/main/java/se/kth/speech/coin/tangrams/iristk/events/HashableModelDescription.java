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

import java.util.Arrays;
import java.util.List;

import se.kth.speech.Integers;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 19 Oct 2017
 *
 */
public final class HashableModelDescription {

	private final int colCount;

	private final List<Integer> coordOccupants;

	public HashableModelDescription(final ModelDescription modelDesc) {
		final List<String> coordOccupantDescs = modelDesc.getCoordOccupants();
		coordOccupants = Arrays.asList(coordOccupantDescs.stream()
				.map(Integers::valueOfNullable).toArray(Integer[]::new));
		colCount = modelDesc.getColCount();
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
		if (!(obj instanceof HashableModelDescription)) {
			return false;
		}
		final HashableModelDescription other = (HashableModelDescription) obj;
		if (colCount != other.colCount) {
			return false;
		}
		if (coordOccupants == null) {
			if (other.coordOccupants != null) {
				return false;
			}
		} else if (!coordOccupants.equals(other.coordOccupants)) {
			return false;
		}
		return true;
	}

	/**
	 * @return the colCount
	 */
	public int getColCount() {
		return colCount;
	}

	/**
	 * @return the coordOccupants
	 */
	public List<Integer> getCoordOccupants() {
		return coordOccupants;
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
		result = prime * result + colCount;
		result = prime * result + (coordOccupants == null ? 0 : coordOccupants.hashCode());
		return result;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder((coordOccupants.size() + 1) * 8);
		builder.append("HashableModelDescription [colCount=");
		builder.append(colCount);
		builder.append(", coordOccupants=");
		builder.append(coordOccupants);
		builder.append("]");
		return builder.toString();
	}

}
