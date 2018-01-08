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

import se.kth.speech.SpatialRegion;

/**
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since 19 Oct 2017
 *
 */
public final class Selection {

	private final SpatialRegion area;

	private final int pieceId;

	public Selection(final int pieceId, final SpatialRegion area) {
		this.pieceId = pieceId;
		this.area = area;
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
		if (!(obj instanceof Selection)) {
			return false;
		}
		final Selection other = (Selection) obj;
		if (area == null) {
			if (other.area != null) {
				return false;
			}
		} else if (!area.equals(other.area)) {
			return false;
		}
		if (pieceId != other.pieceId) {
			return false;
		}
		return true;
	}

	/**
	 * @return the area
	 */
	public SpatialRegion getArea() {
		return area;
	}

	/**
	 * @return the pieceId
	 */
	public int getPieceId() {
		return pieceId;
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
		result = prime * result + (area == null ? 0 : area.hashCode());
		result = prime * result + pieceId;
		return result;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder(64);
		builder.append("Selection [area=");
		builder.append(area);
		builder.append(", pieceId=");
		builder.append(pieceId);
		builder.append("]");
		return builder.toString();
	}

}
