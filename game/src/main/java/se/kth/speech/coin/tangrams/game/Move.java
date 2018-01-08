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
package se.kth.speech.coin.tangrams.game;

import se.kth.speech.SpatialRegion;

/**
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since 19 Oct 2017
 *
 */
public final class Move {

	private final int pieceId;

	private final SpatialRegion source;

	private final SpatialRegion target;

	public Move(final Move copyee) {
		this(copyee.getSource(), copyee.getTarget(), copyee.getPieceId());
	}

	public Move(final SpatialRegion source, final SpatialRegion target, final int pieceId) {
		this.source = source;
		this.target = target;
		this.pieceId = pieceId;
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
		if (!(obj instanceof Move)) {
			return false;
		}
		final Move other = (Move) obj;
		if (pieceId != other.pieceId) {
			return false;
		}
		if (source == null) {
			if (other.source != null) {
				return false;
			}
		} else if (!source.equals(other.source)) {
			return false;
		}
		if (target == null) {
			if (other.target != null) {
				return false;
			}
		} else if (!target.equals(other.target)) {
			return false;
		}
		return true;
	}

	/**
	 * @return the pieceId
	 */
	public int getPieceId() {
		return pieceId;
	}

	/**
	 * @return the source
	 */
	public SpatialRegion getSource() {
		return source;
	}

	/**
	 * @return the target
	 */
	public SpatialRegion getTarget() {
		return target;
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
		result = prime * result + pieceId;
		result = prime * result + (source == null ? 0 : source.hashCode());
		result = prime * result + (target == null ? 0 : target.hashCode());
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
		builder.append("Move [pieceId=");
		builder.append(pieceId);
		builder.append(", source=");
		builder.append(source);
		builder.append(", target=");
		builder.append(target);
		builder.append("]");
		return builder.toString();
	}

}
