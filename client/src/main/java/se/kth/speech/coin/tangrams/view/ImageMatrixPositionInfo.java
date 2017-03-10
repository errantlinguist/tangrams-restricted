/*
 *  This file is part of client.
 *
 *  client is free software: you can redistribute it and/or modify
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
package se.kth.speech.coin.tangrams.view;

import java.awt.Image;
import java.util.Arrays;
import java.util.Map.Entry;

final class ImageMatrixPositionInfo<I> {

	private final Entry<? extends Image, ImageViewInfo> imgViewInfoDatum;

	private final I pieceId;

	private final int[] piecePosMatrixSize;

	ImageMatrixPositionInfo(final I pieceId, final int[] piecePosMatrixSize,
			final Entry<? extends Image, ImageViewInfo> imgViewInfoDatum) {
		this.pieceId = pieceId;
		this.piecePosMatrixSize = piecePosMatrixSize;
		this.imgViewInfoDatum = imgViewInfoDatum;
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
		if (!(obj instanceof ImageMatrixPositionInfo)) {
			return false;
		}
		final ImageMatrixPositionInfo<?> other = (ImageMatrixPositionInfo<?>) obj;
		if (imgViewInfoDatum == null) {
			if (other.imgViewInfoDatum != null) {
				return false;
			}
		} else if (!imgViewInfoDatum.equals(other.imgViewInfoDatum)) {
			return false;
		}
		if (pieceId == null) {
			if (other.pieceId != null) {
				return false;
			}
		} else if (!pieceId.equals(other.pieceId)) {
			return false;
		}
		if (!Arrays.equals(piecePosMatrixSize, other.piecePosMatrixSize)) {
			return false;
		}
		return true;
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
		result = prime * result + (imgViewInfoDatum == null ? 0 : imgViewInfoDatum.hashCode());
		result = prime * result + (pieceId == null ? 0 : pieceId.hashCode());
		result = prime * result + Arrays.hashCode(piecePosMatrixSize);
		return result;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder();
		builder.append("ImageMatrixPositionInfo [imgViewInfoDatum=");
		builder.append(imgViewInfoDatum);
		builder.append(", pieceId=");
		builder.append(pieceId);
		builder.append(", piecePosMatrixSize=");
		builder.append(Arrays.toString(piecePosMatrixSize));
		builder.append("]");
		return builder.toString();
	}

	/**
	 * @return the imgViewInfoDatum
	 */
	Entry<? extends Image, ImageViewInfo> getImgViewInfoDatum() {
		return imgViewInfoDatum;
	}

	/**
	 * @return the pieceId
	 */
	I getPieceId() {
		return pieceId;
	}

	/**
	 * @return the piecePosMatrixSize
	 */
	int[] getPiecePosMatrixSize() {
		return piecePosMatrixSize;
	}
}