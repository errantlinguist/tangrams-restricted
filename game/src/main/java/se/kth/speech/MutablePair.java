/*
 *  This file is part of se.kth.speech.coin.tangrams.game.
 *
 *  se.kth.speech.coin.tangrams.game is free software: you can redistribute it and/or modify
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
package se.kth.speech;

import java.util.Map.Entry;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 2 Feb 2017
 *
 */
public final class MutablePair<L, R> implements Entry<L, R> {

	private L left;

	private R right;

	public MutablePair(final L left, final R right) {
		this.left = left;
		this.right = right;
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
		if (!(obj instanceof MutablePair)) {
			return false;
		}
		final MutablePair<?, ?> other = (MutablePair<?, ?>) obj;
		if (left == null) {
			if (other.left != null) {
				return false;
			}
		} else if (!left.equals(other.left)) {
			return false;
		}
		if (right == null) {
			if (other.right != null) {
				return false;
			}
		} else if (!right.equals(other.right)) {
			return false;
		}
		return true;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.Map.Entry#getKey()
	 */
	@Override
	public L getKey() {
		return getLeft();
	}

	/**
	 * @return the left
	 */
	public L getLeft() {
		return left;
	}

	/**
	 * @return the right
	 */
	public R getRight() {
		return right;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.Map.Entry#getValue()
	 */
	@Override
	public R getValue() {
		return getRight();
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
		result = prime * result + (left == null ? 0 : left.hashCode());
		result = prime * result + (right == null ? 0 : right.hashCode());
		return result;
	}

	/**
	 * @param left
	 *            the left to set
	 */
	public void setLeft(final L left) {
		this.left = left;
	}

	/**
	 * @param right
	 *            the right to set
	 */
	public void setRight(final R right) {
		this.right = right;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.Map.Entry#setValue(java.lang.Object)
	 */
	@Override
	public R setValue(final R value) {
		final R result = getRight();
		setRight(value);
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
		builder.append("MutablePair [getLeft()=");
		builder.append(getLeft());
		builder.append(", getRight()=");
		builder.append(getRight());
		builder.append("]");
		return builder.toString();
	}

}
