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
package se.kth.speech;

/**
 * A very rudimentary region of a geographic space, analogous to but much cruder
 * than the regions of a
 * <a href="https://en.wikipedia.org/wiki/Quadtree">quadtree</a>.
 *
 * @see <a href=
 *      "https://en.wikipedia.org/wiki/Binary_space_partitioning">Binary space
 *      partitioning</a>
 * @author <a href="mailto:tcshore@kth.se>Todd Shore</a>
 * @since 8 Mar 2017
 *
 */
public final class SpatialRegion {

	/**
	 * @see <a href=
	 *      "http://stackoverflow.com/a/17394265/1391325">StackOverflow</a>
	 * @param r1x1
	 * @param r1x2
	 * @param r1y1
	 * @param r1y2
	 * @param r2x1
	 * @param r2x2
	 * @param r2y1
	 * @param r2y2
	 * @return
	 */
	public static boolean intersects(final int r1x1, final int r1x2, final int r1y1, final int r1y2, final int r2x1,
			final int r2x2, final int r2y1, final int r2y2) {
		final boolean result;
		if (r1x1 < r2x2) {
			if (r1x2 > r2x1) {
				if (r1y1 < r2y2) {
					if (r1y2 > r2y1) {
						result = true;
					} else {
						result = false;
					}
				} else {
					result = false;
				}
			} else {
				result = false;
			}
		} else {
			result = false;
		}
		return result;
	}

	/**
	 * @see <a href=
	 *      "http://stackoverflow.com/q/17394089/1391325">StackOverflow</a>
	 * @param r1x1
	 * @param r1x2
	 * @param r1y1
	 * @param r1y2
	 * @param r2x1
	 * @param r2x2
	 * @param r2y1
	 * @param r2y2
	 * @return
	 */
	public static boolean subsumes(final int r1x1, final int r1x2, final int r1y1, final int r1y2, final int r2x1,
			final int r2x2, final int r2y1, final int r2y2) {
		final boolean result;
		if (r1x1 <= r2x1) {
			if (r1x2 >= r2x2) {
				if (r1y1 <= r2y1) {
					if (r1y2 >= r2y2) {
						result = true;
					} else {
						result = false;
					}
				} else {
					result = false;
				}
			} else {
				result = false;
			}
		} else {
			result = false;
		}
		return result;
	}

	static int[] getDimensions(final int xLowerBound, final int xUpperBound, final int yLowerBound,
			final int yUpperBound) {
		return new int[] { getLength(xLowerBound, xUpperBound), getLength(yLowerBound, yUpperBound) };
	}

	static int getGridArea(final int xLowerBound, final int xUpperBound, final int yLowerBound, final int yUpperBound) {
		return getLength(xLowerBound, xUpperBound) * getLength(yLowerBound, yUpperBound);
	}

	static int getLength(final int lowerBound, final int upperBound) {
		return upperBound - lowerBound;
	}

	private final int hashCode;

	private final int xLowerBound;

	private final int xUpperBound;

	private final int yLowerBound;

	private final int yUpperBound;

	SpatialRegion(final int xLowerBound, final int xUpperBound, final int yLowerBound, final int yUpperBound) {
		this.xLowerBound = xLowerBound;
		this.xUpperBound = xUpperBound;
		this.yLowerBound = yLowerBound;
		this.yUpperBound = yUpperBound;
		if (!areBoundariesValid()) {
			throw new IllegalArgumentException(String.format("Boundary values are invalid: %d, %d, %d, %d", xLowerBound,
					xUpperBound, yLowerBound, yUpperBound));
		}

		hashCode = createHashCode();
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
		if (!(obj instanceof SpatialRegion)) {
			return false;
		}
		final SpatialRegion other = (SpatialRegion) obj;
		if (xLowerBound != other.xLowerBound) {
			return false;
		}
		if (xUpperBound != other.xUpperBound) {
			return false;
		}
		if (yLowerBound != other.yLowerBound) {
			return false;
		}
		if (yUpperBound != other.yUpperBound) {
			return false;
		}
		return true;
	}

	public int[] getDimensions() {
		return new int[] { getLengthX(), getLengthY() };
	}

	public int getGridArea() {
		return getLengthX() * getLengthY();
	}

	public int getLengthX() {
		return getLength(getXLowerBound(), getXUpperBound());
	}

	public int getLengthY() {
		return getLength(getYLowerBound(), getYUpperBound());
	}

	/**
	 * @return the xLowerBound
	 */
	public int getXLowerBound() {
		return xLowerBound;
	}

	/**
	 * @return the xUpperBound
	 */
	public int getXUpperBound() {
		return xUpperBound;
	}

	/**
	 * @return the yLowerBound
	 */
	public int getYLowerBound() {
		return yLowerBound;
	}

	/**
	 * @return the yUpperBound
	 */
	public int getYUpperBound() {
		return yUpperBound;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return hashCode;
	}

	public boolean intersects(final SpatialRegion other) {
		return intersects(getXLowerBound(), getXUpperBound(), getYLowerBound(), getYUpperBound(),
				other.getXLowerBound(), other.getXUpperBound(), other.getYLowerBound(), other.getYUpperBound());
	}

	public boolean subsumes(final SpatialRegion other) {
		return subsumes(getXLowerBound(), getXUpperBound(), getYLowerBound(), getYUpperBound(), other.getXLowerBound(),
				other.getXUpperBound(), other.getYLowerBound(), other.getYUpperBound());
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder(48);
		builder.append("SpatialRegion [x=[");
		builder.append(xLowerBound);
		builder.append(", ");
		builder.append(xUpperBound);
		builder.append("), y=[");
		builder.append(yLowerBound);
		builder.append(", ");
		builder.append(yUpperBound);
		builder.append(")]");
		return builder.toString();
	}

	private boolean areBoundariesValid() {
		return getXLowerBound() < getXUpperBound() && getYLowerBound() < getYUpperBound();
	}

	private int createHashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + xLowerBound;
		result = prime * result + xUpperBound;
		result = prime * result + yLowerBound;
		result = prime * result + yUpperBound;
		return result;
	}

	int getColumnEndIdx() {
		return getYUpperBound();
	}

	int getColumnStartIdx() {
		return getYLowerBound();
	}

	int getRowEndIdx() {
		return getXUpperBound();
	}

	int getRowStartIdx() {
		return getXLowerBound();
	}

}