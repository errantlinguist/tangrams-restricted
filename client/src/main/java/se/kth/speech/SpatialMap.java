/*
 *  This file is part of client.
 *
 *  se.kth.speech.coin.tangrams.client is free software: you can redistribute it and/or modify
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

import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.google.common.collect.Maps;

/**
 * A very rudimentary data structure for searching a two-dimensional space.
 *
 * @author <a href="mailto:tcshore@kth.se>Todd Shore</a>
 * @since 9 Mar 2017
 *
 */
public final class SpatialMap<V> {

	/**
	 * A very rudimentary region of a geographic space, analogous to but much
	 * cruder than the regions of a
	 * <a href="https://en.wikipedia.org/wiki/Quadtree">quadtree</a>.
	 *
	 * @see <a href=
	 *      "https://en.wikipedia.org/wiki/Binary_space_partitioning">Binary
	 *      space partitioning</a>
	 * @author <a href="mailto:tcshore@kth.se>Todd Shore</a>
	 * @since 8 Mar 2017
	 *
	 */
	public static final class Region {

		private static boolean intersects(int firstLower, int secondUpper){
			return !(firstLower > secondUpper);
		}
		
		private static boolean subsumes(final int lower, final int upper, final int point) {
			return lower <= point && point <= upper;
		}

		private final int xLowerBound;

		private final int xUpperBound;

		private final int yLowerBound;

		private final int yUpperBound;

		public Region(final int xLowerBound, final int xUpperBound, final int yLowerBound, final int yUpperBound) {
			this.xLowerBound = xLowerBound;
			this.xUpperBound = xUpperBound;
			this.yLowerBound = yLowerBound;
			this.yUpperBound = yUpperBound;
			if (!areBoundariesValid()) {
				throw new IllegalArgumentException("Boundary values are invalid.");
			}
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
			if (!(obj instanceof Region)) {
				return false;
			}
			final Region other = (Region) obj;
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

		public int getLengthX() {
			return getXUpperBound() - getXLowerBound();
		}

		public int getLengthY() {
			return getYUpperBound() - getYLowerBound();
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
			final int prime = 31;
			int result = 1;
			result = prime * result + xLowerBound;
			result = prime * result + xUpperBound;
			result = prime * result + yLowerBound;
			result = prime * result + yUpperBound;
			return result;
		}
		
//		private static int intersection(int firstLower, int firstUpper, int secondLower, int secondUpper){
//			int upperDiff = firstUpper - secondUpper;
//			int lowerDiff = firstLower - secondLower;
//		}
		
		public boolean intersects(final Region other) {
//			finAL BOOLEAN RESULT;
//			IF (THIS.GETXLOWERBOUND() <= OTHER.GETXUPPERBOUND()) {
//				IF (THIS.GETYLOWERBOUND() <= OTHER.GETYLOWERBOUND()){
//					// THIS SUBSUMES THE OTHER SO OF COURSE IT INTERSECT AS WELL
//					RESULT = TRUE;
//				} ELSE {
//					
//				}
//			}
			// http://stackoverflow.com/a/13390495/1391325
			// If you have four coordinates – ((X,Y),(A,B)) and
			// ((X1,Y1),(A1,B1)) – rather than two plus width and height, it
			// would look like this:
			// if (A<X1 or A1<X or B<Y1 or B1<Y):
			// Intersection = Empty
			// else:
			// Intersection = Not Empty
			return intersectsX(other) && intersectsY(other);

		}
		
		public boolean intersectsX(Region other){
			int thisLower = this.getXLowerBound();
			int otherUpper = other.getXUpperBound();
			final boolean result = intersects(thisLower, otherUpper);
			System.out.println("X - thisLower: " + thisLower + " otherUpper: " + otherUpper + " intersects? " + result);
			return result;
		}

		public boolean intersectsY(Region other){
			int thisLower = this.getYLowerBound();
			int otherUpper = other.getYUpperBound();
			final boolean result = intersects(thisLower, otherUpper);
			System.out.println("Y - thisLower: " + thisLower + " otherUpper: " + otherUpper + " intersects? " + result);
			return result;
		}

		public boolean subsumes(final Region other) {
			return subsumesX(other) && subsumesY(other);
		}

		public boolean subsumesX(final int x) {
			return subsumes(this.getXLowerBound(), this.getXUpperBound(), x);
		}

		public boolean subsumesX(final Region other) {
			return subsumesX(other.getXLowerBound()) && subsumesX(other.getXUpperBound());
		}

		public boolean subsumesY(final int y) {
			return subsumes(this.getYLowerBound(), this.getYUpperBound(), y);
		}

		public boolean subsumesY(final Region other) {
			return subsumesY(other.getYLowerBound()) && subsumesY(other.getYUpperBound());
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.lang.Object#toString()
		 */
		@Override
		public String toString() {
			final StringBuilder builder = new StringBuilder();
			builder.append("Region [xLowerBound=");
			builder.append(xLowerBound);
			builder.append(", xUpperBound=");
			builder.append(xUpperBound);
			builder.append(", yLowerBound=");
			builder.append(yLowerBound);
			builder.append(", yUpperBound=");
			builder.append(yUpperBound);
			builder.append("]");
			return builder.toString();
		}

		private boolean areBoundariesValid() {
			return getXLowerBound() <= getXUpperBound() && getYLowerBound() <= getYUpperBound();
		}

	}

	/**
	 * <strong>TODO:</strong> Reverse this so that the region is the key
	 * pointing to another {@link SpatialMap} of sub-regions.
	 */
	private final Map<V, Region> elementRegions;

	public SpatialMap(final int expectedSize) {
		this(Maps.newHashMapWithExpectedSize(expectedSize));
	}

	private SpatialMap(final Map<V, Region> elementRegions) {
		this.elementRegions = elementRegions;
	}

	public Set<Entry<V, Region>> elementRegions() {
		return elementRegions.entrySet();
	}

	public Set<V> elements() {
		return elementRegions.keySet();
	}

	/**
	 * @param region
	 * @return
	 */
	public boolean isOccupied(final Region region) {
		return elementRegions.values().stream().anyMatch(elementRegion -> elementRegion.intersects(region));
	}

	public void put(final Region region, final V element) {
		elementRegions.put(element, region);
	}

}
