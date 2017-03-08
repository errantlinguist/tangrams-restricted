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

import java.util.Arrays;
import java.util.Map;

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

		private final int[] boundaries;

		public Region(final int xLowerBound, final int xUpperBound, final int yLowerBound, final int yUpperBound) {
			boundaries = new int[] { xLowerBound, xUpperBound, yLowerBound, yUpperBound };
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
			if (!Arrays.equals(boundaries, other.boundaries)) {
				return false;
			}
			return true;
		}

		public int getXLowerBound() {
			return boundaries[0];
		}

		public int getXUpperBound() {
			return boundaries[1];
		}

		public int getYLowerBound() {
			return boundaries[2];
		}

		public int getYUpperBound() {
			return boundaries[3];
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
			result = prime * result + Arrays.hashCode(boundaries);
			return result;
		}

		public boolean intersects(final Region other) {
			final int otherXLowerBound = other.getXLowerBound();
			final int otherXUpperBound = other.getXUpperBound();
			// if (otherXLowerBound > this.getXLowerBound() && otherXLowerBound
			// < this.getXUpperBound()
			// || ){
			//
			// }
			// TODO: Finish implementation
			return false;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.lang.Object#toString()
		 */
		@Override
		public String toString() {
			return "SpatialRegion [boundaries=" + Arrays.toString(boundaries) + "]";
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

	/**
	 * @param region
	 * @return
	 */
	public boolean isOccupied(final Region region) {
		return elementRegions.values().stream().anyMatch(elementRegion -> elementRegion.intersects(region));
	}

	/**
	 * @param imgViewInfoDatum
	 * @param imgRegion
	 */
	public void put(final Region region, final V element) {
		elementRegions.put(element, region);
	}

}
