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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map.Entry;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;

/**
 * A very rudimentary data structure for searching a two-dimensional space.
 *
 * @author <a href="mailto:tcshore@kth.se>Todd Shore</a>
 * @since 9 Mar 2017
 *
 */
public final class SpatialMap<E> {

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
			return r1x1 < r2x2 && r1x2 > r2x1 && r1y1 < r2y2 && r1y2 > r2y1;
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
			return r1x1 >= r2x1 && r1x2 >= r2x2 && r1y1 >= r2y1 && r1y2 <= r2y2;
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

		public boolean intersects(final Region other) {
			return intersects(this.getXLowerBound(), this.getXUpperBound(), this.getYLowerBound(),
					this.getYUpperBound(), other.getXLowerBound(), other.getXUpperBound(), other.getYLowerBound(),
					other.getYUpperBound());
		}

		public boolean subsumes(final Region other) {
			return subsumes(this.getXLowerBound(), this.getXUpperBound(), this.getYLowerBound(), this.getYUpperBound(),
					other.getXLowerBound(), other.getXUpperBound(), other.getYLowerBound(), other.getYUpperBound());
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.lang.Object#toString()
		 */
		@Override
		public String toString() {
			final StringBuilder builder = new StringBuilder();
			builder.append("Region [x=[");
			builder.append(xLowerBound);
			builder.append(", ");
			builder.append(xUpperBound);
			builder.append("], y=[");
			builder.append(yLowerBound);
			builder.append(", ");
			builder.append(yUpperBound);
			builder.append("]");
			builder.append("]");
			return builder.toString();
		}

		private boolean areBoundariesValid() {
			return getXLowerBound() <= getXUpperBound() && getYLowerBound() <= getYUpperBound();
		}

	}

	private static final String TABLE_STRING_REPR_ROW_DELIMITER = System.lineSeparator();

	private static final String TABLE_STRING_REPR_COL_DELIMITER = "\t";

	private static void appendOccupiedRegionRepr(final StringBuilder sb,
			final Entry<Region, ? extends Iterable<?>> minimalRegionOccupyingElementSet) {
		sb.append(minimalRegionOccupyingElementSet.getKey());
		for (final Object occupyingElement : minimalRegionOccupyingElementSet.getValue()) {
			sb.append(TABLE_STRING_REPR_COL_DELIMITER);
			sb.append(occupyingElement);
		}
	}

	private static int estimateRegionCount(final int expectedElementCount) {
		// TODO: implement
		return expectedElementCount;
	}

	/**
	 * <strong>TODO:</strong> Reverse this so that the region is the key
	 * pointing to another {@link SpatialMap} of sub-regions.
	 */
	private final Multimap<Region, E> regionElements;

	/**
	 * An ordered sequence of regions being used, e.g. for getting an ID for a
	 * particular region
	 */
	private transient final List<Region> regions;

	public SpatialMap(final int expectedElementCount) {
		this(HashMultimap.create(estimateRegionCount(expectedElementCount), expectedElementCount));
	}

	private SpatialMap(final Multimap<Region, E> regionElements) {
		this.regionElements = regionElements;
		this.regions = new ArrayList<>(Math.max(regionElements.size(), 16));
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
		if (!(obj instanceof SpatialMap)) {
			return false;
		}
		final SpatialMap<?> other = (SpatialMap<?>) obj;
		if (regionElements == null) {
			if (other.regionElements != null) {
				return false;
			}
		} else if (!regionElements.equals(other.regionElements)) {
			return false;
		}
		return true;
	}

	public Collection<E> getAllElements() {
		return regionElements.values();
	}

	public Stream<Entry<Region, E>> getIntersectedElements(final Region intersectingRegion) {
		// TODO: When turning this class into an actual spatial-indexing system,
		// create
		// heuristic for judging the size of the set of elements contained in a
		// given region (e.g. based on how big the region is)
		return regionElements.entries().stream()
				.filter(regionElementColl -> intersectingRegion.intersects(regionElementColl.getKey()));
	}

	public Multimap<Region, E> getMinimalRegionElements() {
		return regionElements;
	}

	public List<Region> getMinimalRegions() {
		return regions;
	}

	public Stream<Entry<Region, E>> getSubsumedElements(final Region subsumingRegion) {
		// TODO: When turning this class into an actual spatial-indexing system,
		// create
		// heuristic for judging the size of the set of elements contained in a
		// given region (e.g. based on how big the region is)
		return regionElements.entries().stream()
				.filter(regionElementColl -> subsumingRegion.subsumes(regionElementColl.getKey()));
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
		result = prime * result + (regionElements == null ? 0 : regionElements.hashCode());
		return result;
	}

	/**
	 * @param region
	 * @return
	 */
	public boolean isOccupied(final Region region) {
		return regionElements.keys().stream().anyMatch(elementRegion -> elementRegion.intersects(region));
	}

	public void put(final E element, final Region region) {
		regionElements.put(region, element);
		regions.add(region);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder();
		builder.append("SpatialMap  - regionElements:");
		builder.append(TABLE_STRING_REPR_ROW_DELIMITER);
		builder.append(createRegionStringRepr());
		return builder.toString();
	}

	private String createRegionStringRepr() {
		final Stream<String> colNames = Stream.of(Region.class.getSimpleName(), "OCCUPANTS...");
		final String header = colNames.collect(Collectors.joining(TABLE_STRING_REPR_COL_DELIMITER));
		final Multimap<Region, E> minRegionElements = getMinimalRegionElements();
		final StringBuilder sb = new StringBuilder(header.length() + minRegionElements.size() * 16);
		sb.append(header);
		for (final Entry<Region, Collection<E>> minimalRegionOccupyingElementSet : minRegionElements.asMap()
				.entrySet()) {
			sb.append(TABLE_STRING_REPR_ROW_DELIMITER);
			appendOccupiedRegionRepr(sb, minimalRegionOccupyingElementSet);
		}
		return sb.toString();
	}

}
