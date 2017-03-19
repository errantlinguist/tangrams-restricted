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
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Maps;
import com.google.common.collect.Multimap;

import se.kth.speech.matrix.SpatialRegion;

/**
 * A very rudimentary data structure for searching a two-dimensional space.
 *
 * @author <a href="mailto:tcshore@kth.se>Todd Shore</a>
 * @since 9 Mar 2017
 *
 */
public final class SpatialMap<E> {

	private static final String TABLE_STRING_REPR_ROW_DELIMITER = System.lineSeparator();

	private static final String TABLE_STRING_REPR_COL_DELIMITER = "\t";

	private static void appendOccupiedRegionRepr(final StringBuilder sb,
			final Entry<SpatialRegion, ? extends Iterable<?>> minimalRegionOccupyingElementSet) {
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
	private final Multimap<SpatialRegion, E> regionElements;

	private final Map<E, SpatialRegion> elementRegions;

	/**
	 * An ordered sequence of regions being used, e.g. for getting an ID for a
	 * particular region
	 */
	private transient final List<SpatialRegion> regions;

	public SpatialMap(final int expectedElementCount) {
		this.regionElements = HashMultimap.create(estimateRegionCount(expectedElementCount), expectedElementCount);
		this.elementRegions = Maps.newHashMapWithExpectedSize(expectedElementCount);
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

	public Stream<Entry<SpatialRegion, E>> getIntersectedElements(final SpatialRegion intersectingRegion) {
		// TODO: When turning this class into an actual spatial-indexing system,
		// create
		// heuristic for judging the size of the set of elements contained in a
		// given region (e.g. based on how big the region is)
		return regionElements.entries().stream()
				.filter(regionElementColl -> intersectingRegion.intersects(regionElementColl.getKey()));
	}

	public Multimap<SpatialRegion, E> getMinimalRegionElements() {
		return regionElements;
	}

	public List<SpatialRegion> getMinimalRegions() {
		return regions;
	}

	public Stream<Entry<SpatialRegion, E>> getSubsumedElements(final SpatialRegion subsumingRegion) {
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
	public boolean isOccupied(final SpatialRegion region) {
		boolean result = false;
		for (final SpatialRegion elementRegion : regionElements.keySet()) {
			if(elementRegion.intersects(region)){
				result = true;
				break;
			}
		}
		return result;
	}

	public synchronized SpatialRegion put(final E element, final SpatialRegion region) {
		final SpatialRegion result = elementRegions.put(element, region);
		if (result == null) {
			// The element has never been seen by this instance before
			final boolean wasPut = regionElements.put(region, element);
			assert wasPut;
			final boolean wasAdded = regions.add(region);
			assert wasAdded;
		} else {
			// Add the element to its new region
			final boolean wasPut = regionElements.put(region, element);
			assert wasPut;
			final boolean wasAdded = regions.add(region);
			assert wasAdded;
			// Remove the element from its old region
			final Collection<E> oldRegionElements = regionElements.asMap().get(result);
			final boolean wasRemoved = oldRegionElements.remove(element);
			assert wasRemoved;
			if (oldRegionElements.isEmpty()) {
				// Remove the old region from the set of all regions
				final boolean wasRegionRemoved = regions.remove(result);
				assert wasRegionRemoved;
			}
		}
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
		builder.append("SpatialMap - regionElements:");
		builder.append(TABLE_STRING_REPR_ROW_DELIMITER);
		builder.append(createRegionStringRepr());
		return builder.toString();
	}

	private String createRegionStringRepr() {
		final Stream<String> colNames = Stream.of(SpatialRegion.class.getSimpleName(), "OCCUPANTS...");
		final String header = colNames.collect(Collectors.joining(TABLE_STRING_REPR_COL_DELIMITER));
		final Multimap<SpatialRegion, E> minRegionElements = getMinimalRegionElements();
		final StringBuilder sb = new StringBuilder(header.length() + minRegionElements.size() * 16);
		sb.append(header);
		for (final Entry<SpatialRegion, Collection<E>> minimalRegionOccupyingElementSet : minRegionElements.asMap()
				.entrySet()) {
			sb.append(TABLE_STRING_REPR_ROW_DELIMITER);
			appendOccupiedRegionRepr(sb, minimalRegionOccupyingElementSet);
		}
		return sb.toString();
	}

}
