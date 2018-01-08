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
package se.kth.speech;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.annotation.concurrent.ThreadSafe;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.LinkedHashMultimap;
import com.google.common.collect.Multimap;
import com.google.common.collect.Multimaps;

import it.unimi.dsi.fastutil.objects.Object2ObjectLinkedOpenHashMap;
import it.unimi.dsi.fastutil.objects.Object2ObjectMap;
import it.unimi.dsi.fastutil.objects.Object2ObjectMaps;
import it.unimi.dsi.fastutil.objects.Object2ObjectOpenHashMap;
import se.kth.speech.fastutil.TrimmableObject2ObjectMap;

/**
 * A very rudimentary data structure for searching a two-dimensional space.
 *
 * @author <a href="mailto:errantlinguist+github@gmail.com>Todd Shore</a>
 * @since 9 Mar 2017
 *
 */
@ThreadSafe
public final class SpatialMap<E> {

	public enum Factory {
		STABLE_ITER_ORDER {
			@Override
			public <E> SpatialMap<E> create(final int expectedElementCount) {
				final Multimap<SpatialRegion, E> regionElements = LinkedHashMultimap
						.create(estimateRegionCount(expectedElementCount), expectedElementCount);
				final Object2ObjectLinkedOpenHashMap<E, SpatialRegion> elementRegions = new Object2ObjectLinkedOpenHashMap<>(
						expectedElementCount);
				return new SpatialMap<>(regionElements, new TrimmableObject2ObjectMap<>(elementRegions),
						new ArrayList<>(Math.max(regionElements.size(), 16)));
			}
		},
		UNSTABLE_ITER_ORDER {
			@Override
			public <E> SpatialMap<E> create(final int expectedElementCount) {
				final Multimap<SpatialRegion, E> regionElements = HashMultimap
						.create(estimateRegionCount(expectedElementCount), expectedElementCount);
				final Object2ObjectOpenHashMap<E, SpatialRegion> elementRegions = new Object2ObjectOpenHashMap<>(
						expectedElementCount);
				return new SpatialMap<>(regionElements, new TrimmableObject2ObjectMap<>(elementRegions),
						new ArrayList<>(Math.max(regionElements.size(), 16)));
			}
		};

		public abstract <E> SpatialMap<E> create(final int expectedElementCount);
	}

	private static final String TABLE_STRING_REPR_COL_DELIMITER = "\t";

	private static final String TABLE_STRING_REPR_ROW_DELIMITER = System.lineSeparator();

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

	private final TrimmableObject2ObjectMap<E, SpatialRegion> elementRegions;

	private final ReadWriteLock lock = new ReentrantReadWriteLock();

	/**
	 * <strong>TODO:</strong> Reverse this so that the region is the key pointing to
	 * another {@link SpatialMap} of sub-regions.
	 */
	private final Multimap<SpatialRegion, E> regionElements;

	/**
	 * An ordered sequence of regions being used, e.g. for getting an ID for a
	 * particular region
	 */
	private final ArrayList<SpatialRegion> regions;

	private SpatialMap(final Multimap<SpatialRegion, E> regionElements,
			final TrimmableObject2ObjectMap<E, SpatialRegion> elementRegions, final ArrayList<SpatialRegion> regions) {
		this.regionElements = regionElements;
		this.elementRegions = elementRegions;
		this.regions = regions;
	}

	public void clearRegion(final SpatialRegion region) {
		final Lock writeLock = lock.writeLock();
		writeLock.lock();
		try {
			final Collection<?> elements = regionElements.get(region);
			// NOTE: Iterator.remove() for the instance returned by the
			// multimap's collection iterator throws a
			// ConcurrentModificationException
			elements.clear();
		} finally {
			writeLock.unlock();
		}

	}

	public boolean compact() {
		final Lock writeLock = lock.writeLock();
		writeLock.lock();
		final boolean result;
		try {
			result = elementRegions.trim();
			regions.trimToSize();
		} finally {
			writeLock.unlock();
		}

		return result;
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

		final Lock readLock = lock.readLock();
		readLock.lock();

		final boolean result;
		try {
			result = Objects.equals(this.regionElements, other.regionElements);
		} finally {
			readLock.unlock();
		}
		return result;

	}

	public Collection<E> getAllElements() {
		final Lock readLock = lock.readLock();
		readLock.lock();

		Collection<E> result;
		try {
			result = Collections.unmodifiableCollection(regionElements.values());
		} finally {
			readLock.unlock();
		}
		return result;
	}

	public Object2ObjectMap<E, SpatialRegion> getElementMinimalRegions() {
		final Lock readLock = lock.readLock();
		readLock.lock();

		Object2ObjectMap<E, SpatialRegion> result;
		try {
			result = Object2ObjectMaps.unmodifiable(elementRegions);
		} finally {
			readLock.unlock();
		}
		return result;
	}

	public Stream<Entry<SpatialRegion, E>> getIntersectedElements(final SpatialRegion intersectingRegion) {
		final Lock readLock = lock.readLock();
		readLock.lock();

		Stream<Entry<SpatialRegion, E>> result;
		try {
			// TODO: When turning this class into an actual spatial-indexing system,
			// create
			// heuristic for judging the size of the set of elements contained in a
			// given region (e.g. based on how big the region is)
			result = regionElements.entries().stream()
					.filter(regionElementColl -> intersectingRegion.intersects(regionElementColl.getKey()));
		} finally {
			readLock.unlock();
		}
		return result;

	}

	public Multimap<SpatialRegion, E> getMinimalRegionElements() {
		final Lock readLock = lock.readLock();
		readLock.lock();

		Multimap<SpatialRegion, E> result;
		try {
			result = Multimaps.unmodifiableMultimap(regionElements);
		} finally {
			readLock.unlock();
		}
		return result;
	}

	public List<SpatialRegion> getMinimalRegions() {
		final Lock readLock = lock.readLock();
		readLock.lock();

		List<SpatialRegion> result;
		try {
			result = Collections.unmodifiableList(regions);
		} finally {
			readLock.unlock();
		}
		return result;
	}

	public Stream<Entry<SpatialRegion, E>> getSubsumedElements(final SpatialRegion subsumingRegion) {
		final Lock readLock = lock.readLock();
		readLock.lock();

		Stream<Entry<SpatialRegion, E>> result;
		try {
			// TODO: When turning this class into an actual spatial-indexing system,
			// create
			// heuristic for judging the size of the set of elements contained in a
			// given region (e.g. based on how big the region is)
			result = regionElements.entries().stream()
					.filter(regionElementColl -> subsumingRegion.subsumes(regionElementColl.getKey()));
		} finally {
			readLock.unlock();
		}
		return result;

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

		final Lock readLock = lock.readLock();
		readLock.lock();
		try {
			result = prime * result + (regionElements == null ? 0 : regionElements.hashCode());
		} finally {
			readLock.unlock();
		}

		return result;
	}

	/**
	 * @param region
	 *            The {@link SpatialRegion} to check.
	 * @return <code>true</code> iff the region is occupied by an element.
	 */
	public boolean isOccupied(final SpatialRegion region) {
		boolean result = false;

		final Lock readLock = lock.readLock();
		readLock.lock();

		try {
			for (final SpatialRegion elementRegion : regionElements.keySet()) {
				if (elementRegion.intersects(region)) {
					result = true;
					break;
				}
			}
		} finally {
			readLock.unlock();
		}

		return result;
	}

	public SpatialRegion put(final E element, final SpatialRegion region) {
		final SpatialRegion result;

		final Lock writeLock = lock.writeLock();
		writeLock.lock();

		try {
			result = elementRegions.put(element, region);
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
		} finally {
			writeLock.unlock();
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
		final String prefix = "SpatialMap - regionElements: ";

		final Lock readLock = lock.readLock();
		readLock.lock();

		final StringBuilder builder;
		try {
			final String regionRepr = createRegionStringRepr();
			builder = new StringBuilder(prefix.length() + regionRepr.length());
			builder.append(prefix);
			builder.append(TABLE_STRING_REPR_ROW_DELIMITER);
			builder.append(regionRepr);
		} finally {
			readLock.unlock();
		}
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
