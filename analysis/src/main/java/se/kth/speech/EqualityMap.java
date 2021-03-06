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
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Queue;
import java.util.Set;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import com.google.common.collect.Sets;

/**
 * Useful for mapping classes which have no proper {@link Object#hashCode()}
 * functionality. <strong>TODO:</strong> Make changes to collections returned by
 * {@link #keySet()} {@link #entrySet()} and {@link #values()} propagate to
 * backing arrays and vice versa
 *
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since May 1, 2017
 *
 */
public final class EqualityMap<K, V> implements Map<K, V> {

	private class IndexedEntry implements Entry<K, V> {

		private final int idx;

		private IndexedEntry(final int idx) {
			this.idx = idx;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.lang.Object#equals(java.lang.Object)
		 */
		@Override
		public boolean equals(final Object obj) {
			final boolean result;
			if (this == obj) {
				result = true;
			} else if (obj == null) {
				result = false;
			} else if (obj instanceof Entry<?, ?>) {
				final Entry<?, ?> other = (Entry<?, ?>) obj;
				result = Objects.equals(this.getKey(), other.getKey())
						&& Objects.equals(this.getValue(), other.getValue());
			} else {
				result = false;
			}
			return result;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.util.Map.Entry#getKey()
		 */
		@Override
		public K getKey() {
			return keys.get(idx);
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.util.Map.Entry#getValue()
		 */
		@Override
		public V getValue() {
			return values.get(idx);
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
			result = prime * result + Objects.hashCode(getKey());
			result = prime * result + Objects.hashCode(getValue());
			return result;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.util.Map.Entry#setValue(java.lang.Object)
		 */
		@Override
		public V setValue(final V value) {
			return values.set(idx, value);
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.lang.Object#toString()
		 */
		@Override
		public String toString() {
			final StringBuilder builder = new StringBuilder(64);
			builder.append("IndexedEntry [getKey()=");
			builder.append(getKey());
			builder.append(", getValue()=");
			builder.append(getValue());
			builder.append(']');
			return builder.toString();
		}

	}

	/**
	 * Default initial capacity.
	 */
	private static final int DEFAULT_CAPACITY = 16;

	private static <T> Collector<T, ?, ArrayList<T>> createArrayListCollector(final int initialCapacity) {
		return Collectors.toCollection(() -> new ArrayList<>(initialCapacity));
	}

	private final Queue<Integer> freedIdxs;

	private final ArrayList<Boolean> idxOccupations;

	private final ArrayList<K> keys;

	private int occupiedIdxCount;

	private final ArrayList<V> values;

	public EqualityMap() {
		this(DEFAULT_CAPACITY);
	}

	public EqualityMap(final int initialCapacity) {
		this(new ArrayList<>(initialCapacity), new ArrayList<>(initialCapacity),
				new ArrayList<Boolean>(initialCapacity));
	}

	public EqualityMap(final Map<? extends K, ? extends V> copyee) {
		// Put the entries into an ArrayList in order to ensure that the iteration order is stable
		this(new ArrayList<>(copyee.entrySet()));
	}

	private EqualityMap(final ArrayList<K> keys, final ArrayList<V> values, final ArrayList<Boolean> idxOccupations) {
		this.keys = keys;
		this.values = values;
		assert keys.size() == values.size();
		this.idxOccupations = idxOccupations;
		assert keys.size() == idxOccupations.size();

		freedIdxs = new LinkedList<>();
	}

	private EqualityMap(final List<? extends Entry<? extends K, ? extends V>> entries) {
		this(entries.stream().map(Entry::getKey).collect(createArrayListCollector(entries.size())),
				entries.stream().map(Entry::getValue).collect(createArrayListCollector(entries.size())),
				entries.stream().map(entry -> Boolean.TRUE).collect(createArrayListCollector(entries.size())));
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.Map#clear()
	 */
	@Override
	public void clear() {
		keys.clear();
		values.clear();
		idxOccupations.clear();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.Map#containsKey(java.lang.Object)
	 */
	@Override
	public boolean containsKey(final Object key) {
		return keys.contains(key);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.Map#containsValue(java.lang.Object)
	 */
	@Override
	public boolean containsValue(final Object value) {
		return values.contains(value);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.Map#entrySet()
	 */
	@Override
	public Set<Entry<K, V>> entrySet() {
		final int currentSize = size();
		return occupiedIdxs().mapToObj(IndexedEntry::new)
				.collect(Collectors.toCollection(() -> Sets.newHashSetWithExpectedSize(currentSize)));
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(final Object obj) {
		final boolean result;
		if (this == obj) {
			result = true;
		} else if (obj == null) {
			result = false;
		} else if (obj instanceof EqualityMap) {
			final EqualityMap<?, ?> other = (EqualityMap<?, ?>) obj;
			result = Objects.equals(this.entrySet(), other.entrySet());
		} else {
			result = false;
		}
		return result;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.Map#get(java.lang.Object)
	 */
	@Override
	public V get(final Object key) {
		final int keyIdx = keys.indexOf(key);
		return keyIdx < 0 ? null : values.get(keyIdx);
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
		result = prime * result + Objects.hashCode(entrySet());
		return result;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.Map#isEmpty()
	 */
	@Override
	public boolean isEmpty() {
		return size() <= 0;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.Map#keySet()
	 */
	@Override
	public Set<K> keySet() {
		final int currentSize = size();
		return occupiedIdxs().mapToObj(keys::get)
				.collect(Collectors.toCollection(() -> Sets.newLinkedHashSetWithExpectedSize(currentSize)));
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.Map#put(java.lang.Object, java.lang.Object)
	 */
	@Override
	public V put(final K key, final V value) {
		V result;
		final int occupiedIdx = keys.indexOf(key);
		if (occupiedIdx < 0) {
			addNewIndex(key, value);
			result = null;
		} else {
			result = values.set(occupiedIdx, value);
		}
		return result;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.Map#putAll(java.util.Map)
	 */
	@Override
	public void putAll(final Map<? extends K, ? extends V> m) {
		final int minSize = m.size();
		ensureCapacity(minSize);
		m.forEach(this::put);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.Map#remove(java.lang.Object)
	 */
	@Override
	public V remove(final Object key) {
		final V result;
		final int keyIdx = keys.indexOf(key);
		if (keyIdx < 0) {
			result = null;
		} else {
			result = clearIndex(keyIdx);
		}
		return result;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.Map#size()
	 */
	@Override
	public int size() {
		return occupiedIdxCount;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final Set<Entry<K, V>> entrySet = entrySet();
		final StringBuilder builder = new StringBuilder(64 * entrySet.size() + 32);
		builder.append("EqualityMap [entrySet()=");
		builder.append(entrySet);
		builder.append(']');
		return builder.toString();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.Map#values()
	 */
	@Override
	public Collection<V> values() {
		final int currentSize = size();
		return occupiedIdxs().mapToObj(values::get)
				.collect(Collectors.toCollection(() -> new ArrayList<>(currentSize)));
	}

	private void addNewIndex(final K key, final V value) {
		final Integer freedIdx = freedIdxs.poll();
		if (freedIdx == null) {
			keys.add(key);
			values.add(value);
			idxOccupations.add(Boolean.TRUE);
		} else {
			final Boolean wasOccupied = set(freedIdx, key, value);
			assert !wasOccupied;
		}
		occupiedIdxCount++;
	}

	private V clearIndex(final int index) {
		keys.set(index, null);
		final V result = values.set(index, null);
		final Boolean wasOccupied = idxOccupations.set(index, Boolean.FALSE);
		assert wasOccupied;
		freedIdxs.add(index);
		occupiedIdxCount--;
		return result;
	}

	private void ensureCapacity(final int capacity) {
		keys.ensureCapacity(capacity);
		values.ensureCapacity(capacity);
		idxOccupations.ensureCapacity(capacity);
	}

	private IntStream occupiedIdxs() {
		return IntStream.range(0, idxOccupations.size()).filter(idxOccupations::get);
	}

	private Boolean set(final int index, final K key, final V value) {
		keys.set(index, key);
		values.set(index, value);
		return idxOccupations.set(index, Boolean.TRUE);
	}

}
