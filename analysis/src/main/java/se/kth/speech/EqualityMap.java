/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.analysis.
 *
 *  tangrams is free software: you can redistribute it and/or modify
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
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Supplier;
import java.util.stream.Stream;

import com.google.common.collect.Sets;

import se.kth.speech.MutablePair;

/**
 * Useful for mapping classes which have no proper
 * {@link Object#hashCode()} functionality.
 * 
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since May 1, 2017
 *
 */
public final class EqualityMap<K, V> implements Map<K, V> {

	private final List<K> keys;

	private final List<V> values;

	public EqualityMap() {
		this(new ArrayList<>(), new ArrayList<>());
	}

	public EqualityMap(final int initialCapacity) {
		this(new ArrayList<>(initialCapacity), new ArrayList<>(initialCapacity));
	}

	private EqualityMap(final List<K> keys, final List<V> values) {
		assert keys.size() == values.size();
		this.keys = keys;
		this.values = values;
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

	public Stream<Entry<K, V>> entries() {
		final Iterator<K> keyIter = keys.iterator();
		final Iterator<V> valueIter = values.iterator();
		final Supplier<Entry<K, V>> entryFactory = () -> new MutablePair<>(keyIter.next(), valueIter.next());
		return Stream.generate(entryFactory).limit(size());
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.Map#entrySet()
	 */
	@Override
	public Set<Entry<K, V>> entrySet() {
		final Set<Entry<K, V>> result = Sets.newHashSetWithExpectedSize(size());
		entries().forEach(result::add);
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
		if (!(obj instanceof EqualityMap)) {
			return false;
		}
		final EqualityMap<?, ?> other = (EqualityMap<?, ?>) obj;
		if (keys == null) {
			if (other.keys != null) {
				return false;
			}
		} else if (!keys.equals(other.keys)) {
			return false;
		}
		if (values == null) {
			if (other.values != null) {
				return false;
			}
		} else if (!values.equals(other.values)) {
			return false;
		}
		return true;
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
		result = prime * result + (keys == null ? 0 : keys.hashCode());
		result = prime * result + (values == null ? 0 : values.hashCode());
		return result;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.Map#isEmpty()
	 */
	@Override
	public boolean isEmpty() {
		return keys.isEmpty();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.Map#keySet()
	 */
	@Override
	public Set<K> keySet() {
		final Set<K> result = Sets.newHashSetWithExpectedSize(size());
		result.addAll(keys);
		return result;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.Map#put(java.lang.Object, java.lang.Object)
	 */
	@Override
	public V put(final K key, final V value) {
		V result;
		int keyIdx = keys.indexOf(key);
		if (keyIdx < 0) {
			keyIdx = keys.size();
			keys.add(key);
			values.add(value);
			result = null;
		} else {
			result = values.set(keyIdx, value);
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
		m.forEach(this::put);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.Map#remove(java.lang.Object)
	 */
	@Override
	public V remove(final Object key) {
		V result;
		final int keyIdx = keys.indexOf(key);
		if (keyIdx < 0) {
			result = null;
		} else {
			result = values.set(keyIdx, null);
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
		return keys.size();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder();
		builder.append("EqualityMap [entrySet()=");
		builder.append(entrySet());
		builder.append("]");
		return builder.toString();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.Map#values()
	 */
	@Override
	public Collection<V> values() {
		return values;
	}

}
