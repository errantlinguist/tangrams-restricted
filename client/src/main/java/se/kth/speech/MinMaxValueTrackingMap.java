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

import java.util.Collection;
import java.util.Comparator;
import java.util.Map;
import java.util.Set;

import com.google.api.client.repackaged.com.google.common.base.Objects;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 16 Mar 2017
 *
 */
public final class MinMaxValueTrackingMap<K, V> implements Map<K, V> {

	public static <K, V extends Comparable<? super V>> MinMaxValueTrackingMap<K, V> create(final Map<K, V> decorated) {
		return new MinMaxValueTrackingMap<>(decorated, Comparator.naturalOrder());
	}

	public static <K, V> MinMaxValueTrackingMap<K, V> create(final Map<K, V> decorated,
			final Comparator<? super V> comp) {
		return new MinMaxValueTrackingMap<>(decorated, comp);
	}

	private final Map<K, V> decorated;

	private transient final Comparator<? super V> nullsFirstComp;

	private transient final Comparator<? super V> nullsLastComp;

	private transient V maxValue;

	private transient V minValue;

	private MinMaxValueTrackingMap(final Map<K, V> decorated, final Comparator<? super V> comp) {
		this.decorated = decorated;
		this.nullsLastComp = Comparator.nullsLast(comp);
		this.nullsFirstComp = Comparator.nullsFirst(comp);

		minValue = null;
		maxValue = null;
		updateExtrema(decorated.values());
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.Map#clear()
	 */
	@Override
	public void clear() {
		decorated.clear();

	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.Map#containsKey(java.lang.Object)
	 */
	@Override
	public boolean containsKey(final Object key) {
		return decorated.containsKey(key);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.Map#containsValue(java.lang.Object)
	 */
	@Override
	public boolean containsValue(final Object value) {
		return decorated.containsValue(value);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.Map#entrySet()
	 */
	@Override
	public Set<java.util.Map.Entry<K, V>> entrySet() {
		return decorated.entrySet();
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
		if (!(obj instanceof MinMaxValueTrackingMap)) {
			return false;
		}
		final MinMaxValueTrackingMap<?, ?> other = (MinMaxValueTrackingMap<?, ?>) obj;
		if (decorated == null) {
			if (other.decorated != null) {
				return false;
			}
		} else if (!decorated.equals(other.decorated)) {
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
		return decorated.get(key);
	}

	/**
	 * @return the maxValue
	 */
	public V getMaxValue() {
		return maxValue;
	}

	/**
	 * @return the minValue
	 */
	public V getMinValue() {
		return minValue;
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
		result = prime * result + (decorated == null ? 0 : decorated.hashCode());
		return result;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.Map#isEmpty()
	 */
	@Override
	public boolean isEmpty() {
		return decorated.isEmpty();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.Map#keySet()
	 */
	@Override
	public Set<K> keySet() {
		return decorated.keySet();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.Map#put(java.lang.Object, java.lang.Object)
	 */
	@Override
	public V put(final K key, final V value) {
		final V result = decorated.put(key, value);
		updateExtrema(value);
		return result;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.Map#putAll(java.util.Map)
	 */
	@Override
	public void putAll(final Map<? extends K, ? extends V> m) {
		decorated.putAll(m);
		updateExtrema(m.values());
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.Map#remove(java.lang.Object)
	 */
	@Override
	public V remove(final Object key) {
		final V result = decorated.remove(key);
		if (Objects.equal(minValue, result)) {
			if (Objects.equal(maxValue, result)) {
				// Update both min and max
				updateExtrema();
			} else {
				// Value was not max so only the min needs to be updated
				updateMin();
			}
		} else if (Objects.equal(maxValue, result)) {
			// Value was not min so only the max needs to be updated
			updateMax();
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
		return decorated.size();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder();
		builder.append("MinMaxValueTrackingMap [decorated=");
		builder.append(decorated);
		builder.append(", minValue=");
		builder.append(minValue);
		builder.append(", maxValue=");
		builder.append(maxValue);
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
		return decorated.values();
	}

	private void updateExtrema() {
		updateExtrema(values());
	}

	private void updateExtrema(final Iterable<? extends V> values) {
		for (final V value : values) {
			updateExtrema(value);
		}
	}

	private void updateExtrema(final V value) {
		updateMin(value);
		updateMax(value);
	}

	private void updateMax() {
		updateMax(values());
	}

	private void updateMax(final Iterable<? extends V> values) {
		for (final V value : values) {
			updateMax(value);
		}
	}

	private void updateMax(final V value) {
		final int cmp = nullsFirstComp.compare(maxValue, value);
		if (cmp < 0) {
			maxValue = value;
		}
	}

	private void updateMin() {
		updateMin(values());
	}

	private void updateMin(final Iterable<? extends V> values) {
		for (final V value : values) {
			updateMin(value);
		}
	}

	private void updateMin(final V value) {
		final int cmp = nullsLastComp.compare(value, minValue);
		if (cmp < 0) {
			minValue = value;
		}
	}

}
