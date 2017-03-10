/*
 * 	Copyright 2013 Todd Shore
 *
 *	Licensed under the Apache License, Version 2.0 (the "License");
 *	you may not use this file except in compliance with the License.
 *	You may obtain a copy of the License at
 *
 *		http://www.apache.org/licenses/LICENSE-2.0
 *
 *	Unless required by applicable law or agreed to in writing, software
 *	distributed under the License is distributed on an "AS IS" BASIS,
 *	WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *	See the License for the specific language governing permissions and
 *	limitations under the License.
 */
package com.github.errantlinguist.collections;

import java.io.Serializable;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.function.Supplier;

/**
 * A {@link Map} which decorates another {@link Map}, which has
 * {@link Collection collections} of values for each key.
 *
 * @param <K>
 *            The key type.
 * @param <V>
 *            The type of the individual values mapped to each key.
 * @param <C>
 *            The type of {@code Collection} object used to contain the values
 *            for each key.
 *
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since 2013-11-03
 */
public final class MultiValueMap<K, V, C extends Collection<V>> implements Map<K, C>, Serializable {

	/**
	 * The generated serial version UID.
	 */
	private static final long serialVersionUID = 6469509507900144140L;

	/**
	 * Increments the values for a given key which occur within a given range.
	 *
	 * @param multimap
	 *            The {@link MultiValueMap} to add to.
	 * @param keysToIncrement
	 *            The keys to increment the values of.
	 * @param increment
	 *            The amount to increment the values by.
	 * @param fromValue
	 *            The inclusive minimum of the key values to increment.
	 * @param toValue
	 *            The exclusive maximum of the key values to increment.
	 */
	public static final <K, C extends SortedSet<Integer>> void incrementValues(
			final MultiValueMap<K, Integer, C> multimap, final Collection<? extends K> keysToIncrement,
			final int increment, final Integer fromValue, final Integer toValue) {
		assert keysToIncrement != null;
		final Collection<Integer> incrementedValues = new HashSet<>(keysToIncrement.size());
		for (final K keyToIncrement : keysToIncrement) {
			incrementValues(multimap, keyToIncrement, increment, fromValue, toValue, incrementedValues);
		}
	}

	/**
	 * Adds one value for each given key, incrementing the value added for the
	 * next key by one.
	 *
	 * @param multimap
	 *            The {@link MultiValueMap} to add to.
	 * @param keysToAdd
	 *            The keys to add.
	 * @param startValue
	 *            The integer value to start at.
	 * @return If at least one value was added to the map.
	 */
	public static final <K, C extends Collection<Integer>> boolean putIncrementingValues(
			final MultiValueMap<K, Integer, C> multimap, final Iterable<? extends K> keysToAdd, Integer startValue) {
		assert keysToAdd != null;
		boolean result = false;

		for (final K keyToAdd : keysToAdd) {
			if (multimap.putValue(keyToAdd, startValue++)) {
				result = true;
			}
		}

		return result;
	}

	/**
	 * Increments the values for a given key which occur within a given range.
	 *
	 * @param multimap
	 *            The {@link MultiValueMap} to add to.
	 * @param keysToIncrement
	 *            The keys to increment the values of.
	 * @param increment
	 *            The amount to increment the values by.
	 * @param fromValue
	 *            The inclusive minimum of the key values to increment.
	 * @param toValue
	 *            The exclusive maximum of the key values to increment.
	 * @param incrementedValues
	 *            A {@link Collection} of elements in the original mapping which
	 *            were incremented.
	 */
	private static final <K, C extends SortedSet<Integer>> void incrementValues(
			final MultiValueMap<K, Integer, C> multimap, final K keyToIncrement, final int increment,
			final Integer fromValue, final Integer toValue, final Collection<Integer> incrementedValues) {
		// Filter out the values outside the specified range of values to update
		final Iterable<Integer> valuesToIncrement = multimap.get(keyToIncrement).subSet(fromValue, toValue);
		for (final Integer valueToIncrement : valuesToIncrement) {
			if (!incrementedValues.contains(valueToIncrement)) {
				// Remove the old value from the map
				final boolean wasOldValueRemoved = multimap.removeValue(keyToIncrement, valueToIncrement);
				assert wasOldValueRemoved;

				final Integer incrementedValue = valueToIncrement + increment;
				// Add the new value to the map
				final boolean wasNewValuePut = multimap.putValue(keyToIncrement, incrementedValue);
				assert wasNewValuePut;

				// Put the old value in the set of already-incremented
				// values
				final boolean wasAdded = incrementedValues.add(valueToIncrement);
				assert wasAdded;
			}
		}
	}

	/**
	 * A {@link Collection} of all elements for all keys in the
	 * {@link #getDecorated() decorated map}.
	 */
	private final transient Collection<V> allValues;

	/**
	 * The decorated {@link Map} instance.
	 */
	private final Map<K, C> decorated;

	/**
	 * The factory used for creating new value collections for the map keys.
	 */
	private final Supplier<? extends C> valueCollectionFactory;

	/**
	 *
	 * @param decorated
	 *            The {@link Map} to decorate.
	 * @param valueCollectionFactory
	 *            The factory to use for creating new value collections for the
	 *            map keys.
	 */
	protected MultiValueMap(final Map<K, C> decorated, final Supplier<? extends C> valueCollectionFactory) {
		this.decorated = decorated;
		this.valueCollectionFactory = valueCollectionFactory;
		this.allValues = IterableElements.createAllElementSet(decorated.values());

	}

	@Override
	public void clear() {
		decorated.clear();
	}

	@Override
	public boolean containsKey(final Object key) {
		return decorated.containsKey(key);
	}

	/**
	 * Checks if a given value is mapped to a given key.
	 *
	 * @param key
	 *            The key to check the values of.
	 * @param value
	 *            The value to check.
	 * @return {@code true} iff the given key maps to the given value.
	 */
	public boolean containsValue(final K key, final V value) {
		final boolean result;

		final C keyValues = decorated.get(key);
		if (keyValues == null) {
			result = false;
		} else {
			result = keyValues.contains(value);
		}
		return result;
	}

	@Override
	public boolean containsValue(final Object value) {
		return allValues.contains(value);
	}

	@Override
	public Set<Entry<K, C>> entrySet() {
		return decorated.entrySet();
	}

	@Override
	public C get(final Object key) {
		return decorated.get(key);
	}

	/**
	 * @return An unmodifiable view of a {@link Collection} of all elements for
	 *         all keys in the {@link #getDecorated() decorated map}.
	 */
	public Collection<V> getAllValues() {
		return Collections.unmodifiableCollection(allValues);
	}

	/**
	 * @return An unmodifiable view of the decorated {@link Map} instance.
	 */
	public Map<K, C> getDecorated() {
		return Collections.unmodifiableMap(decorated);
	}

	/**
	 * Returns all value elements mapped to a key.
	 *
	 * @param key
	 *            The key to get all the elements for.
	 * @return A {@link Collection} of elements; if there is no mapping for the
	 *         given key, then an empty {@code Collection} is returned.
	 */
	public C getValues(final K key) {
		C result = decorated.get(key);
		if (result == null) {
			result = valueCollectionFactory.get();
			decorated.put(key, result);
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
		result = prime * result + (decorated == null ? 0 : decorated.hashCode());
		return result;
	}

	@Override
	public boolean isEmpty() {
		return decorated.isEmpty();
	}

	@Override
	public Set<K> keySet() {
		return decorated.keySet();
	}

	@Override
	public C put(final K key, final C value) {
		final C result = decorated.put(key, value);

		if (result != null) {
			allValues.removeAll(result);
		}
		allValues.addAll(value);

		return result;
	}

	@Override
	public void putAll(final Map<? extends K, ? extends C> m) {
		decorated.putAll(m);
		IterableElements.addAllElements(allValues, m.values());
	}

	/**
	 * Adds a value for a given key.
	 *
	 * @param key
	 *            The key to add the new value to.
	 * @param value
	 *            The value to add.
	 * @return {@code true} iff the value was successfully added.
	 */
	public boolean putValue(final K key, final V value) {
		final C values = getValues(key);
		final boolean result = values.add(value);
		if (result) {
			allValues.add(value);
		}
		return result;
	}

	/**
	 * Adds values for a given key.
	 *
	 * @param key
	 *            The key to add the new values to.
	 * @param values
	 *            The values to add.
	 * @return {@code true} if at least one value was successfully added.
	 */
	public boolean putValues(final K key, final Collection<V> values) {
		final C keyValues = getValues(key);
		final boolean result = keyValues.addAll(values);
		if (result) {
			allValues.addAll(values);
		}
		return result;
	}

	@Override
	public C remove(final Object key) {
		final C result = decorated.remove(key);

		if (result != null) {
			allValues.removeAll(result);
		}

		return result;
	}

	/**
	 * Removes a given value mapped to a given key.
	 *
	 * @param key
	 *            The key to remove the given mapped value for.
	 * @param value
	 *            The value to remove from the {@link Collection} of values
	 *            mapped to the given key.
	 * @return {@code true} iff the value was successfully removed.
	 */
	public boolean removeValue(final K key, final V value) {
		final boolean result;

		final C values = decorated.get(key);
		if (values == null) {
			result = false;
		} else {
			result = values.remove(value);
			if (result) {
				allValues.remove(value);
			}
		}

		return result;
	}

	/**
	 * Removes given values mapped to a given key.
	 *
	 * @param key
	 *            The key to remove the given mapped value for.
	 * @param values
	 *            The values to remove from the {@link Collection} of values
	 *            mapped to the given key.
	 * @return {@code true} if at least one value was successfully removed.
	 */
	public boolean removeValues(final K key, final Collection<V> values) {
		final boolean result;
		final C keyValues = decorated.get(key);
		if (values == null) {
			result = false;
		} else {
			result = keyValues.remove(values);
			if (result) {
				allValues.removeAll(values);
			}
		}

		return result;
	}

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
		builder.append("MultiValueMap [decorated=");
		builder.append(decorated);
		builder.append(", valueCollectionFactory=");
		builder.append(valueCollectionFactory);
		builder.append("]");
		return builder.toString();
	}

	@Override
	public Collection<C> values() {
		return decorated.values();
	}

}
