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
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.NavigableSet;
import java.util.Objects;
import java.util.SortedSet;

/**
 * A {@link List} implementation which decorates another {@link List} instance,
 * maintaining a {@link MultiValueMap} which has the list elements {@code E} as
 * keys which are mapped to {@link NavigableSet} objects containing the indices
 * at which each element occurs in the decorated {@code List}.
 *
 * @param <E>
 *            The type of the elements of the decorated {@code List}.
 *
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since 2013-10-15
 *
 */
public final class ReverseLookupList<E> implements Serializable, List<E> {

	/**
	 * The generated serial version UID.
	 */
	private static final long serialVersionUID = -1203502786709867906L;

	/**
	 * The decorated {@link List} instance.
	 */
	private final List<E> decorated;

	/**
	 * The reverse-lookup index map for the elements of {@link #decorated the
	 * decorated <code>List</code>}.
	 */
	private transient MultiValueMap<Object, Integer, NavigableSet<Integer>> reverseLookupMap;

	/**
	 * @param decorated
	 *            The {@link List} to decorate.
	 */
	public ReverseLookupList(final List<E> decorated) {
		this.decorated = decorated;
		this.reverseLookupMap = ListIndex.createListIndexMap(decorated);
	}

	@Override
	public boolean add(final E element) {
		final int index = decorated.size();
		final boolean result = decorated.add(element);
		if (result) {
			final boolean wasIndexPut = reverseLookupMap.putValue(element, index);
			assert wasIndexPut;
		}

		return result;
	}

	@Override
	public void add(final int index, final E element) {
		decorated.add(index, element);
		// shift all indices in map after "index"
		final int lastIndex = decorated.size();
		final List<E> shiftedElements = decorated.subList(index + 1, lastIndex);
		MultiValueMap.incrementValues(reverseLookupMap, shiftedElements, 1, index, lastIndex);
		// Put the new element into the reverse-lookup map after shifting the
		// existing elements because it is possible that the element is present
		// elsewhere in the list and so already has entries in the map
		final boolean wasIndexPut = reverseLookupMap.putValue(element, index);
		assert wasIndexPut;
	}

	@Override
	public boolean addAll(final Collection<? extends E> c) {
		final int lowestNewIndex = decorated.size();
		final boolean result = decorated.addAll(c);
		if (result) {
			// Use a sublist of the decorated list for updating the
			// reverse-lookup map instead of using the argument collection
			// directly because it is possible that not all elements from the
			// argument collection were successfully added
			final int highestNewIndex = decorated.size();
			final List<E> addedElements = decorated.subList(lowestNewIndex, highestNewIndex);
			final boolean wereIndicesPut = MultiValueMap.putIncrementingValues(reverseLookupMap, addedElements,
					lowestNewIndex);
			assert wereIndicesPut;
		}

		return result;
	}

	@Override
	public boolean addAll(final int index, final Collection<? extends E> c) {
		final int oldSize = decorated.size();
		final boolean result = decorated.addAll(index, c);
		if (result) {
			// Use the difference of the new size from the old size because it
			// is possible that not every single element from "c" was
			// successfully added
			final int addedElementCount = decorated.size() - oldSize;
			// Shift indices of existing elements in the reverse-lookup map
			// first to avoid possible key-value pair clashes
			final int lastShiftedElementOldIndex = index + addedElementCount;
			final List<E> shiftedElements = decorated.subList(index, lastShiftedElementOldIndex);
			MultiValueMap.incrementValues(reverseLookupMap, shiftedElements, addedElementCount, index,
					lastShiftedElementOldIndex);
			// Put the new elements into the reverse-lookup map after shifting
			// the
			// existing elements because it is possible that the elements are
			// present
			// elsewhere in the list and so already have entries in the map
			final boolean wasPut = MultiValueMap.putIncrementingValues(reverseLookupMap, c, index);
			assert wasPut;
		}

		return result;
	}

	@Override
	public void clear() {
		decorated.clear();
		reverseLookupMap.clear();
	}

	@Override
	public boolean contains(final Object o) {
		return reverseLookupMap.containsKey(o);
	}

	@Override
	public boolean containsAll(final Collection<?> c) {
		return reverseLookupMap.keySet().containsAll(c);
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
		if (!(obj instanceof ReverseLookupList)) {
			return false;
		}
		final ReverseLookupList<?> other = (ReverseLookupList<?>) obj;
		if (decorated == null) {
			if (other.decorated != null) {
				return false;
			}
		} else if (!decorated.equals(other.decorated)) {
			return false;
		}
		return true;
	}

	@Override
	public E get(final int index) {
		return decorated.get(index);
	}

	/**
	 * @return An unmodifiable view of the decorated {@link List}.
	 */
	public List<E> getDecorated() {
		return Collections.unmodifiableList(decorated);
	}

	/**
	 * @return An unmodifiable view of the reverse-lookup index map for the
	 *         elements of {@link #getDecorated() the decorated
	 *         <code>List</code>}.
	 */
	public Map<Object, NavigableSet<Integer>> getReverseLookupMap() {
		return Collections.unmodifiableMap(reverseLookupMap);
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
	public int indexOf(final Object o) {
		final int result;

		final SortedSet<Integer> indices = reverseLookupMap.get(o);
		if (indices.isEmpty()) {
			result = -1;
		} else {
			result = indices.first();
		}

		return result;
	}

	@Override
	public boolean isEmpty() {
		return decorated.isEmpty();
	}

	@Override
	public Iterator<E> iterator() {
		return decorated.iterator();
	}

	@Override
	public int lastIndexOf(final Object o) {
		final int result;

		final SortedSet<Integer> indices = reverseLookupMap.get(o);
		if (indices.isEmpty()) {
			result = -1;
		} else {
			result = indices.last();
		}

		return result;
	}

	@Override
	public ListIterator<E> listIterator() {
		return decorated.listIterator();
	}

	@Override
	public ListIterator<E> listIterator(final int index) {
		return decorated.listIterator(index);
	}

	@Override
	public E remove(final int index) {
		final E result = decorated.remove(index);
		// Remove the element from the reverse-lookup map before shifting the
		// remaining elements because it is possible that the element is present
		// elsewhere in the list and so still has entries in the map
		final boolean wasRemoved = reverseLookupMap.removeValue(result, index);
		assert wasRemoved;
		// shift all indices in map after "index"
		final int lastIndex = decorated.size();
		final List<E> shiftedElements = decorated.subList(index + 1, lastIndex);
		MultiValueMap.incrementValues(reverseLookupMap, shiftedElements, -1, index, lastIndex);

		return result;
	}

	@Override
	public boolean remove(final Object o) {
		final int indexToRemove = indexOf(o);
		final boolean result;
		if (indexToRemove < 0) {
			result = false;
		} else {
			final E removedElement = remove(indexToRemove);
			assert Objects.equals(o, removedElement);
			result = true;
		}

		return result;

	}

	@Override
	public boolean removeAll(final Collection<?> c) {
		final boolean result = decorated.removeAll(c);
		if (result) {
			// Update the entire reverse-lookup map because it's simpler than
			// trying to deduce which elements were successfully removed
			reverseLookupMap = ListIndex.createListIndexMap(decorated);
		}

		return result;
	}

	@Override
	public boolean retainAll(final Collection<?> c) {
		final boolean result = decorated.retainAll(c);
		if (result) {
			// Update the entire reverse-lookup map because it's simpler than
			// trying to deduce which elements were successfully removed
			reverseLookupMap = ListIndex.createListIndexMap(decorated);
		}

		return result;
	}

	@Override
	public E set(final int index, final E element) {
		final E result = decorated.set(index, element);

		final Integer indexValue = Integer.valueOf(index);
		// Remove the old key-index pair from the reverse-lookup map
		final boolean wasOldKeyRemoved = reverseLookupMap.removeValue(result, indexValue);
		assert wasOldKeyRemoved;
		// Put the new key-index pair into the reverse-lookup map
		final boolean wasNewKeyPut = reverseLookupMap.putValue(element, index);
		assert wasNewKeyPut;

		return result;
	}

	@Override
	public int size() {
		return decorated.size();
	}

	@Override
	public List<E> subList(final int fromIndex, final int toIndex) {
		return decorated.subList(fromIndex, toIndex);
	}

	@Override
	public Object[] toArray() {
		return decorated.toArray();
	}

	@Override
	public <T> T[] toArray(final T[] a) {
		return decorated.toArray(a);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final String decoratedFieldPrefix = "ReverseLookupList [decorated=";
		final String decoratedStr = Objects.toString(decorated);
		final StringBuilder builder = new StringBuilder(decoratedFieldPrefix.length() + decoratedStr.length() + 1);
		builder.append(decoratedFieldPrefix);
		builder.append(decoratedStr);
		builder.append(']');
		return builder.toString();
	}

}
