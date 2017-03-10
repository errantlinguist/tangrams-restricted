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

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

/**
 * A utility class for manipulating {@link Collection} elements.
 *
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since 2013-10-30
 *
 */
public final class IterableElements {

	/**
	 * Adds all elements from an arbitrary number of {@link Collection} objects
	 * to a single given {@link Collection} instance.
	 *
	 * @param allElements
	 *            The {@link Collection} to add all the elements to.
	 * @param collections
	 *            The collections for each of which all elements are to be
	 *            added.
	 */
	public static final <E> void addAllElements(final Collection<E> allElements,
			final Iterable<? extends Collection<E>> collections) {
		for (final Collection<E> collection : collections) {
			allElements.addAll(collection);
		}
	}

	/**
	 * Checks if all the elements in a given {@link Collection} are unique,
	 * i.e.&nbsp;that no element occurs more than once.
	 *
	 * @param collection
	 *            The {@code Collection} to check.
	 * @return {@code} true iff there are no elements with more than one
	 *         reference in the {@code Collection}.
	 */
	public static final boolean areElementsUnique(final Collection<?> collection) {
		assert collection != null;
		final Set<Object> uniqueElements = new HashSet<>(collection.size());
		return areElementsUnique(collection, uniqueElements);
	}

	/**
	 * Checks if all the elements in a given {@link Iterable} are unique,
	 * i.e.&nbsp;that no element occurs more than once.
	 *
	 * @param iterable
	 *            The {@code Iterable} to check.
	 * @return {@code} true iff there are no elements with more than one
	 *         reference in the {@code Iterable}.
	 */
	public static final boolean areElementsUnique(final Iterable<?> iterable) {
		assert iterable != null;
		final Set<Object> uniqueElements = new HashSet<>();
		return areElementsUnique(iterable, uniqueElements);
	}

	/**
	 * Gets the sum of all the elements of an arbitrary number of
	 * {@link Collection} objects.
	 *
	 * @param collections
	 *            The collections to sum the {@link Collection#size() size}
	 *            thereof.
	 * @return The sum of the size of all the given collections.
	 */
	public static final int countAllElements(final Collection<?>... collections) {
		int result = 0;
		for (final Collection<?> collection : collections) {
			result += collection.size();
		}
		return result;
	}

	public static final <E> Set<E> createAllElementSet(final Collection<? extends Collection<E>> collections) {
		final int resultSize = countAllElements(collections);
		final Iterable<? extends Collection<E>> upcast = collections;
		return createAllElementSet(upcast, resultSize);
	}

	public static final <E> Set<E> createAllElementSet(final Collection<? extends Collection<E>> collections,
			final int individualCollectionSize) {
		final int totalElementCount = individualCollectionSize * collections.size();
		final Iterable<? extends Collection<E>> upcast = collections;
		return createAllElementSet(upcast, totalElementCount);
	}

	public static final <E> Set<E> createAllElementSet(final Iterable<? extends Collection<E>> collections,
			final int totalElementCount) {
		final Set<E> result = new HashSet<>(totalElementCount);

		addAllElements(result, collections);

		return result;
	}

	/**
	 * Checks if all the elements in a given {@link Iterable} are unique,
	 * i.e.&nbsp;that no element occurs more than once.
	 *
	 * @param iterable
	 *            The {@code Iterable} to check.
	 * @param uniqueElements
	 *            The {@link Set} instance to use for counting unique elements.
	 * @return {@code} true iff there are no elements with more than one
	 *         reference in the {@code Iterable}.
	 */
	private static final <E> boolean areElementsUnique(final Iterable<? extends E> iterable,
			final Set<E> uniqueElements) {
		assert iterable != null;
		assert uniqueElements != null;
		boolean result = true;

		for (final E element : iterable) {
			if (!uniqueElements.add(element)) {
				result = false;
				break;
			}
		}

		return result;
	}

	private IterableElements() {
		// Avoid instantiation
	}

}
