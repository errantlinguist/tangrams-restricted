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

import java.util.TreeSet;
import java.util.function.Supplier;

/**
 * A factory for creating {@link TreeSet} instances which hold {@link Integer}
 * objects as values.
 *
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since 2013-11-03
 *
 */
final class IntegerTreeSetFactory implements Supplier<TreeSet<Integer>> {

	/**
	 * {@link SingletonHolder} is loaded on the first execution of
	 * {@link IntegerTreeSetFactory#getInstance()} or the first access to
	 * {@link SingletonHolder#INSTANCE}, not before.
	 *
	 * @author <a href="http://www.cs.umd.edu/~pugh/">Bill Pugh</a>
	 * @see <a href=
	 *      "https://en.wikipedia.org/wiki/Singleton_pattern#The_solution_of_Bill_Pugh">https://en.wikipedia.org/wiki/Singleton_pattern#The_solution_of_Bill_Pugh</a>
	 */
	private static final class SingletonHolder {
		/**
		 * A singleton instance of {@link IntegerTreeSetFactory}.
		 */
		private static final IntegerTreeSetFactory INSTANCE = new IntegerTreeSetFactory();
	}

	/**
	 * Gets a singleton instance of {@link IntegerTreeSetFactory}.
	 *
	 * @return The singleton instance.
	 */
	static IntegerTreeSetFactory getInstance() {
		return SingletonHolder.INSTANCE;
	}

	private IntegerTreeSetFactory() {
		// Avoid instantiation
	}

	@Override
	public TreeSet<Integer> get() {
		return new TreeSet<>();
	}

}
