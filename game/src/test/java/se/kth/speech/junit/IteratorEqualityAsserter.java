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
package se.kth.speech.junit;

import java.util.Iterator;
import java.util.function.BiConsumer;

import org.junit.Assert;

/**
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since 22 Feb 2017
 *
 */
public final class IteratorEqualityAsserter<T> implements BiConsumer<Iterator<? extends T>, Iterator<? extends T>> {

	private final BiConsumer<? super T, ? super T> assertion;

	public IteratorEqualityAsserter() {
		this(Assert::assertEquals);
	}

	public IteratorEqualityAsserter(final BiConsumer<? super T, ? super T> assertion) {
		this.assertion = assertion;
	}

	/**
	 * @see <a href=
	 *      "http://stackoverflow.com/a/34818800/1391325">StackOverflow</a>
	 * @param iter1
	 *            The first {@link Iterator} to compare.
	 * @param iter2
	 *            The {@code Iterator} to compare against.
	 */
	@Override
	public void accept(final Iterator<? extends T> iter1, final Iterator<? extends T> iter2) {
		if (!iter1.equals(iter2)) {
			while (iter1.hasNext()) {
				if (iter2.hasNext()) {
					assertion.accept(iter1.next(), iter2.next());
				} else {
					Assert.fail("Second iterator is empty but not the first.");
				}
			}
			if (iter2.hasNext()) {
				Assert.fail("First iterator is empty but not the second.");
			}
		}
	}

}
