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

import java.util.Comparator;

import it.unimi.dsi.fastutil.objects.Object2IntMap;
import it.unimi.dsi.fastutil.objects.Object2IntOpenHashMap;

/**
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since Oct 7, 2017
 *
 */
public final class ObservationOrderComparator<T> implements Comparator<T> {

	private static <T> Object2IntMap<T> createDefaultObservationOrderMap(final int expectedObservationCount) {
		final Object2IntMap<T> result = new Object2IntOpenHashMap<>(expectedObservationCount);
		result.defaultReturnValue(-1);
		return result;
	}

	private final int defaultReturnValue;

	private final Object2IntMap<? super T> observationOrders;

	public ObservationOrderComparator(final int expectedObservationCount) {
		this(createDefaultObservationOrderMap(expectedObservationCount));
	}

	public ObservationOrderComparator(final Object2IntMap<? super T> observationOrders) { // NO_UCD (use private)
		this.defaultReturnValue = observationOrders.defaultReturnValue();
		if (this.defaultReturnValue >= 0) {
			throw new IllegalArgumentException(String
					.format("The backing map's default return value is %d but must be negative.", defaultReturnValue));
		}
		this.observationOrders = observationOrders;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
	 */
	@Override
	public int compare(final T o1, final T o2) {
		final int order1 = fetchOrder(o1);
		final int order2 = fetchOrder(o2);
		return Integer.compare(order1, order2);
	}

	private int fetchOrder(final T obj) {
		int result = observationOrders.getInt(obj);
		if (result == defaultReturnValue) {
			result = observationOrders.size();
			observationOrders.put(obj, result);
		}
		return result;
	}

}
