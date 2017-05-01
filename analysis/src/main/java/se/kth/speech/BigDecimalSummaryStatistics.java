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

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.function.Consumer;

public final class BigDecimalSummaryStatistics implements Consumer<BigDecimal> {
	private static BigDecimal maxNullable(final BigDecimal first, final BigDecimal second) {
		final BigDecimal result;
		if (first == null) {
			result = second;
		} else {
			result = first.max(second);
		}
		return result;
	}

	private static BigDecimal minNullable(final BigDecimal first, final BigDecimal second) {
		final BigDecimal result;
		if (first == null) {
			result = second;
		} else {
			result = first.min(second);
		}
		return result;
	}

	private BigDecimal count = BigDecimal.ZERO;

	private BigDecimal max = null;

	private BigDecimal min = null;

	private BigDecimal sum = BigDecimal.ZERO;

	public BigDecimalSummaryStatistics() {
	}

	/**
	 * Records a new value into the summary information
	 *
	 * @param value
	 *            the input value
	 */
	@Override
	public void accept(final BigDecimal value) {
		count = count.add(BigDecimal.ONE);
		sum = sum.add(value);
		min = minNullable(min, value);
		max = maxNullable(max, value);
	}

	/**
	 * Combines the state of another {@code BigDecimalSummaryStatistics} into
	 * this one.
	 *
	 * @param other
	 *            another {@code BigDecimalSummaryStatistics}
	 * @throws NullPointerException
	 *             if {@code other} is null
	 */
	public void combine(final BigDecimalSummaryStatistics other) {
		count = count.add(other.count);
		sum = sum.add(other.sum);
		min = minNullable(min, other.min);
		max = maxNullable(max, other.max);
	}

	/**
	 * Returns the arithmetic mean of values recorded, or zero if no values have
	 * been recorded.
	 *
	 * @return the arithmetic mean of values, or zero if none
	 */
	public final BigDecimal getAverage() {
		final BigDecimal result;
		final BigDecimal count = getCount();
		final int cmp = count.compareTo(BigDecimal.ZERO);
		if (cmp > 0) {
			final BigDecimal sum = getSum();
			result = sum.divide(count, RoundingMode.HALF_UP);
		} else {
			result = BigDecimal.ZERO;
		}
		return result;
	}

	/**
	 * Returns the count of values recorded.
	 *
	 * @return the count of values
	 */
	public final BigDecimal getCount() {
		return count;
	}

	/**
	 * Returns the maximum value recorded, or {@code Integer.MIN_VALUE} if no
	 * values have been recorded.
	 *
	 * @return the maximum value, or {@code Integer.MIN_VALUE} if none
	 */
	public final BigDecimal getMax() {
		return max;
	}

	/**
	 * Returns the minimum value recorded, or {@code Integer.MAX_VALUE} if no
	 * values have been recorded.
	 *
	 * @return the minimum value, or {@code Integer.MAX_VALUE} if none
	 */
	public final BigDecimal getMin() {
		return min;
	}

	/**
	 * Returns the sum of values recorded, or zero if no values have been
	 * recorded.
	 *
	 * @return the sum of values, or zero if none
	 */
	public final BigDecimal getSum() {
		return sum;
	}

	@Override
	/**
	 * {@inheritDoc}
	 *
	 * Returns a non-empty string representation of this object suitable for
	 * debugging. The exact presentation format is unspecified and may vary
	 * between implementations and versions.
	 */
	public String toString() {
		return String.format("%s{count=%d, sum=%d, min=%d, average=%f, max=%d}", this.getClass().getSimpleName(),
				getCount(), getSum(), getMin(), getAverage(), getMax());
	}
}
