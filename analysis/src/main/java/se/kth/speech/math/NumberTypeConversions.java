/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.analysis.
 *
 *  se.kth.speech.coin.tangrams-restricted.analysis is free software: you can redistribute it and/or modify
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
package se.kth.speech.math;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 15 Nov 2017
 *
 */
public final class NumberTypeConversions {

	/**
	 * Uses a given {@link Number} instance's {@link Number#doubleValue()}
	 * method to represent the given value as a {@code double}-precision
	 * floating-point number, checking after conversion that the result value is
	 * not {@link Double#isInfinite(double) infinite}. This value is the result
	 * of some implementations of {@link Number#doubleValue()} where the
	 * original {@link Number} instance represents a value too large to be
	 * represented by a {@code double} &mdash; for example by
	 * {@link java.math.BigDecimal#doubleValue()}.
	 *
	 * @param value
	 *            The {@code Number} instance to convert.
	 * @return A {@code double} value representing the same number represented
	 *         by the given {@code Number}, which is guaranteed to not be
	 *         infinite.
	 * @throws IllegalArgumentException
	 *             Iff the resulting {@code double} is infinite. This occurs
	 *             even if the given {@code Number} instance actually does
	 *             represent an infinite number.
	 */
	public static double nonInfiniteDoubleValue(final Number value) {
		final double result = value.doubleValue();
		if (Double.isInfinite(result)) {
			throw new IllegalArgumentException("BigDecimal was converted to floating-point infinity: " + value);
		} else {
			return result;
		}
	}

	private NumberTypeConversions() {
	}

}
