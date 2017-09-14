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
package se.kth.speech;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.text.DecimalFormat;
import java.time.Duration;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.time.temporal.Temporal;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 11 Apr 2017
 *
 */
public final class TimestampArithmetic {

	/**
	 * Returns A {@link DecimalFormat} instance used for representing second
	 * amounts,e.g.&nbsp;<code>03.255</code>. <strong>NOTE:</strong>
	 * {@code DecimalFormat} is not guaranteed to be thread-safe; Always create
	 * a new instance thereof before using!
	 */
	private static final ThreadLocal<DecimalFormat> DURATION_SECONDS_FORMAT = new ThreadLocal<DecimalFormat>() {

		/*
		 * (non-Javadoc)
		 *
		 * @see java.lang.ThreadLocal#initialValue()
		 */
		@Override
		protected DecimalFormat initialValue() {
			// NOTE: DecimalFormat is not guaranteed to be thread-safe; Always
			// create a new instance thereof before using!
			final DecimalFormat result = new DecimalFormat();
			result.setMinimumIntegerDigits(2);
			result.setMinimumFractionDigits(SECS_PRECISION);
			result.setMaximumFractionDigits(SECS_PRECISION);
			return result;
		}

	};

	private static final BigDecimal NANOS_PER_SEC_BIGDECIMAL;

	private static final long NANOS_PER_SEC_LONG;

	private static final BigInteger SECS_PER_HOUR = new BigInteger("3600");

	private static final BigInteger SECS_PER_MIN = new BigInteger("60");

	private static final int SECS_PRECISION = 3;

	static {
		NANOS_PER_SEC_BIGDECIMAL = new BigDecimal("1000000000");
		NANOS_PER_SEC_LONG = NANOS_PER_SEC_BIGDECIMAL.longValueExact();
	}

	public static BigDecimal calculateDecimalSecondDifference(final Temporal firstTime, final Temporal nextTime,
			final MathContext mathCtx) {
		final long diffNanos = ChronoUnit.NANOS.between(firstTime, nextTime);
		return new BigDecimal(diffNanos).divide(NANOS_PER_SEC_BIGDECIMAL, mathCtx);
	}

	public static LocalDateTime createOffsetTimestamp(final LocalDateTime augend, final double offsetSecs) {
		final long wholePart = Math.round(Math.floor(offsetSecs));
		final double fractionPart = offsetSecs - wholePart;
		final long nanos = Math.round(fractionPart * NANOS_PER_SEC_LONG);
		final Duration duration = Duration.ofSeconds(wholePart, nanos);
		return augend.plus(duration);
	}

	/**
	 *
	 * @param duration
	 *            The {@link Duration} to format.
	 * @return A string representation of the given {@code Duration}.
	 * @see <a href="https://stackoverflow.com/a/266846/1391325">Original
	 *      StackOverflow answer</a>
	 */
	public static String formatDurationHours(final Duration duration) {
		final BigDecimal decimalSeconds = toDecimalSeconds(duration);
		final BigDecimal absDecimalSeconds = decimalSeconds.abs();
		final BigInteger absSecondsWholePart = absDecimalSeconds.toBigInteger();

		final BigInteger[] hoursAndRemainingSecs = absSecondsWholePart.divideAndRemainder(SECS_PER_HOUR);
		final String decimalSecondsRepr = DURATION_SECONDS_FORMAT.get().format(absDecimalSeconds);
		final String positive = String.format("%s:%s:%s", hoursAndRemainingSecs[0],
				hoursAndRemainingSecs[1].divide(SECS_PER_MIN), decimalSecondsRepr);
		return decimalSeconds.compareTo(BigDecimal.ZERO) < 0 ? "-" + positive : positive;
	}
	
	/**
	 *
	 * @param duration
	 *            The {@link Duration} to format.
	 * @return A string representation of the given {@code Duration}.
	 * @see <a href="https://stackoverflow.com/a/266846/1391325">Original
	 *      StackOverflow answer</a>
	 */
	public static String formatDurationMinutes(final Duration duration) {
		final BigDecimal decimalSeconds = toDecimalSeconds(duration);
		final BigDecimal absDecimalSeconds = decimalSeconds.abs();
		final BigInteger absSecondsWholePart = absDecimalSeconds.toBigInteger();

		final BigInteger[] hoursAndRemainingSecs = absSecondsWholePart.divideAndRemainder(SECS_PER_HOUR);
		final String decimalSecondsRepr = DURATION_SECONDS_FORMAT.get().format(absDecimalSeconds);
		final String positive = String.format("%s:%s:%s", hoursAndRemainingSecs[0],
				hoursAndRemainingSecs[1].divide(SECS_PER_MIN), decimalSecondsRepr);
		return decimalSeconds.compareTo(BigDecimal.ZERO) < 0 ? "-" + positive : positive;
	}

	public static BigDecimal toDecimalSeconds(final Duration duration) {
		final BigDecimal nanos = new BigDecimal(duration.toNanos());
		return nanos.divide(NANOS_PER_SEC_BIGDECIMAL, MathContext.UNLIMITED);
	}

	private TimestampArithmetic() {

	}

}
