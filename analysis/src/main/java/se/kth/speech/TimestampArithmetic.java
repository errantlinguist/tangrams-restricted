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

import java.math.BigDecimal;
import java.math.MathContext;
import java.time.Duration;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.time.temporal.Temporal;

/**
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since 11 Apr 2017
 *
 */
public final class TimestampArithmetic {

	private static final BigDecimal NANOS_PER_SEC_BIGDECIMAL;

	private static final long NANOS_PER_SEC_LONG;

	private static final BigDecimal SECS_PER_HOUR = new BigDecimal("3600");

	private static final BigDecimal SECS_PER_MIN = new BigDecimal("60");

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
		final BigDecimal[] absHoursAndRemainingSecs = absDecimalSeconds.divideAndRemainder(SECS_PER_HOUR);
		final BigDecimal[] absMinutesAndRemainingSecs = absHoursAndRemainingSecs[1].divideAndRemainder(SECS_PER_MIN);
		final String positive = String.format("%02d:%02d:%02.03f", absHoursAndRemainingSecs[0].toBigIntegerExact(),
				absMinutesAndRemainingSecs[0].toBigIntegerExact(), absMinutesAndRemainingSecs[1]);
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
		final BigDecimal[] absMinutesAndRemainingSecs = absDecimalSeconds.divideAndRemainder(SECS_PER_MIN);
		final String positive = String.format("%02d:%02.03f", absMinutesAndRemainingSecs[0].toBigIntegerExact(),
				absMinutesAndRemainingSecs[1]);
		return decimalSeconds.compareTo(BigDecimal.ZERO) < 0 ? "-" + positive : positive;
	}

	public static BigDecimal toDecimalSeconds(final Duration duration) {
		final BigDecimal nanos = new BigDecimal(duration.toNanos());
		return nanos.divide(NANOS_PER_SEC_BIGDECIMAL, MathContext.UNLIMITED);
	}

	private TimestampArithmetic() {

	}

}
