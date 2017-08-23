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
import java.math.MathContext;
import java.time.Duration;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 23 Aug 2017
 *
 */
public final class Durations {

	private static final BigDecimal NANOS_TO_SECS_DIVISOR = new BigDecimal("1000000000");

	public static BigDecimal toDecimalSeconds(final Duration duration) {
		final BigDecimal nanos = new BigDecimal(duration.toNanos());
		return nanos.divide(NANOS_TO_SECS_DIVISOR, MathContext.UNLIMITED);
	}

	private Durations() {
	}

}
