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

import java.time.Duration;
import java.time.LocalDateTime;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 11 Apr 2017
 *
 */
public final class TimestampArithmetic {

	public static LocalDateTime createOffsetTimestamp(final LocalDateTime augend, final double offsetSecs) {
		final long wholePart = Math.round(Math.floor(offsetSecs));
		final double fractionPart = offsetSecs - wholePart;
		final long nanos = Math.round(fractionPart * 1000000000);
		final Duration duration = Duration.ofSeconds(wholePart, nanos);
		return augend.plus(duration);
	}

	private TimestampArithmetic() {

	}

}
