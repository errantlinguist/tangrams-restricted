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

import java.sql.Timestamp;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 11 Apr 2017
 *
 */
public final class TimestampArithmetic {

	public static Timestamp createOffsetTimestamp(final Timestamp augend, final float offsetMilliseconds) {
		final long wholePart = Math.round(Math.floor(offsetMilliseconds));
		final long summedMills = augend.getTime() + wholePart;
		final Timestamp result = new Timestamp(summedMills);
		final float fracPart = offsetMilliseconds - wholePart;
		final int nanosToAdd = Math.round(fracPart * 1000000);
		final int summedNanos = augend.getNanos() + nanosToAdd;
		result.setNanos(summedNanos);
		return result;
	}

	private TimestampArithmetic() {

	}

}
