/*
 *  This file is part of se.kth.speech.coin.tangrams.playback.
 *
 *  se.kth.speech.coin.tangrams.playback is free software: you can redistribute it and/or modify
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
import java.time.Duration;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 9 Feb 2017
 *
 */
public final class Durations {

	/**
	 * @see <a href="http://stackoverflow.com/a/266970/1391325">StackOverflow</a>
	 * @param duration
	 * @return
	 */
	public static String formatDuration(final Duration duration) {
		return formatDuration(duration.getSeconds(), duration.getNano());
	}

	/**
	 * @see <a href="http://stackoverflow.com/a/266970/1391325">StackOverflow</a>
	 * @param seconds
	 * @param nanoseconds
	 * @return
	 */
	public static String formatDuration(final long seconds, final int nanoseconds) {
		// final long hours = seconds / 3600;
		final long mins = seconds % 3600 / 60;
		final long secs = seconds % 60;
		return String.format("%d:%02d.%03d", mins, secs, nanoseconds);
	}

	/**
	 * @see <a href="http://stackoverflow.com/a/24656244/1391325">StackOverflow</a>
	 * @param value
	 * @return
	 */
	public static Duration parseDuration(final String value) {
		final String[] fields = value.split(":");
		final BigDecimal secs = new BigDecimal(fields[1]);
		final String durationStr = String.format("PT%sM%sS", fields[0], secs);
		return Duration.parse(durationStr);
	}

	private Durations() {

	}

}
