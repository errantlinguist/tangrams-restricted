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
package se.kth.speech.coin.tangrams.iristk;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 4 May 2017
 *
 */
public final class EventTimes {

	public static final DateTimeFormatter FORMATTER = createFormatterBuilder().toFormatter();

	public static DateTimeFormatterBuilder createFormatterBuilder() {
		return new DateTimeFormatterBuilder().parseCaseInsensitive().append(DateTimeFormatter.ISO_LOCAL_DATE)
				.appendLiteral(' ').append(DateTimeFormatter.ISO_LOCAL_TIME);
	}

	public static LocalDateTime parseEventTime(final String timeStr) {
		return LocalDateTime.parse(timeStr, FORMATTER);
	}

	private EventTimes() {
	}

}
