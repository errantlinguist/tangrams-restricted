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
package se.kth.speech.coin.tangrams.iristk.io;

import java.util.Arrays;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import com.google.common.collect.Maps;

import iristk.system.Event;
import se.kth.speech.coin.tangrams.analysis.GameHistory;
import se.kth.speech.coin.tangrams.analysis.GameHistoryeCollector;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 6 Feb 2017
 *
 */
final class LoggedGameStateChangeDataParser implements Function<Stream<String>, Map<String, GameHistory>> {

	private static final Pattern EMPTY_OR_WHITESPACE_PATTERN = Pattern.compile("\\s*");

	@Override
	public Map<String, GameHistory> apply(final Stream<String> lines) {
		final Stream<Event> loggedEvents = lines.filter(line -> !EMPTY_OR_WHITESPACE_PATTERN.matcher(line).matches())
				.flatMap(LoggedEvents.JSON_EVENT_PARSER);
		final Event[] loggedEventArray = loggedEvents.toArray(Event[]::new);
		final Supplier<Map<String, GameHistory>> mapFactory = () -> Maps
				.newHashMapWithExpectedSize(loggedEventArray.length);
		return Arrays.stream(loggedEventArray).collect(new GameHistoryeCollector(mapFactory));
	}

}
