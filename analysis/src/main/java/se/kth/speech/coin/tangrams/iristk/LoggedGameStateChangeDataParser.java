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
package se.kth.speech.coin.tangrams.iristk;

import java.util.Arrays;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Stream;

import com.google.common.collect.Maps;

import iristk.system.Event;
import se.kth.speech.coin.tangrams.iristk.io.LoggedEvents;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 6 Feb 2017
 *
 */
public final class LoggedGameStateChangeDataParser
		implements Function<Stream<String>, Map<String, GameStateChangeData>> {

	@Override
	public Map<String, GameStateChangeData> apply(final Stream<String> lines) {
		final Stream<Event> loggedEvents = lines.flatMap(LoggedEvents.JSON_EVENT_PARSER);
		final Event[] loggedEventArray = loggedEvents.toArray(Event[]::new);
		final Supplier<Map<String, GameStateChangeData>> mapFactory = () -> Maps
				.newHashMapWithExpectedSize(loggedEventArray.length);
		return Arrays.stream(loggedEventArray).collect(new GameStateCollector(mapFactory));
	}

}
