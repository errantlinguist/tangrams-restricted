/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.analysis.
 *
 *  tangrams is free software: you can redistribute it and/or modify
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
package se.kth.speech.coin.tangrams.analysis;

import java.time.LocalDateTime;
import java.util.Collection;
import java.util.List;
import java.util.NavigableMap;
import java.util.function.Function;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import iristk.system.Event;
import se.kth.speech.TimestampArithmetic;
import se.kth.speech.coin.tangrams.iristk.EventTimes;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Apr 17, 2017
 *
 */
public final class UtteranceGameContextFactory {

	private static final Logger LOGGER = LoggerFactory.getLogger(UtteranceGameContextFactory.class);

	// private static final Collector<CharSequence, ?, String> TOKEN_FORM_JOINER
	// = Collectors.joining(" ");

	private final Function<? super String, GameHistory> playerGameHistoryFactory;

	public UtteranceGameContextFactory(final Function<? super String, GameHistory> playerGameHistoryFactory) {
		this.playerGameHistoryFactory = playerGameHistoryFactory;
	}

	public Stream<GameContext> apply(final float startTime, final float endTime, final String playerId) {
		assert startTime <= endTime;
		LOGGER.debug("Getting history for player \"{}\".", playerId);
		final GameHistory history = playerGameHistoryFactory.apply(playerId);
		final NavigableMap<LocalDateTime, List<Event>> events = history.getEvents();
		final LocalDateTime gameStartTime = history.getStartTime();
		final LocalDateTime uttStartTimestamp = TimestampArithmetic.createOffsetTimestamp(gameStartTime, startTime);

		final LocalDateTime uttEndTimestamp = TimestampArithmetic.createOffsetTimestamp(gameStartTime, endTime);
		assert uttStartTimestamp.isBefore(uttEndTimestamp) || uttStartTimestamp.isEqual(uttEndTimestamp);
		final NavigableMap<LocalDateTime, List<Event>> eventsDuringUtt = events.subMap(uttStartTimestamp, true,
				uttEndTimestamp, true);

		final Stream.Builder<GameContext> resultBuilder = Stream.builder();
		if (eventsDuringUtt.isEmpty()) {
			resultBuilder.accept(new GameContext(history, uttStartTimestamp, playerId));
		} else {
			// Create one data point for each event found during the utterance
			// TODO: estimate partitions for utterance: By phones?
			final Collection<List<Event>> timedEvents = eventsDuringUtt.values();
			// if (LOGGER.isDebugEnabled()) {
			// final List<Event> allEventsDuringUtt =
			// timedEvents.stream().flatMap(Collection::stream)
			// .collect(Collectors.toList());
			// final String delim = System.lineSeparator() + '\t';
			// final String uttRepr =
			// allEventsDuringUtt.stream().map(Event::toString)
			// .collect(Collectors.joining(delim));
			// LOGGER.debug("Found {} event(s) during utterance \"{}\"
			// subsequence: \"{}\"" + delim + "{}",
			// new Object[] { allEventsDuringUtt.size(), utt.getSegmentId(),
			// utt.getTokens().stream().collect(TOKEN_FORM_JOINER), uttRepr });
			// }
			final Stream<Event> allEventsDuringUtt = timedEvents.stream().flatMap(Collection::stream);
			final Stream<LocalDateTime> allTimestampsDuringUtt = allEventsDuringUtt.map(Event::getTime)
					.map(EventTimes::parseEventTime);
			allTimestampsDuringUtt.map(timestampDuringUtt -> new GameContext(history, timestampDuringUtt, playerId))
					.forEachOrdered(resultBuilder);
		}
		return resultBuilder.build();
	}

}
