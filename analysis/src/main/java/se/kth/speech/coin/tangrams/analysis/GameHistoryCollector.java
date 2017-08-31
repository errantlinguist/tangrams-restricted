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
package se.kth.speech.coin.tangrams.analysis;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collector;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import iristk.system.Event;
import se.kth.speech.coin.tangrams.iristk.EventTimes;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;
import se.kth.speech.coin.tangrams.iristk.events.EventSystems;
import se.kth.speech.coin.tangrams.iristk.events.GameStateDescription;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 27 Jan 2017
 *
 */
public final class GameHistoryCollector
		implements Collector<Event, Map<String, GameHistory>, Map<String, GameHistory>> {

	private class Accumulator implements BiConsumer<Map<String, GameHistory>, Event> {

		private static final String GAME_END_EVENT_NAME = "monitor.system.disconnected";

		private final Set<GameManagementEvent> ignoredEventTypes = EnumSet.of(GameManagementEvent.PLAYER_JOIN_REQUEST,
				GameManagementEvent.PLAYER_JOIN_RESPONSE);

		/*
		 * (non-Javadoc)
		 *
		 * @see java.util.function.BiConsumer#accept(java.lang.Object,
		 * java.lang.Object)
		 */
		@Override
		public void accept(final Map<String, GameHistory> gameHistories, final Event event) {
			final String eventName = event.getName();
			final GameManagementEvent gameEventType = GameManagementEvent.getEventType(eventName);
			if (gameEventType == null) {
				if (GAME_END_EVENT_NAME.equals(eventName)) {
					// Ensure that the disconnection event is for the
					// tangrams game
					final String eventSystem = event.getString("system");
					if (EventSystems.NAME.equals(eventSystem)) {
						final String time = event.getTime();
						LOGGER.debug("Found end-game event sent at {}.", time);
						// TODO: Add a "gameEnd" attribute to GameHistory class
						// final LocalDateTime timestamp =
						// EventTimes.parseEventTime(time);
						// clientDisconnectionTimes.put(timestamp,
						// Boolean.TRUE);
					} else {
						// The broker event is not a relevant tangrams game
						// event;
						// Ignore it
						LOGGER.debug("Ignoring broker event named \"{}\" for system \"{}\".", eventName, eventSystem);
					}
				} else {
					// The broker event is not a relevant tangrams game event;
					// Ignore it
					LOGGER.debug("Ignoring broker event named \"{}\".", eventName);
				}
			} else {
				final String time = event.getTime();
				LOGGER.debug("Found {} sent at \"{}\".", event.getClass().getSimpleName(), time);
				final String gameId = event.getString(GameManagementEvent.Attribute.GAME_ID.toString());
				final LocalDateTime timestamp = timeParser.apply(time);
				accept(gameHistories, gameId, timestamp, gameEventType, event);
			}
		}

		private void accept(final Map<String, GameHistory> gameHistories, final String gameId, final LocalDateTime time,
				final GameManagementEvent gameEventType, final Event event) {
			if (ignoredEventTypes.contains(gameEventType)) {
				LOGGER.debug("Ignored event of type {} sent at \"{}\".", gameEventType, time);
			} else {
				switch (gameEventType) {
				case GAME_READY_RESPONSE: {
					final GameStateDescription gameDesc = (GameStateDescription) event
							.get(GameManagementEvent.Attribute.GAME_STATE.toString());
					LOGGER.debug("Found {} sent at \"{}\".", gameDesc.getClass().getSimpleName(), time);
					putInitialState(gameHistories, gameId, time, gameDesc);
					break;
				}
				default: {
					addEvent(gameHistories, gameId, time, event);
					break;
				}
				}
			}
		}

		private void addEvent(final Map<String, GameHistory> gameHistories, final String gameId,
				final LocalDateTime time, final Event event) {
			gameHistories.compute(gameId, (gKey, oldVal) -> {
				if (oldVal == null) {
					throw new IllegalArgumentException(String.format(
							"Non-game state event found before the initial game state was parsed for game \"%s\": %s",
							gameId, event));
				} else {
					final List<Event> timeEvents = oldVal.getEventsMutable().computeIfAbsent(time,
							tKey -> new ArrayList<>(EXPECTED_EVENTS_FOR_TIMESTAMP));
					timeEvents.add(event);
				}
				return oldVal;
			});
		}

		private void putInitialState(final Map<String, GameHistory> gameHistories, final String gameId,
				final LocalDateTime startTime, final GameStateDescription gameDesc) {
			gameHistories.compute(gameId, (key, oldVal) -> {
				final GameHistory result;
				if (oldVal == null) {
					result = new GameHistory(gameDesc, startTime);
				} else {
					throw new IllegalArgumentException(
							String.format("More than one initial-state event found for game \"%s\".", gameId));
				}
				return result;
			});
		}
	}

	private static final Set<Characteristics> CHARACTERISTICS = EnumSet.of(Characteristics.UNORDERED,
			Characteristics.IDENTITY_FINISH);

	private static final BinaryOperator<Map<String, GameHistory>> COMBINER = new BinaryOperator<Map<String, GameHistory>>() {

		@Override
		public Map<String, GameHistory> apply(final Map<String, GameHistory> target,
				final Map<String, GameHistory> source) {
			for (final Entry<String, GameHistory> sourceGameLoggedEventMap : source.entrySet()) {
				final String gameId = sourceGameLoggedEventMap.getKey();
				final GameHistory sourceGameData = sourceGameLoggedEventMap.getValue();
				target.compute(gameId, (gKey, oldVal) -> {
					GameHistory result;
					if (oldVal == null) {
						// There is nothing to merge with; Just re-use the
						// source object
						result = sourceGameData;
					} else {
						// Sanity check
						if (Objects.equals(sourceGameData.getStartTime(), oldVal.getStartTime())) {
							if (Objects.equals(sourceGameData.getInitialState(), oldVal.getInitialState())) {
								final Map<LocalDateTime, List<Event>> targetLoggedEventMap = oldVal.getEventsMutable();
								sourceGameData.getEvents().forEach((time, sourceLoggedEventList) -> {
									final List<Event> targetEventList = targetLoggedEventMap.computeIfAbsent(time,
											tKey -> new ArrayList<>(EXPECTED_EVENTS_FOR_TIMESTAMP));
									targetEventList.addAll(sourceLoggedEventList);
								});
							} else {
								throw new IllegalArgumentException(String.format(
										"The two results to merge have differing initial states for game \"%s\".",
										gKey));
							}
						} else {
							throw new IllegalArgumentException(String.format(
									"The two results to merge have differing start times for game \"%s\".", gKey));
						}
						result = oldVal;
					}

					return result;
				});
			}
			return target;
		}

	};

	/**
	 * It should be exceedingly rare to have logged two events logged at exactly
	 * the same time
	 */
	private static final int EXPECTED_EVENTS_FOR_TIMESTAMP = 1;

	private static final Logger LOGGER = LoggerFactory.getLogger(GameHistoryCollector.class);

	private final Accumulator accumulator;

	private final Supplier<Map<String, GameHistory>> supplier;

	private final Function<? super String, LocalDateTime> timeParser;

	public GameHistoryCollector(final Supplier<Map<String, GameHistory>> supplier) {
		this(supplier, EventTimes::parseEventTime);
	}

	public GameHistoryCollector(final Supplier<Map<String, GameHistory>> supplier,
			final Function<? super String, LocalDateTime> timeParser) {
		this.supplier = supplier;
		this.timeParser = timeParser;
		accumulator = new Accumulator();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.stream.Collector#accumulator()
	 */
	@Override
	public BiConsumer<Map<String, GameHistory>, Event> accumulator() {
		return accumulator;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.stream.Collector#characteristics()
	 */
	@Override
	public Set<Characteristics> characteristics() {
		return CHARACTERISTICS;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.stream.Collector#combiner()
	 */
	@Override
	public BinaryOperator<Map<String, GameHistory>> combiner() {
		return COMBINER;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.stream.Collector#finisher()
	 */
	@Override
	public Function<Map<String, GameHistory>, Map<String, GameHistory>> finisher() {
		return Function.identity();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.stream.Collector#supplier()
	 */
	@Override
	public Supplier<Map<String, GameHistory>> supplier() {
		return supplier;
	}

}
