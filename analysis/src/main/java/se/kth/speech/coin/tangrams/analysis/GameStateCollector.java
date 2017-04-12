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

import java.sql.Timestamp;
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
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;
import se.kth.speech.coin.tangrams.iristk.events.GameStateDescription;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 27 Jan 2017
 *
 */
public final class GameStateCollector
		implements Collector<Event, Map<String, GameStateChangeData>, Map<String, GameStateChangeData>> {

	private static final BiConsumer<Map<String, GameStateChangeData>, Event> ACCUMULATOR = new BiConsumer<Map<String, GameStateChangeData>, Event>() {

		private final Set<GameManagementEvent> ignoredEventTypes = EnumSet.of(GameManagementEvent.PLAYER_JOIN_REQUEST,
				GameManagementEvent.PLAYER_JOIN_RESPONSE);

		/*
		 * (non-Javadoc)
		 *
		 * @see java.util.function.BiConsumer#accept(java.lang.Object,
		 * java.lang.Object)
		 */
		@Override
		public void accept(final Map<String, GameStateChangeData> gameStateChangeData, final Event event) {
			final String eventName = event.getName();
			final GameManagementEvent gameEventType = GameManagementEvent.getEventType(eventName);
			if (gameEventType == null) {
				// The broker event is not a relevant tangrams game event;
				// Ignore it
				LOGGER.debug("Ignoring broker event named \"{}\".", eventName);
			} else {
				final String time = event.getTime();
				LOGGER.debug("Found {} sent at \"{}\".", new Object[] { event.getClass().getSimpleName(), time });
				final String gameId = event.getString(GameManagementEvent.Attribute.GAME_ID.toString());
				final Timestamp timestamp = Timestamp.valueOf(time);
				accept(gameStateChangeData, gameId, timestamp, gameEventType, event);
			}
		}

		private void accept(final Map<String, GameStateChangeData> gameStateChangeData, final String gameId,
				final Timestamp time, final GameManagementEvent gameEventType, final Event event) {
			if (ignoredEventTypes.contains(gameEventType)) {
				LOGGER.debug("Ignored event of type {} sent at \"{}\".", new Object[] { gameEventType, time });
			} else {
				switch (gameEventType) {
				case GAME_READY_RESPONSE: {
					final GameStateDescription gameDesc = (GameStateDescription) event
							.get(GameManagementEvent.Attribute.GAME_STATE.toString());
					LOGGER.info("Found {} sent at \"{}\".", new Object[] { gameDesc.getClass().getSimpleName(), time });
					putInitialState(gameStateChangeData, gameId, time, gameDesc);
					break;
				}
				default: {
					addEvent(gameStateChangeData, gameId, time, event);
					break;
				}
				}
			}
		}

		/**
		 * @param gameId
		 * @param timestamp
		 * @param event
		 */
		private void addEvent(final Map<String, GameStateChangeData> gameStateChangeData, final String gameId,
				final Timestamp time, final Event event) {
			gameStateChangeData.compute(gameId, (gKey, oldVal) -> {
				if (oldVal == null) {
					throw new IllegalArgumentException(String.format(
							"Non-game state event found before the initial game state was parsed for game \"%s\": %s",
							gameId, event));
				} else {
					final List<Event> timeEvents = oldVal.getEvents().computeIfAbsent(time,
							tKey -> new ArrayList<>(EXPECTED_EVENTS_FOR_TIMESTAMP));
					timeEvents.add(event);
				}
				return oldVal;
			});
		}

		/**
		 * @param gameId
		 * @param startTime
		 * @param gameDesc
		 */
		private void putInitialState(final Map<String, GameStateChangeData> gameStateChangeData, final String gameId,
				final Timestamp startTime, final GameStateDescription gameDesc) {
			gameStateChangeData.compute(gameId, (key, oldVal) -> {
				final GameStateChangeData result;
				if (oldVal == null) {
					result = new GameStateChangeData(gameDesc, startTime);
				} else {
					throw new IllegalArgumentException(
							String.format("More than one initial-state event found for game \"%s\".", gameId));
				}
				return result;
			});
		}
	};

	private static final Set<Characteristics> CHARACTERISTICS = EnumSet.of(Characteristics.UNORDERED,
			Characteristics.IDENTITY_FINISH);

	private static final BinaryOperator<Map<String, GameStateChangeData>> COMBINER = new BinaryOperator<Map<String, GameStateChangeData>>() {

		@Override
		public Map<String, GameStateChangeData> apply(final Map<String, GameStateChangeData> target,
				final Map<String, GameStateChangeData> source) {
			for (final Entry<String, GameStateChangeData> sourceGameLoggedEventMap : source.entrySet()) {
				final String gameId = sourceGameLoggedEventMap.getKey();
				final GameStateChangeData sourceGameData = sourceGameLoggedEventMap.getValue();
				target.compute(gameId, (gKey, oldVal) -> {
					GameStateChangeData result;
					if (oldVal == null) {
						// There is nothing to merge with; Just re-use the
						// source object
						result = sourceGameData;
					} else {
						// Sanity check
						if (Objects.equals(sourceGameData.getStartTime(), oldVal.getStartTime())) {
							if (Objects.equals(sourceGameData.getInitialState(), oldVal.getInitialState())) {
								final Map<Timestamp, List<Event>> targetLoggedEventMap = oldVal.getEvents();
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

	private static final Logger LOGGER = LoggerFactory.getLogger(GameStateCollector.class);

	private final Supplier<Map<String, GameStateChangeData>> supplier;

	public GameStateCollector(final Supplier<Map<String, GameStateChangeData>> supplier) {
		this.supplier = supplier;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.stream.Collector#accumulator()
	 */
	@Override
	public BiConsumer<Map<String, GameStateChangeData>, Event> accumulator() {
		return ACCUMULATOR;
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
	public BinaryOperator<Map<String, GameStateChangeData>> combiner() {
		return COMBINER;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.stream.Collector#finisher()
	 */
	@Override
	public Function<Map<String, GameStateChangeData>, Map<String, GameStateChangeData>> finisher() {
		return Function.identity();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.stream.Collector#supplier()
	 */
	@Override
	public Supplier<Map<String, GameStateChangeData>> supplier() {
		return supplier;
	}

}
