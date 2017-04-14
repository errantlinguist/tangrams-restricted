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
package se.kth.speech.coin.tangrams.analysis;

import java.sql.Timestamp;
import java.util.Arrays;
import java.util.EnumMap;
import java.util.EnumSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.DoubleStream;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Lists;

import iristk.system.Event;
import it.unimi.dsi.fastutil.objects.Object2DoubleMap;
import it.unimi.dsi.fastutil.objects.Object2DoubleOpenHashMap;
import se.kth.speech.coin.tangrams.iristk.EventSubmittingPlayerMatcher;
import se.kth.speech.coin.tangrams.iristk.EventTypeMatcher;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;
import se.kth.speech.coin.tangrams.iristk.events.GameStateDescription;
import se.kth.speech.coin.tangrams.iristk.events.Move;

final class GameEventFeatureExtractor implements FeatureExtractor {

	/**
	 *
	 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
	 * @since 13 Apr 2017
	 * @see <a href="http://www.aclweb.org/anthology/P/P10/P10-1128.pdf">Ryu
	 *      Iida, Shumpei Kobayashi, Takenobu Tokunaga. &ldquo;Incorporating
	 *      Extra-linguistic Information into Reference Resolution in
	 *      Collaborative Task Dialogue&rdquo;. The 48<sup>th</sup> Annual
	 *      Meeting of the Association for Computational Linguistics (ACL 2010),
	 *      pp. 1259&ndash;1267. 2010.</a>
	 *
	 */
	private enum ActionFeature {
		LAST_SUBMITTED_EVENT_TYPE, SELECTED_ENTITY;

		private static final List<GameManagementEvent> EVENT_TYPE_FEATURE_ORDERING;

		private static final Object2DoubleMap<GameManagementEvent> EVENT_TYPE_FEATURE_VALS;

		private static final List<ActionFeature> ORDERING;

		static {
			ORDERING = Arrays.asList(SELECTED_ENTITY, LAST_SUBMITTED_EVENT_TYPE);
			assert ORDERING.size() == ActionFeature.values().length;
		}

		static {
			EVENT_TYPE_FEATURE_ORDERING = Arrays.asList(GameManagementEvent.COMPLETED_TURN_REQUEST,
					GameManagementEvent.GAME_READY_RESPONSE, GameManagementEvent.NEXT_TURN_REQUEST,
					GameManagementEvent.PLAYER_JOIN_REQUEST, GameManagementEvent.PLAYER_JOIN_RESPONSE,
					GameManagementEvent.SELECTION_REJECTION, GameManagementEvent.SELECTION_REQUEST);
			assert EVENT_TYPE_FEATURE_ORDERING.size() == GameManagementEvent.values().length;
			EVENT_TYPE_FEATURE_VALS = new Object2DoubleOpenHashMap<>(EVENT_TYPE_FEATURE_ORDERING.size() + 1);
			FeatureMaps.putOrdinalFeatureVals(EVENT_TYPE_FEATURE_VALS, EVENT_TYPE_FEATURE_ORDERING);
			EVENT_TYPE_FEATURE_VALS.put(null, -1.0);
		}

		private static Stream<String> createFeatureDescriptions() {
			return ActionFeature.ORDERING.stream().map(Enum::toString);
		}

		private static void setVals(final DoubleStream.Builder vals, final NavigableMap<Timestamp, List<Event>> events,
				final Timestamp time, final String speakingPlayerId) {
			for (final ActionFeature feature : ORDERING) {
				switch (feature) {
				case LAST_SUBMITTED_EVENT_TYPE: {
					final NavigableMap<Timestamp, List<Event>> timedEventsBeforeUtt = events.headMap(time, true);
					// Look for the last time the speaking player submitted an
					// event, iterating backwards
					final Stream<Event> eventsReversed = timedEventsBeforeUtt.descendingMap().values().stream()
							.map(Lists::reverse).flatMap(List::stream);
					final Predicate<Event> lastSubmittedByPlayerMatcher = new EventSubmittingPlayerMatcher(
							speakingPlayerId);
					final Optional<Event> lastEventSubmittedByPlayer = eventsReversed
							.filter(lastSubmittedByPlayerMatcher).findFirst();
					final GameManagementEvent lastEventType = lastEventSubmittedByPlayer.isPresent()
							? GameManagementEvent.getEventType(lastEventSubmittedByPlayer.get()) : null;
					final double val = EVENT_TYPE_FEATURE_VALS.get(lastEventType);
					vals.accept(val);
					break;
				}
				case SELECTED_ENTITY: {
					final NavigableMap<Timestamp, List<Event>> timedEventsBeforeUtt = events.headMap(time, true);
					final Stream<Event> eventsDescTime = getValuesDescendingOrder(timedEventsBeforeUtt);
					final String moveAttrName = GameManagementEvent.Attribute.MOVE.toString();
					final Optional<Event> lastSelectionEvent = eventsDescTime.filter(event -> event.has(moveAttrName))
							.findFirst();
					// TODO: This will break if the entity IDs are not valid
					// numeric values; Generalize so that the strings are
					// treated as ordinal values (e.g. "1" -> first, "2" ->
					// second, etc.)
					final double val;
					if (lastSelectionEvent.isPresent()) {
						final Event event = lastSelectionEvent.get();
						final Move move = (Move) event.get(moveAttrName);
						final Integer entityId = move.getPieceId();
						val = entityId.doubleValue();
					} else {
						val = -1.0;
					}
					vals.accept(val);
					break;
				}
				default: {
					throw new AssertionError("Missing enum-handling logic.");
				}
				}
			}
		}
	}

	/**
	 *
	 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
	 * @since 13 Apr 2017
	 * @see <a href="http://www.aclweb.org/anthology/P/P10/P10-1128.pdf">Ryu
	 *      Iida, Shumpei Kobayashi, Takenobu Tokunaga. &ldquo;Incorporating
	 *      Extra-linguistic Information into Reference Resolution in
	 *      Collaborative Task Dialogue&rdquo;. The 48<sup>th</sup> Annual
	 *      Meeting of the Association for Computational Linguistics (ACL 2010),
	 *      pp. 1259&ndash;1267. 2010.</a>
	 *
	 */
	private enum EventHistoryFeature {
		LAST_OCC_DIST, LAST_SPEAKER_SUB_DIST;

		private static final List<GameManagementEvent> EVENT_TYPE_FEATURE_ORDERING;

		private static final Object2DoubleMap<GameManagementEvent> EVENT_TYPE_FEATURE_VALS;

		private static final Map<GameManagementEvent, Predicate<Event>> EVENT_TYPE_MATCHERS;

		private static final List<EventHistoryFeature> ORDERING;

		static {
			ORDERING = Arrays.asList(LAST_OCC_DIST, LAST_SPEAKER_SUB_DIST);
			assert ORDERING.size() == EventHistoryFeature.values().length;
		}

		static {
			EVENT_TYPE_FEATURE_ORDERING = Arrays.asList(GameManagementEvent.COMPLETED_TURN_REQUEST,
					GameManagementEvent.GAME_READY_RESPONSE, GameManagementEvent.NEXT_TURN_REQUEST,
					GameManagementEvent.PLAYER_JOIN_REQUEST, GameManagementEvent.PLAYER_JOIN_RESPONSE,
					GameManagementEvent.SELECTION_REJECTION, GameManagementEvent.SELECTION_REQUEST);
			assert EVENT_TYPE_FEATURE_ORDERING.size() == GameManagementEvent.values().length;
			final Function<GameManagementEvent, Predicate<Event>> eventTypeMatcherFactory = eventType -> new EventTypeMatcher(
					EnumSet.of(eventType));
			EVENT_TYPE_MATCHERS = new EnumMap<>(GameManagementEvent.class);
			Arrays.stream(GameManagementEvent.values())
					.forEach(eventType -> EVENT_TYPE_MATCHERS.put(eventType, eventTypeMatcherFactory.apply(eventType)));
			EVENT_TYPE_FEATURE_VALS = new Object2DoubleOpenHashMap<>(EVENT_TYPE_FEATURE_ORDERING.size() + 1);
			FeatureMaps.putOrdinalFeatureVals(EVENT_TYPE_FEATURE_VALS, EVENT_TYPE_FEATURE_ORDERING);
			EVENT_TYPE_FEATURE_VALS.put(null, -1.0);
		}

		private static Stream<String> createFeatureDescriptions() {
			final int eventTypeCount = GameManagementEvent.values().length;
			return IntStream.range(0, eventTypeCount).mapToObj(eventId -> {
				final Stream<String> baseFeatureDescs = EventHistoryFeature.ORDERING.stream().map(Enum::toString);
				return baseFeatureDescs.map(desc -> "EVT_" + eventId + "-" + desc);
			}).flatMap(Function.identity());
		}

		private static <E> int findFirstMatchingDistance(final Stream<E> elems, final Predicate<? super E> matcher) {
			int result = -1;
			int currentDist = 0;
			for (final Iterator<E> elemIter = elems.iterator(); elemIter.hasNext();) {
				final E elem = elemIter.next();
				if (matcher.test(elem)) {
					LOGGER.debug("Found matching element: {}", elem);
					result = currentDist;
					break;
				}
				currentDist++;
			}
			return result;
		}

		private static void setVals(final DoubleStream.Builder vals, final NavigableMap<Timestamp, List<Event>> events,
				final Timestamp time, final String speakingPlayerId) {
			for (final GameManagementEvent eventType : EVENT_TYPE_FEATURE_ORDERING) {
				final Predicate<Event> eventTypeMatcher = EVENT_TYPE_MATCHERS.get(eventType);
				for (final EventHistoryFeature feature : ORDERING) {
					switch (feature) {
					case LAST_OCC_DIST: {
						final NavigableMap<Timestamp, List<Event>> timedEventsBeforeUtt = events.headMap(time, true);
						// Look for the last time the event was seen (iterating
						// backwards)
						final Stream<Event> eventsDescTime = getValuesDescendingOrder(timedEventsBeforeUtt);
						final int lastOccurrenceDist = findFirstMatchingDistance(eventsDescTime, eventTypeMatcher);
						vals.accept(lastOccurrenceDist);
						break;
					}
					case LAST_SPEAKER_SUB_DIST: {
						final NavigableMap<Timestamp, List<Event>> timedEventsBeforeUtt = events.headMap(time, true);
						// Look for the last time the event submitted by the
						// speaking player (iterating
						// backwards)
						final Predicate<Event> matcher = eventTypeMatcher
								.and(new EventSubmittingPlayerMatcher(speakingPlayerId));
						final Stream<Event> eventsDescTime = getValuesDescendingOrder(timedEventsBeforeUtt);
						final int lastOccurrenceDist = findFirstMatchingDistance(eventsDescTime, matcher);
						vals.accept(lastOccurrenceDist);
						break;
					}
					default: {
						throw new AssertionError("Missing enum-handling logic.");
					}
					}
				}
			}
		}
	}

	private static final Logger LOGGER = LoggerFactory.getLogger(GameEventFeatureExtractor.class);

	private static <V> Stream<V> getValuesDescendingOrder(final NavigableMap<?, ? extends List<? extends V>> map) {
		return map.descendingMap().values().stream().map(Lists::reverse).flatMap(List::stream);
	}

	@Override
	public void accept(final GameContext gameState, final DoubleStream.Builder vals) {
		final NavigableMap<Timestamp, List<Event>> events = gameState.getHistory().getEvents();
		final Timestamp uttStartTimestamp = gameState.getTime();
		final String playerId = gameState.getPlayerId();
		ActionFeature.setVals(vals, events, uttStartTimestamp, playerId);
		EventHistoryFeature.setVals(vals, events, uttStartTimestamp, playerId);
	}

	@Override
	public Stream<String> createFeatureDescriptions(final GameStateDescription initialState) {
		final Stream.Builder<String> resultBuilder = Stream.builder();
		ActionFeature.createFeatureDescriptions().forEachOrdered(resultBuilder);
		EventHistoryFeature.createFeatureDescriptions().forEachOrdered(resultBuilder);
		return resultBuilder.build();
	}

}