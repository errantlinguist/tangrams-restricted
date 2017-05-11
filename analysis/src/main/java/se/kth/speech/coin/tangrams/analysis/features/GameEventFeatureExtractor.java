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
package se.kth.speech.coin.tangrams.analysis.features;

import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.EnumMap;
import java.util.EnumSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NavigableMap;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import com.google.common.collect.Lists;

import iristk.system.Event;
import it.unimi.dsi.fastutil.objects.Object2DoubleMap;
import it.unimi.dsi.fastutil.objects.Object2DoubleOpenHashMap;
import se.kth.speech.coin.tangrams.analysis.GameContext;
import se.kth.speech.coin.tangrams.iristk.EventSubmittingPlayerMatcher;
import se.kth.speech.coin.tangrams.iristk.EventTypeMatcher;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;
import se.kth.speech.coin.tangrams.iristk.events.GameStateDescription;
import weka.core.Attribute;
import weka.core.Instance;

final class GameEventFeatureExtractor implements GameContextFeatureExtractor {

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
	public enum ActionFeature {
		LAST_SUBMITTED_EVENT_TYPE, SELECTED_ENTITY;

		public static final class Extractor {

			private final Map<ActionFeature, Attribute> featureAttrs;

			public Extractor(final Map<ActionFeature, Attribute> featureAttrs) {
				this.featureAttrs = featureAttrs;
			}

			private void setVals(final Instance vals, final GameContext context) {
				for (final Entry<ActionFeature, Attribute> featureAttr : featureAttrs.entrySet()) {
					ActionFeature feature = featureAttr.getKey();
					Attribute attr = featureAttr.getValue();
					switch (feature) {
					case LAST_SUBMITTED_EVENT_TYPE: {
						final NavigableMap<LocalDateTime, List<Event>> timedEventsBeforeUtt = context
								.getPrecedingEvents();
						// Look for the last time the speaking player submitted
						// an
						// event, iterating backwards
						final Stream<Event> eventsReversed = timedEventsBeforeUtt.descendingMap().values().stream()
								.map(Lists::reverse).flatMap(List::stream);
						final Predicate<Event> lastSubmittedByPlayerMatcher = new EventSubmittingPlayerMatcher(
								context.getPersepectivePlayerId());
						final Optional<Event> lastEventSubmittedByPlayer = eventsReversed
								.filter(lastSubmittedByPlayerMatcher).findFirst();
						final GameManagementEvent lastEventType = lastEventSubmittedByPlayer
								.map(GameManagementEvent::getEventType).orElse(null);
						// final double val =
						// EVENT_TYPE_FEATURE_VALS.getDouble(lastEventType);
						vals.setValue(attr, lastEventType.name());
						break;
					}
					case SELECTED_ENTITY: {
						final Optional<Integer> lastSelectedEntityId = context.findLastSelectedEntityId();
						// TODO: This will break if the entity IDs are not valid
						// numeric values; Generalize so that the strings are
						// treated as ordinal values (e.g. "1" -> first, "2" ->
						// second, etc.)
						// final double val;
						// if (lastSelectedEntityId.isPresent()) {
						// final Integer entityId = lastSelectedEntityId.get();
						// val = entityId.doubleValue();
						// } else {
						// val = NULL_FEATURE_VAL;
						// }
						// vals.setValue(attr, val);
						lastSelectedEntityId.ifPresent(entityId -> vals.setValue(attr, entityId));
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
	public enum EventHistoryFeature {
		LAST_OCC_DIST, LAST_SPEAKER_SUB_DIST;

		private static final Map<GameManagementEvent, Predicate<Event>> EVENT_TYPE_MATCHERS;

		static {
			final Function<GameManagementEvent, Predicate<Event>> eventTypeMatcherFactory = eventType -> new EventTypeMatcher(
					EnumSet.of(eventType));
			EVENT_TYPE_MATCHERS = new EnumMap<>(GameManagementEvent.class);
			Arrays.stream(GameManagementEvent.values())
					.forEach(eventType -> EVENT_TYPE_MATCHERS.put(eventType, eventTypeMatcherFactory.apply(eventType)));
		}

//		public static final class Extractor {
//			
//			private final Map<EventHistoryFeature, Attribute> featureAttrs;
//			
//			public Extractor(final Map<EventHistoryFeature, Attribute> featureAttrs){
//				this.featureAttrs = featureAttrs;
//			}
//
//			private void setVals(final Instance vals, final GameContext context) {
//				for (final GameManagementEvent eventType : EVENT_TYPE_FEATURE_ORDERING) {
//					final Predicate<Event> eventTypeMatcher = EVENT_TYPE_MATCHERS.get(eventType);
//					for (final Entry<EventHistoryFeature, Attribute> featureAttr : featureAttrs.entrySet()) {
//						EventHistoryFeature feature = featureAttr.getKey();
//						Attribute attr = featureAttr.getValue();
//						switch (feature) {
//						case LAST_OCC_DIST: {
//							final int lastOccurrenceDist = context.findLastEventDistance(eventTypeMatcher);
//							vals.setValue(attr, lastOccurrenceDist);
//							break;
//						}
//						case LAST_SPEAKER_SUB_DIST: {
//							final Predicate<Event> matcher = eventTypeMatcher
//									.and(new EventSubmittingPlayerMatcher(context.getPlayerId()));
//							final int lastOccurrenceDist = context.findLastEventDistance(matcher);
//							vals.setValue(attr, lastOccurrenceDist);
//							break;
//						}
//						default: {
//							throw new AssertionError("Missing enum-handling logic.");
//						}
//						}
//					}
//				}
//			}
	}

//	private static final double NULL_FEATURE_VAL = -1.0;

	@Override
	public void accept(final GameContext context, final Instance vals) {
//		ActionFeature.setVals(vals, context);
//		EventHistoryFeature.setVals(vals, context);
	}

}