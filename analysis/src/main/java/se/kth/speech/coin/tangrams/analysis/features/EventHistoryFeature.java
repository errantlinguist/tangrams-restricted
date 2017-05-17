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
package se.kth.speech.coin.tangrams.analysis.features;

import java.util.Arrays;
import java.util.EnumMap;
import java.util.Map;
import java.util.Optional;
import java.util.OptionalInt;
import java.util.function.Function;
import java.util.function.Predicate;

import iristk.system.Event;
import se.kth.speech.coin.tangrams.analysis.GameContext;
import se.kth.speech.coin.tangrams.iristk.EventSubmittingPlayerMatcher;
import se.kth.speech.coin.tangrams.iristk.EventTypeMatcher;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;

/**
 *
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 13 Apr 2017
 * @see <a href="http://www.aclweb.org/anthology/P/P10/P10-1128.pdf">Ryu Iida,
 *      Shumpei Kobayashi, Takenobu Tokunaga. &ldquo;Incorporating
 *      Extra-linguistic Information into Reference Resolution in Collaborative
 *      Task Dialogue&rdquo;. The 48<sup>th</sup> Annual Meeting of the
 *      Association for Computational Linguistics (ACL 2010),
 *      pp.&nbsp;1259&ndash;1267. 2010.</a>
 *
 */
public enum EventHistoryFeature {
	/**
	 * The number of events which are between the last time the given event was
	 * observed and the current event.
	 */
	LAST_OCCURRENCE_DIST,
	/**
	 * The number of events which are between the last time the given event was
	 * submitted by the player whose perspective is being used for feature
	 * extraction.
	 */
	LAST_SPEAKER_SUBMISSION_DIST;

	public static final class Extractor implements FeatureExtractor<EventHistoryFeature, GameContext> {

		private final Predicate<Event> gameEventToConditionMatcher;

		public Extractor(final GameManagementEvent gameEventToCondition) {
			this(EVENT_TYPE_MATCHERS.get(gameEventToCondition));
		}

		private Extractor(final Predicate<Event> gameEventToConditionMatcher) {
			this.gameEventToConditionMatcher = gameEventToConditionMatcher;
		}

		@Override
		public Optional<Object> apply(final EventHistoryFeature feature, final GameContext context) {
			final Optional<Object> result;
			switch (feature) {
			case LAST_OCCURRENCE_DIST: {
				final OptionalInt lastOccurrenceDist = context.findLastEventDistance(gameEventToConditionMatcher);
				result = lastOccurrenceDist.isPresent() ? Optional.of(lastOccurrenceDist.getAsInt()) : Optional.empty();
				break;
			}
			case LAST_SPEAKER_SUBMISSION_DIST: {
				final Predicate<Event> matcher = gameEventToConditionMatcher
						.and(new EventSubmittingPlayerMatcher(context.getPersepectivePlayerId()));
				final OptionalInt lastOccurrenceDist = context.findLastEventDistance(matcher);
				result = lastOccurrenceDist.isPresent() ? Optional.of(lastOccurrenceDist.getAsInt()) : Optional.empty();
				break;
			}
			default: {
				throw new AssertionError("Missing enum-handling logic.");
			}
			}
			return result;
		}
	}

	private static final Map<GameManagementEvent, EventTypeMatcher> EVENT_TYPE_MATCHERS;

	static {
		final Function<GameManagementEvent, EventTypeMatcher> eventTypeMatcherFactory = eventType -> new EventTypeMatcher(
				eventType);
		EVENT_TYPE_MATCHERS = new EnumMap<>(GameManagementEvent.class);
		Arrays.stream(GameManagementEvent.values())
				.forEach(eventType -> EVENT_TYPE_MATCHERS.put(eventType, eventTypeMatcherFactory.apply(eventType)));
	}
}