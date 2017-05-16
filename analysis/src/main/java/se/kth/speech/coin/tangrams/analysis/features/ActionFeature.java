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

import java.time.LocalDateTime;
import java.util.List;
import java.util.NavigableMap;
import java.util.Optional;
import java.util.function.Predicate;
import java.util.stream.Stream;

import com.google.common.collect.Lists;

import iristk.system.Event;
import se.kth.speech.coin.tangrams.analysis.GameContext;
import se.kth.speech.coin.tangrams.iristk.EventSubmittingPlayerMatcher;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;

/**
 *
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 13 Apr 2017
 * @see <a href="http://www.aclweb.org/anthology/P/P10/P10-1128.pdf">Ryu Iida,
 *      Shumpei Kobayashi, Takenobu Tokunaga. &ldquo;Incorporating
 *      Extra-linguistic Information into Reference Resolution in Collaborative
 *      Task Dialogue&rdquo;. The 48<sup>th</sup> Annual Meeting of the
 *      Association for Computational Linguistics (ACL 2010), pp.
 *      1259&ndash;1267. 2010.</a>
 *
 */
public enum ActionFeature {
	LAST_SUBMITTED_EVENT_TYPE;

	public static final class Extractor implements FeatureExtractor<ActionFeature, GameContext> {

		@Override
		public Optional<Object> apply(final ActionFeature feature, final GameContext context) {
			final Optional<Object> result;
			switch (feature) {
			case LAST_SUBMITTED_EVENT_TYPE: {
				final NavigableMap<LocalDateTime, List<Event>> timedEventsBeforeUtt = context.getPrecedingEvents();
				// Look for the last time the speaking player submitted
				// an
				// event, iterating backwards
				final Stream<Event> eventsReversed = timedEventsBeforeUtt.descendingMap().values().stream()
						.map(Lists::reverse).flatMap(List::stream);
				final Predicate<Event> lastSubmittedByPlayerMatcher = new EventSubmittingPlayerMatcher(
						context.getPersepectivePlayerId());
				final Optional<Event> lastEventSubmittedByPlayer = eventsReversed.filter(lastSubmittedByPlayerMatcher)
						.findFirst();
				result = lastEventSubmittedByPlayer.map(GameManagementEvent::getEventType);
				break;
			}
			default: {
				throw new AssertionError("Missing enum-handling logic.");
			}
			}
			return result;
		}
	}
}