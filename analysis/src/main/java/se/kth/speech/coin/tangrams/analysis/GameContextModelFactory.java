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

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.function.Function;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Maps;

import iristk.system.Event;
import se.kth.speech.Matrix;
import se.kth.speech.SpatialMatrix;
import se.kth.speech.SpatialMatrixRegionElementMover;
import se.kth.speech.SpatialRegion;
import se.kth.speech.coin.tangrams.AreaSpatialRegionFactory;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;
import se.kth.speech.coin.tangrams.iristk.events.GameStateDescription;
import se.kth.speech.coin.tangrams.iristk.events.GameStateUnmarshalling;
import se.kth.speech.coin.tangrams.iristk.events.ModelDescription;
import se.kth.speech.coin.tangrams.iristk.events.Move;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 27 Apr 2017
 *
 */
public final class GameContextModelFactory implements Function<GameContext, SpatialMatrix<Integer>> {

	private static final Logger LOGGER = LoggerFactory.getLogger(GameContextModelFactory.class);

	private static final SpatialMatrix.Factory MODEL_FACTORY = SpatialMatrix.Factory.UNSTABLE_ITER_ORDER;

	private static void applyEvent(final SpatialMatrix<Integer> model, final Event event) {
		final Move move = (Move) event.get(GameManagementEvent.Attribute.MOVE.toString());
		if (move == null) {
			LOGGER.debug("Event has no move attribute; Ignoring.");
		} else {
			applyMove(model, move);
		}
	}

	private static void applyMove(final SpatialMatrix<Integer> model, final Move move) {
		final AreaSpatialRegionFactory areaRegionFactory = new AreaSpatialRegionFactory(model);
		final SpatialMatrixRegionElementMover<Integer> pieceUpdater = new SpatialMatrixRegionElementMover<>(model);
		final SpatialRegion source = areaRegionFactory.apply(move.getSource());
		final SpatialRegion target = areaRegionFactory.apply(move.getTarget());
		pieceUpdater.accept(source, target);
	}

	private static SpatialMatrix<Integer> copyInitialModel(final Matrix<Integer> copyee) {
		final List<Integer> copiedVals = new ArrayList<>(copyee.getValues());
		final Matrix<Integer> deepCopy = new Matrix<>(copiedVals, copyee.getDimensions()[1]);
		return MODEL_FACTORY.create(deepCopy);
	}

	private static SpatialMatrix<Integer> copyInitialModel(final SpatialMatrix<Integer> copyee) {
		return copyInitialModel(copyee.getPositionMatrix());
	}

	private static Function<ModelDescription, SpatialMatrix<Integer>> createCachingInitialGameModelFactory(
			final int expectedniqueModelCount) {
		final Map<ModelDescription, SpatialMatrix<Integer>> gameModels = Maps
				.newHashMapWithExpectedSize(expectedniqueModelCount);
		return modelDesc -> gameModels.computeIfAbsent(modelDesc,
				k -> GameStateUnmarshalling.createModel(k, MODEL_FACTORY));
	}

	private static void updateToTime(final SpatialMatrix<Integer> model,
			final NavigableMap<LocalDateTime, List<Event>> timestampedEvents, final LocalDateTime time) {
		final NavigableMap<LocalDateTime, List<Event>> timestampedEventsToApply = timestampedEvents.headMap(time, true);
		final Stream<Event> eventsToApply = timestampedEventsToApply.values().stream().flatMap(List::stream);
		eventsToApply.forEachOrdered(event -> applyEvent(model, event));
	}

	private final Function<? super ModelDescription, SpatialMatrix<Integer>> initialGameModelFactory;

	public GameContextModelFactory(final int expectedUniqueModelCount) {
		this(createCachingInitialGameModelFactory(expectedUniqueModelCount));
	}

	private GameContextModelFactory(
			final Function<? super ModelDescription, SpatialMatrix<Integer>> initialGameModelFactory) {
		this.initialGameModelFactory = initialGameModelFactory;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Function#apply(java.lang.Object)
	 */
	@Override
	public SpatialMatrix<Integer> apply(final GameContext context) {
		final GameHistory history = context.getHistory();
		final GameStateDescription initialState = history.getInitialState();
		final SpatialMatrix<Integer> result = copyInitialModel(
				initialGameModelFactory.apply(initialState.getModelDescription()));
		final NavigableMap<LocalDateTime, List<Event>> events = history.getEvents();
		updateToTime(result, events, context.getTime());
		return result;
	}

}
