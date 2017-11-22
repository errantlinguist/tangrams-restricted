/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.client.
 *
 *  client is free software: you can redistribute it and/or modify
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
package se.kth.speech.coin.tangrams.game;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Random;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.junit.Assert;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.Mockito;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.MapEntryRemapping;
import se.kth.speech.Matrix;
import se.kth.speech.RandomCollectionElementChooser;
import se.kth.speech.SpatialMatrix;
import se.kth.speech.SpatialRegion;
import se.kth.speech.coin.tangrams.MatrixTests;
import se.kth.speech.coin.tangrams.SpatialMatrixTests;
import se.kth.speech.coin.tangrams.iristk.GameManagementClient;
import se.kth.speech.coin.tangrams.iristk.events.Area2D;
import se.kth.speech.coin.tangrams.iristk.events.Selection;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 24 Mar 2017
 *
 */
public final class GameplayControllerTest {

	public static final long[] TEST_SEEDS = new long[] { 1, 56, 3 };

	private static final GameManagementClient CLIENT = Mockito.mock(GameManagementClient.class);

	private static final Logger LOGGER = LoggerFactory.getLogger(GameplayControllerTest.class);

	private static final Function<SpatialRegion, Area2D> REGION_AREA_FACTORY = new SpatialRegionAreaFactory();

	private static List<SpatialMatrix<Integer>> createMatrices() {
		final Collection<MatrixTests.Description> testDescs = MatrixTests.getNamedTestDescs().values();
		return testDescs.stream().map(desc -> new Matrix<>(desc.getValues(), desc.getColCount()))
				.map(matrix -> SpatialMatrix.Factory.STABLE_ITER_ORDER.create(matrix))
				.collect(Collectors.toCollection(() -> new ArrayList<>(testDescs.size())));
	}

	private static MapEntryRemapping<Integer, SpatialRegion> submitRandomTurn(final Controller controller,
			final RandomCollectionElementChooser rnd) {
		final MapEntryRemapping<Integer, SpatialRegion> result = SpatialMatrixTests
				.createRandomValidMove(controller.getModel(), rnd);
		final SpatialRegion sourceRegion = result.getOldValue();
		final Integer pieceId = result.getKey();
		final SpatialRegion targetRegion = result.getNewValue();

		controller.submitNextMove(sourceRegion, targetRegion, pieceId);
		controller.notifyPlayerSelection("otherPlayer",
				new Selection(pieceId, REGION_AREA_FACTORY.apply(sourceRegion)));
		controller.submitTurnComplete();
		return result;
	}

	private static void submitTurnComplete(final Controller controller) {
		LOGGER.info("Submitting turn complete signal.");
		try {
			controller.submitTurnComplete();
		} catch (final Exception e) {
			LOGGER.error("Exception on submitting turn.", e);
			throw e;
		}
		LOGGER.error("Successfuly sent turn complete signal.");
	}

	private static void testGetTurnCountNoMoves(final SpatialMatrix<Integer> model, final long seed) {
		final Controller controller = new GameplayController(model, "localPlayer", PlayerRole.MOVE_SUBMISSION, CLIENT);
		Assert.assertEquals(0, controller.getTurnCount());
	}

	private static void testGetTurnCountOneTurn(final SpatialMatrix<Integer> model, final long seed) {
		final Controller controller = new GameplayController(model, "localPlayer", PlayerRole.MOVE_SUBMISSION, CLIENT);
		final Random rnd = new Random(seed);
		submitRandomTurn(controller, new RandomCollectionElementChooser(rnd));
		Assert.assertEquals(1, controller.getTurnCount());
	}

	private static void testNotifyPlayerSelectionPositive(final SpatialMatrix<Integer> model, final long seed) {
		final Controller controller = new GameplayController(model, "localPlayer", PlayerRole.MOVE_SUBMISSION, CLIENT);
		final Random rnd = new Random(seed);
		final MapEntryRemapping<Integer, SpatialRegion> move = SpatialMatrixTests.createRandomValidMove(model,
				new RandomCollectionElementChooser(rnd));
		final SpatialRegion sourceRegion = move.getOldValue();
		final Integer pieceId = move.getKey();
		final SpatialRegion targetRegion = move.getNewValue();

		controller.submitNextMove(sourceRegion, targetRegion, pieceId);
		controller.notifyPlayerSelection("otherPlayer",
				new Selection(pieceId, REGION_AREA_FACTORY.apply(sourceRegion)));
	}

	private static void testSubmitNextMovePositive(final SpatialMatrix<Integer> model, final long seed) {
		final Controller controller = new GameplayController(model, "localPlayer", PlayerRole.MOVE_SUBMISSION, CLIENT);
		final Random rnd = new Random(seed);
		final MapEntryRemapping<Integer, SpatialRegion> move = SpatialMatrixTests.createRandomValidMove(model,
				new RandomCollectionElementChooser(rnd));
		final SpatialRegion sourceRegion = move.getOldValue();
		final Integer pieceId = move.getKey();
		final SpatialRegion targetRegion = move.getNewValue();

		controller.submitNextMove(sourceRegion, targetRegion, pieceId);
		controller.notifyPlayerSelection("otherPlayer",
				new Selection(pieceId, REGION_AREA_FACTORY.apply(sourceRegion)));
		submitTurnComplete(controller);
	}

	private static void testSubmitTurnCompletePositive(final SpatialMatrix<Integer> model, final long seed) {
		final Controller controller = new GameplayController(model, "localPlayer", PlayerRole.MOVE_SUBMISSION, CLIENT);
		final Random rnd = new Random(seed);
		final MapEntryRemapping<Integer, SpatialRegion> move = SpatialMatrixTests.createRandomValidMove(model,
				new RandomCollectionElementChooser(rnd));
		final SpatialRegion sourceRegion = move.getOldValue();
		final Integer pieceId = move.getKey();
		final SpatialRegion targetRegion = move.getNewValue();

		controller.submitNextMove(sourceRegion, targetRegion, pieceId);
		controller.notifyPlayerSelection("otherPlayer",
				new Selection(pieceId, REGION_AREA_FACTORY.apply(sourceRegion)));
		submitTurnComplete(controller);
	}

	@Rule
	public final ExpectedException thrown = ExpectedException.none();

	@Test
	public void testGetTurnCountNoMoves() {
		for (final long seed : TEST_SEEDS) {
			for (final SpatialMatrix<Integer> matrix : createMatrices()) {
				testGetTurnCountNoMoves(matrix, seed);
			}
		}
	}

	@Test
	public void testGetTurnCountOneTurn() {
		for (final long seed : TEST_SEEDS) {
			for (final SpatialMatrix<Integer> matrix : createMatrices()) {
				testGetTurnCountOneTurn(matrix, seed);
			}
		}
	}

	@Test
	public void testNotifyPlayerSelectionPositive() {
		for (final long seed : TEST_SEEDS) {
			for (final SpatialMatrix<Integer> matrix : createMatrices()) {
				testNotifyPlayerSelectionPositive(matrix, seed);
			}
		}
	}

	@Test
	public void testSubmitNextMovePositive() {
		for (final long seed : TEST_SEEDS) {
			for (final SpatialMatrix<Integer> matrix : createMatrices()) {
				testSubmitNextMovePositive(matrix, seed);
			}
		}
	}

	@Test
	public void testSubmitTurnCompletePositive() {
		for (final long seed : TEST_SEEDS) {
			for (final SpatialMatrix<Integer> matrix : createMatrices()) {
				testSubmitTurnCompletePositive(matrix, seed);
			}
		}
	}

}
