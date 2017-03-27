/*
 *  This file is part of client.
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
import java.util.Random;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.junit.Assert;
import org.junit.Rule;
import org.junit.experimental.theories.DataPoints;
import org.junit.experimental.theories.Theories;
import org.junit.experimental.theories.Theory;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.mockito.Mockito;

import se.kth.speech.MapEntryRemapping;
import se.kth.speech.Matrix;
import se.kth.speech.SpatialMatrix;
import se.kth.speech.SpatialRegion;
import se.kth.speech.coin.tangrams.MatrixTests;
import se.kth.speech.coin.tangrams.SpatialMatrixTests;
import se.kth.speech.coin.tangrams.iristk.GameManagementClientModule;
import se.kth.speech.coin.tangrams.iristk.events.Area2D;
import se.kth.speech.coin.tangrams.iristk.events.Selection;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 24 Mar 2017
 *
 */
@RunWith(Theories.class)
public final class ControllerHistoryTest {

	@DataPoints("testMatrices")
	public static final Collection<SpatialMatrix<Integer>> TEST_MODELS;

	@DataPoints("seeds")
	public static final long[] TEST_SEEDS = new Random().longs().distinct().limit(5).toArray();

	private static final GameManagementClientModule CLIENT_MODULE = Mockito.mock(GameManagementClientModule.class);

	private static final Function<SpatialRegion, Area2D> REGION_AREA_FACTORY = new SpatialRegionAreaFactory();

	static {
		final Collection<MatrixTests.Description> testDescs = MatrixTests.getNamedTestDescs().values();
		TEST_MODELS = testDescs.stream().map(desc -> new Matrix<>(desc.getValues(), desc.getColCount()))
				.map(SpatialMatrix<Integer>::new)
				.collect(Collectors.toCollection(() -> new ArrayList<>(testDescs.size())));
	}

	private static MapEntryRemapping<Integer, SpatialRegion> submitRandomTurn(final Controller controller,
			final Random rnd) {
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

	@Rule
	public final ExpectedException thrown = ExpectedException.none();

	@Theory
	public void testGetTurnCountNoMoves(final SpatialMatrix<Integer> model, final long seed) {
		final Controller controller = new Controller(model, "localPlayer", PlayerRole.MOVE_SUBMISSION, CLIENT_MODULE);
		final Controller.History history = controller.getHistory();
		Assert.assertEquals(0, history.getTurnCount());
	}

	@Theory
	public void testGetTurnCountOneTurn(final SpatialMatrix<Integer> model, final long seed) {
		final Controller controller = new Controller(model, "localPlayer", PlayerRole.MOVE_SUBMISSION, CLIENT_MODULE);
		final Random rnd = new Random(seed);
		submitRandomTurn(controller, rnd);
		final Controller.History history = controller.getHistory();
		Assert.assertEquals(1, history.getTurnCount());
	}

}
