/*
 *  This file is part of se.kth.speech.coin.tangrams.server.
 *
 *  se.kth.speech.coin.tangrams.server is free software: you can redistribute it and/or modify
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
package se.kth.speech.coin.tangrams;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import org.junit.Assert;
import org.junit.Test;
import org.junit.experimental.theories.DataPoints;
import org.junit.experimental.theories.Theories;
import org.junit.experimental.theories.Theory;
import org.junit.runner.RunWith;

import se.kth.speech.coin.tangrams.GameTests.TestDescription;
import se.kth.speech.coin.tangrams.game.Model;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 20 Feb 2017
 *
 */
@RunWith(Theories.class)
public final class GameFactoryTest {

	@DataPoints("gameTestDescs")
	public static Collection<TestDescription> getGameTestDescs() {
		return GameTests.getNamedTestDescMap().values();
	}
	
	private static GameFactory createGameFactory(final TestDescription testDesc) {
		final int[] modelDims = testDesc.getModelDims();
		final RandomModelFactory<Integer> modelFactory = new RandomModelFactory<>(modelDims,
				testDesc.createCoordOccupantArray());
		return new GameFactory(modelFactory, modelDims[1]);
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.GameFactory#apply(java.lang.String)}.
	 */
	@Theory
	public final void testApply(final TestDescription testDesc) {
		final GameFactory gameFactory = createGameFactory(testDesc);
		final long seed = testDesc.getSeed();
		final String gameName = Long.toString(seed);
		final Game<Integer> actualGame = gameFactory.apply(gameName);
		// NOTE: This is only a partial (i.e. sanity) check
		Assert.assertEquals(seed, actualGame.getSeed());
		final Model<Integer> expectedModel = RandomModelFactoryTest.createExpectedModel(testDesc);
		final Model<Integer> actualModel = actualGame.getRemoteController().getModel();
		Assert.assertEquals(expectedModel, actualModel);

		final Set<Integer> expectedModelPieceSet = new HashSet<>(
				expectedModel.getCoordinateOccupants().getValues());
		final Set<Integer> actualModelPieceSet = new HashSet<>(
				actualModel.getCoordinateOccupants().getValues());
		Assert.assertEquals(expectedModelPieceSet, actualModelPieceSet);
		final Set<Integer> actualWinningModelPieceSet = new HashSet<>(
				actualGame.getWinningModel().getCoordinateOccupants().getValues());
		Assert.assertEquals(expectedModelPieceSet, actualWinningModelPieceSet);
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.GameFactory#apply(java.lang.String)}.
	 */
	@Test
	public final void testApplyDefaultConstructor() {
		new GameFactory().apply("1");
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.GameFactory#GameFactory()}.
	 */
	@Theory
	public final void testGameFactory(final TestDescription testDesc) {
		createGameFactory(testDesc);
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.GameFactory#GameFactory()}.
	 */
	@Test
	public final void testGameFactoryDefaultConstructor() {
		new GameFactory();
	}

}
