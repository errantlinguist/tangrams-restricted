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

import static org.junit.Assert.assertEquals;

import java.util.Collection;
import java.util.Random;
import java.util.stream.IntStream;

import org.junit.Rule;
import org.junit.experimental.theories.DataPoints;
import org.junit.experimental.theories.Theories;
import org.junit.experimental.theories.Theory;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;

import se.kth.speech.Matrix;
import se.kth.speech.coin.tangrams.GameTests.TestDescription;
import se.kth.speech.coin.tangrams.game.Model;
import se.kth.speech.hat.xsd.Transcription.T;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 20 Feb 2017
 *
 */
@RunWith(Theories.class)
public final class RandomModelFactoryTest {

	@DataPoints("gameTestDescs")
	public static Collection<TestDescription> getGameTestDescs() {
		return GameTests.getNamedTestDescMap().values();
	}

	static Model<Integer> createExpectedModel(final TestDescription testDesc) {
		return new Model<>(new Matrix<>(testDesc.getCoords(), testDesc.getModelDims()[1]));
	}

	@Rule
	public final ExpectedException thrown = ExpectedException.none();

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.RandomModelFactory#apply(java.util.Random)}.
	 */
	@Theory
	public final void testApply(final TestDescription testDesc) {
		// Create a piece ID array just big enough to fill all the occupied
		// cells rather than every cell in the matrix
		final Integer[] coordOccupants = IntStream.range(0, testDesc.calculateOccupiedCellCount()).boxed()
				.toArray(Integer[]::new);
		final RandomModelFactory<Integer> testFactory = new RandomModelFactory<>(testDesc.getModelDims(),
				coordOccupants);
		final Random rnd = new Random(testDesc.getSeed());
		final Model<Integer> actual = testFactory.apply(rnd);
		final Model<Integer> expected = createExpectedModel(testDesc);
		assertEquals(expected, actual);
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.RandomModelFactory#RandomModelFactory(int[], T[])}.
	 */
	@Theory
	public final void testRandomModelFactoryInvalidModelDims(final TestDescription testDesc) {
		final int[] invalidModelDims = new int[0];
		thrown.expect(IllegalArgumentException.class);
		new RandomModelFactory<>(invalidModelDims, testDesc.createCoordOccupantArray());
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.RandomModelFactory#RandomModelFactory(int[], T[])}.
	 */
	@Theory
	public final void testRandomModelFactoryPositive(final TestDescription testDesc) {
		new RandomModelFactory<>(testDesc.getModelDims(), testDesc.createCoordOccupantArray());
	}

}
