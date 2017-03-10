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
package se.kth.speech.coin.tangrams.content;

import static org.junit.Assert.assertEquals;

import java.util.List;
import java.util.Random;
import java.util.stream.Collectors;
import java.util.stream.LongStream;

import org.junit.experimental.theories.DataPoints;
import org.junit.experimental.theories.Theories;
import org.junit.experimental.theories.Theory;
import org.junit.runner.RunWith;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 10 Mar 2017
 *
 */
@RunWith(Theories.class)
public class RandomPieceImageDataFactoryTest {

	private static final RandomPieceImageDataFactory TEST_FACTORY = new RandomPieceImageDataFactory(
			IconImages.getIconImageResources().entrySet(), 2);

	@DataPoints
	public static final List<Random> TEST_RANDOMS = LongStream.generate(new Random()::nextLong).distinct()
			.mapToObj(Random::new).limit(10).collect(Collectors.toList());

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.content.RandomPieceImageDataFactory#apply(java.util.Random)}.
	 */
	@Theory
	public final void testApplyUnique(final Random rnd) {
		final List<?> results = TEST_FACTORY.apply(rnd).collect(Collectors.toList());
		final List<?> distinctResults = results.stream().distinct().collect(Collectors.toList());
		assertEquals(distinctResults, results);
	}

}
