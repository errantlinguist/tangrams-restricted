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

import java.util.List;
import java.util.Optional;
import java.util.Random;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.experimental.theories.DataPoints;
import org.junit.experimental.theories.Theories;
import org.junit.experimental.theories.Theory;
import org.junit.runner.RunWith;

import se.kth.speech.junit.IteratorEqualityAsserter;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 10 Mar 2017
 *
 */
@RunWith(Theories.class)
public final class ImageVisualizationInfoFactoryTest {

	@DataPoints
	public static final long[] TEST_SEEDS = new Random().longs().distinct().limit(10).toArray();

	private static final IteratorEqualityAsserter<ImageVisualizationInfo> ITER_EQUALITY_ASSERTER = new IteratorEqualityAsserter<>();

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.content.ImageVisualizationInfoFactory#next()}.
	 */
	@Theory
	public final void testNextNotEmpty(final long s) {
		final Random rnd = new Random(s);
		final Optional<ImageVisualizationInfo> any = Stream.of(new ImageVisualizationInfoFactory(rnd).next()).findAny();
		Assert.assertTrue(any.isPresent());
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.content.ImageVisualizationInfoFactory#next()}.
	 */
	@Theory
	public final void testNextStable(final long s) {
		final Random rnd1 = new Random(s);
		final Random rnd2 = new Random(s);
		final Stream<ImageVisualizationInfo> results1 = Stream.generate(new ImageVisualizationInfoFactory(rnd1)::next);
		final Stream<ImageVisualizationInfo> results2 = Stream.generate(new ImageVisualizationInfoFactory(rnd2)::next);
		ITER_EQUALITY_ASSERTER.accept(results1.iterator(), results2.iterator());
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.content.ImageVisualizationInfoFactory#next()}.
	 */
	@Theory
	public final void testNextUnique(final long seed) {
		final Random rnd = new Random(seed);
		final List<ImageVisualizationInfo> results = Stream.generate(new ImageVisualizationInfoFactory(rnd)::next)
				.collect(Collectors.toList());
		final Stream<ImageVisualizationInfo> distinctResults = results.stream().distinct();
		ITER_EQUALITY_ASSERTER.accept(results.iterator(), distinctResults.iterator());
	}

}
