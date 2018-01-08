/*
 *  This file is part of Tangrams-restricted.
 *
 *  Tangrams-restricted is free software: you can redistribute it and/or modify
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

import java.awt.Color;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.Random;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.Assert;
import org.junit.experimental.theories.DataPoints;
import org.junit.experimental.theories.Theories;
import org.junit.experimental.theories.Theory;
import org.junit.runner.RunWith;

import com.google.common.collect.Sets;

import se.kth.speech.RandomTests;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo.Datum;
import se.kth.speech.junit.IteratorEqualityAsserter;

/**
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since 10 Mar 2017
 *
 */
@RunWith(Theories.class)
public final class RandomImageVisualizationInfoFactoryTest {

	@DataPoints
	public static final long[] TEST_SEEDS = RandomTests.getSeed().longs().distinct().limit(10).toArray();

	private static final IteratorEqualityAsserter<Object> ITER_EQUALITY_ASSERTER = new IteratorEqualityAsserter<>();

	private static final List<Color> TEST_COLORS = Arrays.asList(Color.RED, Color.YELLOW, Color.GREEN, Color.BLUE);

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.content.RandomImageVisualizationInfoFactory#next()}.
	 */
	@Theory
	public void testNextNotEmpty(final long s) {
		final Random rnd = new Random(s);
		final Optional<ImageVisualizationInfo> any = Stream
				.of(new RandomImageVisualizationInfoFactory(rnd, TEST_COLORS).apply(1)).findAny();
		Assert.assertTrue(any.isPresent());
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.content.RandomImageVisualizationInfoFactory#next()}.
	 *
	 * @throws InterruptedException
	 */
	@Theory
	public void testNextStable(final long s) throws InterruptedException {
		final Random rnd1 = new Random(s);
		final RandomImageVisualizationInfoFactory f1 = new RandomImageVisualizationInfoFactory(rnd1, TEST_COLORS);
		final List<Datum> results1 = f1.apply(f1.combinationCount()).getData();
		Thread.sleep(100);
		final Random rnd2 = new Random(s);
		final RandomImageVisualizationInfoFactory f2 = new RandomImageVisualizationInfoFactory(rnd2, TEST_COLORS);
		final List<Datum> results2 = f2.apply(f2.combinationCount()).getData();
		ITER_EQUALITY_ASSERTER.accept(results1.iterator(), results2.iterator());
		Thread.sleep(100);
		final Random rnd3 = new Random(s);
		final RandomImageVisualizationInfoFactory f3 = new RandomImageVisualizationInfoFactory(rnd3, TEST_COLORS);
		final List<Datum> results3 = f3.apply(f3.combinationCount()).getData();
		ITER_EQUALITY_ASSERTER.accept(results1.iterator(), results3.iterator());
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.content.RandomImageVisualizationInfoFactory#next()}.
	 */
	@Theory
	public void testNextUnique(final long seed) {
		final Random rnd = new Random(seed);
		final RandomImageVisualizationInfoFactory f = new RandomImageVisualizationInfoFactory(rnd, TEST_COLORS);
		final List<Datum> results = new ArrayList<>(f.apply(f.combinationCount()).getData());
		final Set<ImageVisualizationInfo.Datum> distinctResults = results.stream()
				.collect(Collectors.toCollection(() -> Sets.newHashSetWithExpectedSize(results.size())));
		results.removeAll(distinctResults);
		Assert.assertTrue("Some elements were duplicated: " + results, results.isEmpty());
	}

}
