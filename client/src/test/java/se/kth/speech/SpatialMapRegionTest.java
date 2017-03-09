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
package se.kth.speech;

import java.util.Random;
import java.util.Set;
import java.util.stream.IntStream;

import org.junit.Assert;
import org.junit.Assume;
import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.theories.DataPoints;
import org.junit.experimental.theories.Theories;
import org.junit.experimental.theories.Theory;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;

import com.google.common.collect.Sets;

import se.kth.speech.SpatialMap.Region;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 9 Mar 2017
 *
 */
@RunWith(Theories.class)
public final class SpatialMapRegionTest {

	@DataPoints
	public static final int[] TEST_POINTS;

	@DataPoints
	public static final Set<Region> TEST_REGIONS;

	static {
		final Random rnd = new Random();
		final int testPointCount = 10;
		TEST_POINTS = createRandomIntArray(rnd, testPointCount);

		final int testRegionCount = testPointCount / 4;
		TEST_REGIONS = Sets.newHashSetWithExpectedSize(testRegionCount);
		do {
			final int[] boundaries = createRandomBoundingBoxArray(rnd, TEST_POINTS);
			TEST_REGIONS.add(new Region(boundaries));
		} while (TEST_REGIONS.size() < testRegionCount);
	}

	private static int[] createRandomBoundingBoxArray(final Random rnd, final int[] points) {
		final int[] x = createRandomIntervalArray(rnd, points);
		final int[] y = createRandomIntervalArray(rnd, points);
		return IntArrays.concatenate(x, y);
	}

	private static int[] createRandomIntArray(final Random rnd, final int resultSetSize) {
		return IntStream.generate(rnd::nextInt).distinct().limit(resultSetSize).toArray();
	}

	private static int[] createRandomIntervalArray(final Random rnd, final int[] points) {
		final int first = RandomCollections.getRandomElement(points, rnd);
		final int second = RandomCollections.getRandomElement(points, rnd);
		return first <= second ? new int[] { first, second } : new int[] { second, first };
	}

	@Rule
	public final ExpectedException exception = ExpectedException.none();

	/**
	 * Test method for
	 * {@link se.kth.speech.SpatialMap.Region#intersects(se.kth.speech.SpatialMap.Region)}.
	 */
	@Test
	public final void testIntersects() {

	}

	/**
	 * Test method for
	 * {@link se.kth.speech.SpatialMap.Region#Region(int, int, int, int)}.
	 */
	@Theory
	public final void testRegionNegative(final int xLowerBound, final int xUpperBound, final int yLowerBound,
			final int yUpperBound) {
		Assume.assumeFalse("Lower bound of x less than or equal to upper bound.", xLowerBound <= xUpperBound);
		Assume.assumeFalse("Lower bound of y less than or equal to upper bound.", yLowerBound <= yUpperBound);
		exception.expect(IllegalArgumentException.class);
		new Region(xLowerBound, xUpperBound, yLowerBound, yUpperBound);
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.SpatialMap.Region#Region(int, int, int, int)}.
	 */
	@Theory
	public final void testRegionPositive(final int xLowerBound, final int xUpperBound, final int yLowerBound,
			final int yUpperBound) {
		Assume.assumeTrue("Lower bound of x not less than or equal to upper bound.", xLowerBound <= xUpperBound);
		Assume.assumeTrue("Lower bound of y not less than or equal to upper bound.", yLowerBound <= yUpperBound);
		new Region(xLowerBound, xUpperBound, yLowerBound, yUpperBound);
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.SpatialMap.Region#subsumesX(int, int[])}.
	 */
	@Test
	public final void testSubsumesXNegative() {
		final Region r = new Region(0, 2, 3, 4);
		Assert.assertFalse(r.subsumesX(4));
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.SpatialMap.Region#subsumesX(int, int[])}.
	 */
	@Test
	public final void testSubsumesXPositive() {
		final Region r = new Region(0, 2, 3, 4);
		Assert.assertTrue(r.subsumesX(2));
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.SpatialMap.Region#subsumesY(int, int[])}.
	 */
	@Test
	public final void testSubsumesYNegative() {
		final Region r = new Region(0, 2, 3, 4);
		Assert.assertFalse(r.subsumesY(5));
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.SpatialMap.Region#subsumesY(int, int[])}.
	 */
	@Test
	public final void testSubsumesYPositive() {
		final Region r = new Region(0, 2, 3, 4);
		Assert.assertTrue(r.subsumesY(4));
	}

}
