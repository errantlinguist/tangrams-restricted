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
	public static final int[][] PREVIOUS_FAILED_REGION_POINT_ARRAYS;

	@DataPoints
	public static final Set<SpatialMap.Region> PREVIOUS_FAILED_REGIONS;

	@DataPoints
	public static final Set<int[]> TEST_POINT_ARRAYS;

	@DataPoints
	public static final int[] TEST_POINTS;

	@DataPoints
	public static final Set<Region> TEST_REGIONS;

	static {
		PREVIOUS_FAILED_REGION_POINT_ARRAYS = new int[][] {
				new int[] { 1989523352, 2023725473, 1079495603, 1079495603 },
				new int[] { -367670549, 1742426776, -582564738, 1954507817 },
				new int[] { 1661512450, 1661512450, -1470324477, 1742426776 } };
		PREVIOUS_FAILED_REGIONS = Sets.newHashSetWithExpectedSize(PREVIOUS_FAILED_REGION_POINT_ARRAYS.length);
		for (final int[] pointArray : PREVIOUS_FAILED_REGION_POINT_ARRAYS) {
			PREVIOUS_FAILED_REGIONS
					.add(new SpatialMap.Region(pointArray[0], pointArray[1], pointArray[2], pointArray[3]));
		}
	}

	static {
		final Random rnd = new Random();
		final int testPointCount = 10;
		TEST_POINTS = createRandomIntArray(rnd, testPointCount);

		final int testRegionCount = testPointCount / 4;
		TEST_REGIONS = Sets.newHashSetWithExpectedSize(testRegionCount);
		TEST_POINT_ARRAYS = Sets.newHashSetWithExpectedSize(testRegionCount);
		do {
			final int[] x = createRandomIntervalArray(rnd, TEST_POINTS);
			final int[] y = createRandomIntervalArray(rnd, TEST_POINTS);
			TEST_REGIONS.add(new Region(x[0], x[1], y[0], y[1]));
			TEST_POINT_ARRAYS.add(IntArrays.concatenate(x, y));
		} while (TEST_REGIONS.size() < testRegionCount);
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
	 * {@link se.kth.speech.SpatialMap.Region#intersects(int,int,int,int,int,int,int,int)}.
	 */
	@Theory
	public final void testIntersectsIntIntIntIntIntIntIntIntCommutativty(final int[] pointArray) {
		final int r1x1 = pointArray[0];
		final int r1x2 = pointArray[1];
		final int r1y1 = pointArray[2];
		final int r1y2 = pointArray[3];
		final int r2x1 = pointArray[4];
		final int r2x2 = pointArray[5];
		final int r2y1 = pointArray[6];
		final int r2y2 = pointArray[7];
		final boolean orig = SpatialMap.Region.intersects(r1x1, r1x2, r1y1, r1y2, r2x1, r2x2, r2y1, r2y2);
		final boolean inverse = SpatialMap.Region.intersects(r2x1, r2x2, r2y1, r2y2, r1x1, r1x2, r1y1, r1y2);
		Assert.assertEquals(orig, inverse);
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.SpatialMap.Region#intersects(se.kth.speech.SpatialMap.Region)}.
	 */
	@Theory
	public final void testIntersectsRegionCommutativity(final Region r1, final Region r2) {
		final boolean r1Test = r1.intersects(r2);
		final boolean r2Test = r2.intersects(r1);
		if (!r1Test == r2Test) {
			final StringBuilder sb = new StringBuilder();
			sb.append(r1);
			sb.append(" was evaluated");
			if (!r1Test) {
				sb.append(" not");
			}
			sb.append(" to intersect ");
			sb.append(r2);
			sb.append(" but the inverse was not true.");
			Assert.fail(sb.toString());
		}
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.SpatialMap.Region#intersects(se.kth.speech.SpatialMap.Region)}.
	 */
	@Test
	public final void testIntersectsRegionIntersecting() {
		final Region r = new Region(0, 2, 3, 5);
		Assert.assertTrue(r.intersects(new Region(0, 2, 2, 5)));
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.SpatialMap.Region#intersects(se.kth.speech.SpatialMap.Region)}.
	 */
	@Test
	public final void testIntersectsRegionNegative() {
		final Region r = new Region(0, 2, 3, 5);
		Assert.assertFalse(r.intersects(new Region(5, 6, 6, 7)));
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.SpatialMap.Region#intersects(se.kth.speech.SpatialMap.Region)}.
	 */
	@Test
	public final void testIntersectsRegionNegativeAdjacentXNonAdjacentY() {
		final Region r = new Region(3, 5, 0, 2);
		Assert.assertFalse(r.intersects(new Region(1, 2, 5, 6)));
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.SpatialMap.Region#intersects(se.kth.speech.SpatialMap.Region)}.
	 */
	@Test
	public final void testIntersectsRegionNegativeAdjacentXSameY() {
		final Region r = new Region(3, 5, 0, 2);
		Assert.assertFalse(r.intersects(new Region(1, 2, 0, 2)));
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.SpatialMap.Region#intersects(se.kth.speech.SpatialMap.Region)}.
	 */
	@Test
	public final void testIntersectsRegionNegativeAdjacentYNonAdjacentX() {
		final Region r = new Region(0, 2, 3, 5);
		Assert.assertFalse(r.intersects(new Region(5, 6, 1, 2)));
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.SpatialMap.Region#intersects(se.kth.speech.SpatialMap.Region)}.
	 */
	@Test
	public final void testIntersectsRegionNegativeAdjacentYSameX() {
		final Region r = new Region(0, 2, 3, 5);
		Assert.assertFalse(r.intersects(new Region(0, 2, 1, 2)));
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.SpatialMap.Region#intersects(se.kth.speech.SpatialMap.Region)}.
	 */
	@Theory
	public final void testIntersectsRegionSelf(final Region r) {
		Assert.assertTrue(r.intersects(r));
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.SpatialMap.Region#intersects(se.kth.speech.SpatialMap.Region)}.
	 */
	@Test
	public final void testIntersectsRegionSubregion() {
		final Region r = new Region(0, 2, 3, 5);
		Assert.assertTrue(r.intersects(new Region(0, 1, 3, 4)));
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
	 * {@link se.kth.speech.SpatialMap.Region#subsumes(int,int,int,int,int,int,int,int)}.
	 */
	@Theory
	public final void testSubsumesIntIntIntIntIntIntIntIntCommutativty(final int[] pointArray) {
		final int r1x1 = pointArray[0];
		final int r1x2 = pointArray[1];
		final int r1y1 = pointArray[2];
		final int r1y2 = pointArray[3];
		final int r2x1 = pointArray[4];
		final int r2x2 = pointArray[5];
		final int r2y1 = pointArray[6];
		final int r2y2 = pointArray[7];
		final boolean orig = SpatialMap.Region.subsumes(r1x1, r1x2, r1y1, r1y2, r2x1, r2x2, r2y1, r2y2);
		final boolean inverse = SpatialMap.Region.subsumes(r2x1, r2x2, r2y1, r2y2, r1x1, r1x2, r1y1, r1y2);
		Assert.assertEquals(orig, inverse);
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.SpatialMap.Region#subsumes(se.kth.speech.SpatialMap.Region)}.
	 */
	@Test
	public final void testSubsumesRegionNegative() {
		final Region superRegion = new Region(3, 6, 4, 6);
		final Region subRegion = new Region(3, 4, 5, 6);
		Assert.assertFalse(subRegion.subsumes(superRegion));
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.SpatialMap.Region#subsumes(se.kth.speech.SpatialMap.Region)}.
	 */
	@Test
	public final void testSubsumesRegionNegativeIntersecting() {
		final Region r = new Region(0, 2, 0, 1);
		Assert.assertFalse(r.subsumes(new Region(0, 3, 1, 3)));
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.SpatialMap.Region#subsumes(se.kth.speech.SpatialMap.Region)}.
	 */
	@Test
	public final void testSubsumesRegionPositive() {
		final Region superRegion = new Region(3, 6, 4, 6);
		final Region subRegion = new Region(3, 4, 5, 6);
		Assert.assertTrue(superRegion.subsumes(subRegion));
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.SpatialMap.Region#subsumes(se.kth.speech.SpatialMap.Region)}.
	 */
	@Theory
	public final void testSubsumesRegionSelf(final Region r) {
		Assert.assertTrue(r.subsumes(r));
	}

}
