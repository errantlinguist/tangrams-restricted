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
package se.kth.speech.matrix;

import java.util.Random;
import java.util.Set;

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

import se.kth.speech.IntArrays;
import se.kth.speech.RandomCollections;
import se.kth.speech.matrix.SpatialRegion;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 9 Mar 2017
 *
 */
@RunWith(Theories.class)
public final class SpatialRegionTest {

	@DataPoints("previousFailedRegionPointArrays")
	public static final int[][] PREVIOUS_FAILED_REGION_POINT_ARRAYS;

	@DataPoints("previousFailedRegions")
	public static final Set<SpatialRegion> PREVIOUS_FAILED_REGIONS;

	@DataPoints("pointArrays")
	public static final Set<int[]> TEST_POINT_ARRAYS;

	@DataPoints("points")
	public static final int[] TEST_POINTS;

	@DataPoints("regions")
	public static final Set<SpatialRegion> TEST_REGIONS;

	static {
		PREVIOUS_FAILED_REGION_POINT_ARRAYS = new int[][] {
				new int[] { 1989523352, 2023725473, 1079495603, 1079495604 },
				new int[] { -367670549, 1742426776, -582564738, 1954507817 },
				new int[] { 1661512450, 1661512451, -1470324477, 1742426776 } };
		PREVIOUS_FAILED_REGIONS = Sets.newHashSetWithExpectedSize(PREVIOUS_FAILED_REGION_POINT_ARRAYS.length);
		for (final int[] pointArray : PREVIOUS_FAILED_REGION_POINT_ARRAYS) {
			PREVIOUS_FAILED_REGIONS.add(new SpatialRegion(pointArray[0], pointArray[1], pointArray[2], pointArray[3]));
		}
	}

	static {
		final Random rnd = new Random();
		final int testPointCount = 10;
		TEST_POINTS = rnd.ints().distinct().limit(testPointCount).toArray();

		final int testRegionCount = testPointCount / 4;
		TEST_REGIONS = Sets.newHashSetWithExpectedSize(testRegionCount);
		TEST_POINT_ARRAYS = Sets.newHashSetWithExpectedSize(testRegionCount);
		do {
			final int[] x = createRandomIntervalArray(rnd, TEST_POINTS);
			final int[] y = createRandomIntervalArray(rnd, TEST_POINTS);
			TEST_REGIONS.add(new SpatialRegion(x[0], x[1], y[0], y[1]));
			TEST_POINT_ARRAYS.add(IntArrays.concatenate(x, y));
		} while (TEST_REGIONS.size() < testRegionCount);
	}

	private static int[] createRandomIntervalArray(final Random rnd, final int[] points) {
		final int first = RandomCollections.getRandomElement(points, rnd);
		int second = RandomCollections.getRandomElement(points, rnd);
		while (first == second) {
			second = RandomCollections.getRandomElement(points, rnd);
		}
		return first < second ? new int[] { first, second } : new int[] { second, first };
	}

	@Rule
	public final ExpectedException exception = ExpectedException.none();

	@Theory
	public final void testGetDimensionsFactorOfGridArea(final SpatialRegion r) {
		final int[] dims = r.getDimensions();
		final int prod = IntArrays.product(dims);
		Assert.assertEquals(r.getGridArea(), prod);
	}

	@Test
	public final void testGetGridArea1() {
		final SpatialRegion r = new SpatialRegion(0, 1, 1, 2);
		Assert.assertEquals(1, r.getGridArea());
	}

	@Test
	public final void testGetGridArea2() {
		final SpatialRegion r = new SpatialRegion(3, 5, 6, 8);
		Assert.assertEquals(4, r.getGridArea());
	}

	@Test
	public final void testGetGridArea3() {
		final SpatialRegion r = new SpatialRegion(0, 1, 0, 1);
		Assert.assertEquals(1, r.getGridArea());
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.matrix.SpatialRegion#intersects(int,int,int,int,int,int,int,int)}.
	 */
	@Theory
	public final void testIntersectsIntIntIntIntIntIntIntIntCommutativty(final int[] r1, final int[] r2) {
		final int r1x1 = r1[0];
		final int r1x2 = r1[1];
		final int r1y1 = r1[2];
		final int r1y2 = r1[3];
		final int r2x1 = r2[0];
		final int r2x2 = r2[2];
		final int r2y1 = r2[2];
		final int r2y2 = r2[3];
		final boolean orig = SpatialRegion.intersects(r1x1, r1x2, r1y1, r1y2, r2x1, r2x2, r2y1, r2y2);
		final boolean inverse = SpatialRegion.intersects(r2x1, r2x2, r2y1, r2y2, r1x1, r1x2, r1y1, r1y2);
		Assert.assertEquals(orig, inverse);
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.matrix.SpatialRegion#intersects(se.kth.speech.SpatialMap.SpatialRegion)}.
	 */
	@Theory
	public final void testIntersectsRegionCommutativity(final SpatialRegion r1, final SpatialRegion r2) {
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
	 * {@link se.kth.speech.matrix.SpatialRegion#intersects(se.kth.speech.SpatialMap.SpatialRegion)}.
	 */
	@Theory
	public final void testIntersectsRegionEqual(final SpatialRegion r1, final SpatialRegion r2) {
		Assume.assumeTrue(r1.equals(r2));
		Assert.assertTrue(r1.intersects(r2));
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.matrix.SpatialRegion#intersects(se.kth.speech.SpatialMap.SpatialRegion)}.
	 */
	@Test
	public final void testIntersectsRegionIntersecting() {
		final SpatialRegion r = new SpatialRegion(0, 2, 3, 5);
		Assert.assertTrue(r.intersects(new SpatialRegion(0, 2, 2, 5)));
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.matrix.SpatialRegion#intersects(se.kth.speech.SpatialMap.SpatialRegion)}.
	 */
	@Test
	public final void testIntersectsRegionNegative() {
		final SpatialRegion r = new SpatialRegion(0, 2, 3, 5);
		Assert.assertFalse(r.intersects(new SpatialRegion(5, 6, 6, 7)));
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.matrix.SpatialRegion#intersects(se.kth.speech.SpatialMap.SpatialRegion)}.
	 */
	@Test
	public final void testIntersectsRegionNegativeAdjacentXNonAdjacentY() {
		final SpatialRegion r = new SpatialRegion(3, 5, 0, 2);
		Assert.assertFalse(r.intersects(new SpatialRegion(1, 2, 5, 6)));
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.matrix.SpatialRegion#intersects(se.kth.speech.SpatialMap.SpatialRegion)}.
	 */
	@Test
	public final void testIntersectsRegionNegativeAdjacentXSameY() {
		final SpatialRegion r = new SpatialRegion(3, 5, 0, 2);
		Assert.assertFalse(r.intersects(new SpatialRegion(1, 2, 0, 2)));
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.matrix.SpatialRegion#intersects(se.kth.speech.SpatialMap.SpatialRegion)}.
	 */
	@Test
	public final void testIntersectsRegionNegativeAdjacentYNonAdjacentX() {
		final SpatialRegion r = new SpatialRegion(0, 2, 3, 5);
		Assert.assertFalse(r.intersects(new SpatialRegion(5, 6, 1, 2)));
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.matrix.SpatialRegion#intersects(se.kth.speech.SpatialMap.SpatialRegion)}.
	 */
	@Test
	public final void testIntersectsRegionNegativeAdjacentYSameX() {
		final SpatialRegion r = new SpatialRegion(0, 2, 3, 5);
		Assert.assertFalse(r.intersects(new SpatialRegion(0, 2, 1, 2)));
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.matrix.SpatialRegion#intersects(se.kth.speech.SpatialMap.SpatialRegion)}.
	 */
	@Test
	public final void testIntersectsRegionNegativeRegression1(){
		final SpatialRegion r1 = new SpatialRegion(0, 5, 0, 5);
		final SpatialRegion r2 = new SpatialRegion(0, 5, 5, 10);
		Assert.assertFalse(r1.intersects(r2));
		Assert.assertFalse(r2.intersects(r1));
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.matrix.SpatialRegion#intersects(se.kth.speech.SpatialMap.SpatialRegion)}.
	 */
	@Test
	public final void testIntersectsRegionNotSubregion() {
		final SpatialRegion r1 = new SpatialRegion(0, 9, 0, 2);
		final SpatialRegion r2 = new SpatialRegion(8, 12, 0, 3);
		Assert.assertFalse(r1.subsumes(r2));
		Assert.assertFalse(r2.subsumes(r1));
		Assert.assertTrue(r1.intersects(r2));
		Assert.assertTrue(r2.intersects(r1));
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.matrix.SpatialRegion#intersects(se.kth.speech.SpatialMap.SpatialRegion)}.
	 */
	@Theory
	public final void testIntersectsRegionSelf(final SpatialRegion r) {
		Assert.assertTrue(r.intersects(r));
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.matrix.SpatialRegion#intersects(se.kth.speech.SpatialMap.SpatialRegion)}.
	 */
	@Test
	public final void testIntersectsRegionSubregion() {
		final SpatialRegion superRegion = new SpatialRegion(0, 2, 3, 5);
		final SpatialRegion subRegion = new SpatialRegion(0, 1, 3, 4);
		Assert.assertTrue(superRegion.subsumes(subRegion));
		Assert.assertTrue(superRegion.intersects(subRegion));
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.matrix.SpatialRegion#Region(int, int, int, int)}.
	 */
	@Theory
	public final void testRegionNegative(final int xLowerBound, final int xUpperBound, final int yLowerBound,
			final int yUpperBound) {
		Assume.assumeFalse("Lower bound of x less than or equal to upper bound.", xLowerBound <= xUpperBound);
		Assume.assumeFalse("Lower bound of y less than or equal to upper bound.", yLowerBound <= yUpperBound);
		exception.expect(IllegalArgumentException.class);
		new SpatialRegion(xLowerBound, xUpperBound, yLowerBound, yUpperBound);
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.matrix.SpatialRegion#Region(int, int, int, int)}.
	 */
	@Theory
	public final void testRegionPositive(final int xLowerBound, final int xUpperBound, final int yLowerBound,
			final int yUpperBound) {
		Assume.assumeTrue("Lower bound of x not less than upper bound.", xLowerBound < xUpperBound);
		Assume.assumeTrue("Lower bound of y not less than upper bound.", yLowerBound < yUpperBound);
		new SpatialRegion(xLowerBound, xUpperBound, yLowerBound, yUpperBound);
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.matrix.SpatialRegion#subsumes(int,int,int,int,int,int,int,int)}.
	 */
	@Theory
	public final void testSubsumesIntIntIntIntIntIntIntIntEqual(final int[] r1, final int[] r2) {
		final int r1x1 = r1[0];
		final int r1x2 = r1[1];
		final int r1y1 = r1[2];
		final int r1y2 = r1[3];
		final int r2x1 = r2[0];
		Assume.assumeTrue(r1x1 == r2x1);
		final int r2x2 = r2[1];
		Assume.assumeTrue(r1x2 == r2x2);
		final int r2y1 = r2[2];
		Assume.assumeTrue(r1y1 == r2y1);
		final int r2y2 = r2[3];
		Assume.assumeTrue(r1y2 == r2y2);
		final boolean orig = SpatialRegion.subsumes(r1x1, r1x2, r1y1, r1y2, r2x1, r2x2, r2y1, r2y2);
		Assert.assertTrue(orig);
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.matrix.SpatialRegion#subsumes(se.kth.speech.SpatialMap.SpatialRegion)}.
	 */
	@Theory
	public final void testSubsumesRegionEqual(final SpatialRegion r1, final SpatialRegion r2) {
		Assume.assumeTrue(r1.equals(r2));
		Assert.assertTrue(r1.subsumes(r2));
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.matrix.SpatialRegion#subsumes(se.kth.speech.SpatialMap.SpatialRegion)}.
	 */
	@Test
	public final void testSubsumesRegionNegative() {
		final SpatialRegion superRegion = new SpatialRegion(3, 6, 4, 6);
		final SpatialRegion subRegion = new SpatialRegion(3, 4, 5, 6);
		Assert.assertFalse(subRegion.subsumes(superRegion));
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.matrix.SpatialRegion#subsumes(se.kth.speech.SpatialMap.SpatialRegion)}.
	 */
	@Test
	public final void testSubsumesRegionNegativeIntersecting() {
		final SpatialRegion r1 = new SpatialRegion(0, 2, 0, 2);
		final SpatialRegion r2 = new SpatialRegion(0, 3, 1, 3);
		Assert.assertTrue(r1.intersects(r2));
		Assert.assertFalse(r1.subsumes(r2));
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.matrix.SpatialRegion#subsumes(se.kth.speech.SpatialMap.SpatialRegion)}.
	 */
	@Test
	public final void testSubsumesRegionPositive1() {
		final SpatialRegion superRegion = new SpatialRegion(3, 6, 4, 6);
		final SpatialRegion subRegion = new SpatialRegion(3, 4, 5, 6);
		Assert.assertTrue(superRegion.subsumes(subRegion));
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.matrix.SpatialRegion#subsumes(se.kth.speech.SpatialMap.SpatialRegion)}.
	 */
	@Test
	public final void testSubsumesRegionPositive2() {
		final SpatialRegion superRegion = new SpatialRegion(0, 2, 3, 5);
		final SpatialRegion subRegion = new SpatialRegion(0, 1, 3, 4);
		Assert.assertTrue(superRegion.subsumes(subRegion));
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.matrix.SpatialRegion#subsumes(se.kth.speech.SpatialMap.SpatialRegion)}.
	 */
	@Theory
	public final void testSubsumesRegionSelf(final SpatialRegion r) {
		Assert.assertTrue(r.subsumes(r));
	}

}
