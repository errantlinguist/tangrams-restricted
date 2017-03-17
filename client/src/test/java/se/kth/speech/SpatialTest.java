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

import org.junit.experimental.theories.DataPoints;

import com.google.common.collect.Sets;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 9 Mar 2017
 *
 */
public abstract class SpatialTest {

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
				new int[] { 1989523352, 2023725473, 1079495603, 1079495603 },
				new int[] { -367670549, 1742426776, -582564738, 1954507817 },
				new int[] { 1661512450, 1661512450, -1470324477, 1742426776 } };
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
		final int second = RandomCollections.getRandomElement(points, rnd);
		return first <= second ? new int[] { first, second } : new int[] { second, first };
	}

}
