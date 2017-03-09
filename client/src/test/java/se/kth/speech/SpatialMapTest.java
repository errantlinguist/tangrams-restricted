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

import org.junit.Assert;
import org.junit.Test;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 9 Mar 2017
 *
 */
public final class SpatialMapTest {

	/**
	 * Test method for
	 * {@link se.kth.speech.SpatialMap#isOccupied(se.kth.speech.SpatialMap.Region)}.
	 */
	@Test
	public final void testIsOccupiedIntersecting() {
		final SpatialMap<Object> testMap = new SpatialMap<>(2);
		final SpatialMap.Region superRegion = new SpatialMap.Region(0, 20, 10, 30);
		testMap.put(superRegion, "superRegion");
		final SpatialMap.Region subRegion = new SpatialMap.Region(18, 22, 0, 5);
		Assert.assertTrue(testMap.isOccupied(subRegion));
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.SpatialMap#isOccupied(se.kth.speech.SpatialMap.Region)}.
	 */
	@Test
	public final void testIsOccupiedNegative() {
		final SpatialMap<Object> testMap = new SpatialMap<>(2);
		final SpatialMap.Region superRegion = new SpatialMap.Region(0, 20, 10, 30);
		testMap.put(superRegion, "superRegion");
		final SpatialMap.Region subRegion = new SpatialMap.Region(0, 20, 0, 9);
		Assert.assertFalse(testMap.isOccupied(subRegion));
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.SpatialMap#isOccupied(se.kth.speech.SpatialMap.Region)}.
	 */
	@Test
	public final void testIsOccupiedSubregion() {
		final SpatialMap<Object> testMap = new SpatialMap<>(2);
		final SpatialMap.Region superRegion = new SpatialMap.Region(0, 20, 10, 30);
		testMap.put(superRegion, "superRegion");
		final SpatialMap.Region subRegion = new SpatialMap.Region(5, 5, 15, 25);
		Assert.assertTrue(testMap.isOccupied(subRegion));
	}

}
