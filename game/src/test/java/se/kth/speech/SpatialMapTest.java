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
import org.junit.Assume;
import org.junit.Test;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 9 Mar 2017
 *
 */
public final class SpatialMapTest {

	private static <T> void assertPutNewElement(final SpatialMap<? super T> testMap, final T elem,
			final SpatialRegion r) {
		Assume.assumeFalse(testMap.isOccupied(r));
		final SpatialRegion oldRegion = testMap.put(elem, r);
		Assert.assertNull(oldRegion);
		Assert.assertTrue(testMap.getAllElements().contains(elem));
		Assert.assertTrue(testMap.isOccupied(r));
		Assert.assertTrue(testMap.getMinimalRegions().contains(r));
		Assert.assertTrue(testMap.getMinimalRegionElements().get(r).contains(elem));
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.SpatialMap#isOccupied(se.kth.speech.SpatialMap.SpatialRegion)}.
	 */
	@Test
	public void testIsOccupiedIntersecting() {
		final SpatialMap<Object> testMap = SpatialMap.create(2);
		final SpatialRegion r1 = new SpatialRegion(0, 9, 0, 2);
		Assert.assertNull(testMap.put("r1", r1));
		final SpatialRegion r2 = new SpatialRegion(8, 12, 0, 3);
		Assert.assertFalse(r1.subsumes(r2));
		Assert.assertFalse(r2.subsumes(r1));
		Assert.assertTrue(testMap.isOccupied(r2));
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.SpatialMap#isOccupied(se.kth.speech.SpatialMap.SpatialRegion)}.
	 */
	@Test
	public void testIsOccupiedNegative() {
		final SpatialMap<Object> testMap = SpatialMap.create(2);
		final SpatialRegion superRegion = new SpatialRegion(0, 1, 2, 3);
		Assert.assertNull(testMap.put("region", superRegion));
		final SpatialRegion subRegion = new SpatialRegion(0, 1, 4, 5);
		Assert.assertFalse(testMap.isOccupied(subRegion));
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.SpatialMap#isOccupied(se.kth.speech.SpatialMap.SpatialRegion)}.
	 */
	@Test
	public void testIsOccupiedSubregion() {
		final SpatialMap<Object> testMap = SpatialMap.create(2);
		final SpatialRegion superRegion = new SpatialRegion(0, 20, 10, 30);
		testMap.put("superRegion", superRegion);
		final SpatialRegion subRegion = new SpatialRegion(5, 6, 15, 26);
		Assert.assertTrue(testMap.isOccupied(subRegion));
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.SpatialMap#put(Object, se.kth.speech.SpatialMap.SpatialRegion)}
	 */
	@Test
	public void testPutExistingElement() {
		final SpatialMap<Object> testMap = SpatialMap.create(2);
		final SpatialRegion r1 = new SpatialRegion(0, 1, 2, 3);
		final String elem = "foo";
		final SpatialRegion oldRegion1 = testMap.put(elem, r1);
		Assert.assertNull(oldRegion1);
		final SpatialRegion r2 = new SpatialRegion(20, 21, 22, 23);
		final SpatialRegion oldRegion2 = testMap.put(elem, r2);
		Assert.assertNotNull(oldRegion2);
		Assert.assertTrue(testMap.getAllElements().contains(elem));
		Assert.assertFalse(testMap.isOccupied(r1));
		Assert.assertTrue(testMap.isOccupied(r2));
		Assert.assertFalse(testMap.getMinimalRegions().contains(r1));
		Assert.assertTrue(testMap.getMinimalRegions().contains(r2));
		Assert.assertFalse(testMap.getMinimalRegionElements().get(r1).contains(elem));
		Assert.assertTrue(testMap.getMinimalRegionElements().get(r2).contains(elem));
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.SpatialMap#put(Object, se.kth.speech.SpatialMap.SpatialRegion)}
	 */
	@Test
	public void testPutNewElement() {
		final SpatialMap<Object> testMap = SpatialMap.create(2);
		assertPutNewElement(testMap, "foo", new SpatialRegion(0, 1, 2, 3));
		assertPutNewElement(testMap, new int[] { 3, 3 }, new SpatialRegion(2, 3, 4, 5));
	}

}
