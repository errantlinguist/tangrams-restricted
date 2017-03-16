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

import java.util.HashMap;
import java.util.Set;

import org.junit.Assert;
import org.junit.Test;

import com.google.common.collect.Sets;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 16 Mar 2017
 *
 */
public final class MinMaxValueTrackingMapTest {

	/**
	 * Test method for
	 * {@link se.kth.speech.MinMaxValueTrackingMap#getMaxKeys()}.
	 */
	@Test
	public final void testGetMaxKeys() {
		final MinMaxValueTrackingMap<Object, Integer> m = MinMaxValueTrackingMap.create(new HashMap<>());
		final Set<Object> expected1 = Sets.newHashSet("three", "tres", "drei");
		expected1.stream().forEach(e -> m.put(e, 3));
		m.put("one", 1);
		m.put("two", 2);
		m.put("dos", 2);
		m.put("minus one", -1);
		Assert.assertEquals(expected1, m.getMaxKeys());
		final Set<Object> expected2 = Sets.newHashSet("eight", "ocho", "acht", "åtta");
		expected2.stream().forEach(e -> m.put(e, 8));
		Assert.assertEquals(expected2, m.getMaxKeys());
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.MinMaxValueTrackingMap#getMaxValue()}.
	 */
	@Test
	public final void testGetMaxValue() {
		final MinMaxValueTrackingMap<Object, Integer> m = MinMaxValueTrackingMap.create(new HashMap<>());
		final Integer expected1 = 3;
		Sets.newHashSet("three", "tres", "drei").stream().forEach(e -> m.put(e, expected1));
		m.put("one", 1);
		m.put("two", 2);
		m.put("dos", 2);
		m.put("minus one", -1);
		Assert.assertEquals(expected1, m.getMaxValue());
		final Integer expected2 = 8;
		Sets.newHashSet("eight", "ocho", "acht", "åtta").stream().forEach(e -> m.put(e, expected2));
		Assert.assertEquals(expected2, m.getMaxValue());
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.MinMaxValueTrackingMap#getMinKeys()}.
	 */
	@Test
	public final void testGetMinKeys() {
		final MinMaxValueTrackingMap<Object, Integer> m = MinMaxValueTrackingMap.create(new HashMap<>());
		Sets.newHashSet("one", "uno", "eins", "ett", "odin").stream().forEach(e -> m.put(e, 1));
		m.put("two", 2);
		m.put("dos", 2);
		final Set<Object> expected1 = Sets.newHashSet("minus one", "minus eins");
		expected1.stream().forEach(e -> m.put(e, -1));
		Assert.assertEquals(expected1, m.getMinKeys());
		final Set<Object> expected2 = Sets.newHashSet("minus three", "minus drei");
		expected2.stream().forEach(e -> m.put(e, -3));
		Assert.assertEquals(expected2, m.getMinKeys());
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.MinMaxValueTrackingMap#getMinValue()}.
	 */
	@Test
	public final void testGetMinValue() {
		final MinMaxValueTrackingMap<Object, Integer> m = MinMaxValueTrackingMap.create(new HashMap<>());
		Sets.newHashSet("one", "uno", "eins", "ett", "odin").stream().forEach(e -> m.put(e, 1));
		m.put("two", 2);
		m.put("dos", 2);
		final Integer expected1 = -1;
		Sets.newHashSet("minus one", "minus eins").stream().forEach(e -> m.put(e, expected1));
		Assert.assertEquals(expected1, m.getMinValue());
		final Integer expected2 = -3;
		Sets.newHashSet("minus three", "minus drei").stream().forEach(e -> m.put(e, expected2));
		Assert.assertEquals(expected2, m.getMinValue());
	}

}
