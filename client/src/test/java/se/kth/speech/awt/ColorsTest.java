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
package se.kth.speech.awt;

import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.function.Function;

import org.junit.Assert;
import org.junit.Test;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 23 Mar 2017
 *
 */
public final class ColorsTest {

	/**
	 * Test method for
	 * {@link se.kth.speech.awt.Colors#createRGBColorNameMap(java.util.function.Function, java.util.function.Supplier)}.
	 */
	@Test
	public void testCreateColorNameMap() {
		final Map<Integer, Set<String>> result = Colors.createRGBColorNameMap(Function.identity(), HashSet::new);
		Assert.assertFalse(result.isEmpty());
		for (final Entry<Integer, Set<String>> rgbNames : result.entrySet()) {
			final Set<String> names = rgbNames.getValue();
			Assert.assertFalse(names.isEmpty());
		}
	}

}
