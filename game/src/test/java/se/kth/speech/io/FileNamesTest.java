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
package se.kth.speech.io;

import org.junit.Assert;
import org.junit.Test;

/**
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since 12 May 2017
 *
 */
public final class FileNamesTest {

	@Test
	public void testSanitizeNegative() {
		final String orig = "test_file.txt";
		final String actual = FileNames.sanitize(orig, "-");
		Assert.assertEquals(orig, actual);
	}

	@Test
	public void testSanitizePositive() {
		final String orig = "test*file|1.txt";
		final String expected = "test-file-1.txt";
		final String actual = FileNames.sanitize(orig, "-");
		Assert.assertEquals(expected, actual);
	}

}
