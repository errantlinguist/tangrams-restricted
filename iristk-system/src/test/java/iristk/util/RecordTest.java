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
package iristk.util;

import java.util.Map;

import org.junit.Assert;
import org.junit.Test;

/**
 * Created to investigate a case of {@link java.lang.NoClassDefFoundError} for
 * subclasses of {@link iristk.util.Record}.
 *
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since Jul 5, 2017
 *
 */
public final class RecordTest {

	private static class TestRecord extends Record {

		private String testString;

		public TestRecord() {
			super();
		}

		@SuppressWarnings("unused")
		public TestRecord(final Map<?, ?> map) {
			super(map);
		}

		@SuppressWarnings("unused")
		public TestRecord(final Object... init) {
			super(init);
		}

		@SuppressWarnings("unused")
		public TestRecord(final Record initRecord, final Object... init) {
			super(initRecord, init);
		}

		@RecordField(name = "testString")
		public String getTestString() {
			return testString;
		}

		@RecordField(name = "testString")
		public void setTestString(final String testString) {
			this.testString = testString;
		}
	}

//	private static final Logger LOGGER = LoggerFactory.getLogger(RecordTest.class);

	/**
	 * Test method for {@link iristk.util.Record#Record()}.
	 */
	@Test
	public void testRecord() {
		final Record testInst = new Record();
		Assert.assertNotNull(testInst);
//		LOGGER.debug("Successfully instantiated {}.", testInst);
	}

	/**
	 * Test method for {@link iristk.util.Record#Record()}.
	 */
	@Test
	public void testRecordSubclass() {
		final TestRecord testInst = new TestRecord();
		Assert.assertNotNull(testInst);
//		LOGGER.debug("Successfully instantiated {}.", testInst);
	}

}
