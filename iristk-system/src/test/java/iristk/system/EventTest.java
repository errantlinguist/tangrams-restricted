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
package iristk.system;

import org.junit.Assert;
import org.junit.Test;

import iristk.util.Record;

/**
 * Created to investigate a case of {@link java.lang.NoClassDefFoundError} for
 * subclasses of {@link iristk.system.Event}.
 *
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since Jul 5, 2017
 *
 */
public final class EventTest {

	private static class TestEvent extends Event {

		private String testString;

		public TestEvent() {
			super();
		}

		@SuppressWarnings("unused")
		public TestEvent(final Event event) {
			super(event);
		}

		@SuppressWarnings("unused")
		public TestEvent(final String name) {
			super(name);
		}

		@SuppressWarnings("unused")
		public TestEvent(final String name, final Object... params) {
			super(name, params);
		}

		@SuppressWarnings("unused")
		public TestEvent(final String name, final Record parameters) {
			super(name, parameters);
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

	// private static final Logger LOGGER =
	// LoggerFactory.getLogger(EventTest.class);

	/**
	 * Test method for {@link iristk.system.Event#Event()}.
	 */
	@Test
	public void testEvent() {
		final Event testInst = new Event();
		Assert.assertNotNull(testInst);
		// LOGGER.debug("Successfully instantiated {}.", testInst);
	}

	/**
	 * Test method for {@link iristk.system.Event#Event(String))}.
	 */
	@Test
	public void testEventString() {
		final String expectedName = "unittest";
		final Event testInst = new Event(expectedName);
		// LOGGER.debug("Successfully instantiated {}.", testInst);
		Assert.assertEquals(expectedName, testInst.getName());
	}

	/**
	 * Test method for {@link iristk.system.Event#Event()}.
	 */
	@Test
	public void testEventSubclass() {
		final TestEvent testInst = new TestEvent();
		Assert.assertNotNull(testInst);
		// LOGGER.debug("Successfully instantiated {}.", testInst);
	}

}
