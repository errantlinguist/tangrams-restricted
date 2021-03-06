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
package se.kth.speech.coin.tangrams.iristk.io;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Stream;

import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import iristk.system.Event;
import se.kth.speech.coin.tangrams.TestDataResources;

/**
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since Jul 5, 2017
 *
 */
public final class LoggedEventReaderTest {

	private static List<String> eventLines = Collections.emptyList();

	private static final Logger LOGGER = LoggerFactory.getLogger(LoggedEventReaderTest.class);

	@BeforeClass
	public static void initEventLines() throws IOException {
		eventLines = Arrays.asList(readEventLines().toArray(String[]::new));
		LOGGER.info("Read {} line(s) from test history log.", eventLines.size());
	}

	private static Stream<String> readEventLines() throws IOException {
		final String singleMoveSessionDataResLocStr = TestDataResources.SESSION_DATA_DIR
				+ "/karey-tangram_Jutta-ONEMOVE";
		final URL eventLogUrl = TestDataResources.class
				.getResource(singleMoveSessionDataResLocStr + "/events-karey.txt");
		LOGGER.info("Reading event history from \"{}\".", eventLogUrl);
		return new BufferedReader(new InputStreamReader(eventLogUrl.openStream())).lines();
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.iristk.io.LoggedEventReader#parseLoggedEvents(Stream)}.
	 *
	 */
	@Test
	public void testParseLoggedEvents() {
		final Event[] parsedEvents = LoggedEventReader.parseLoggedEvents(eventLines.stream()).toArray(Event[]::new);
		Assert.assertTrue(parsedEvents.length > 0);
		Assert.assertTrue(Arrays.stream(parsedEvents).allMatch(event -> {
			final Class<? extends Event> instClass = event.getClass();
			return Event.class.equals(instClass);
		}));
	}

}
