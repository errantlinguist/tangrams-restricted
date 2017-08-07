/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.analysis.
 *
 *  se.kth.speech.coin.tangrams-restricted.analysis is free software: you can redistribute it and/or modify
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
package se.kth.speech.coin.tangrams.analysis;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.FileVisitOption;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.stream.Stream;

import javax.xml.bind.JAXBException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.HashBasedTable;
import com.google.common.collect.Table;

import iristk.system.Event;
import se.kth.speech.coin.tangrams.iristk.io.LoggedEvents;

/**
 * A class used for re-writing log times based on when the events were written
 * to the log on the sender's local machine rather than when they were written
 * to the log on receiver's local machine.
 *
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 7 Aug 2017
 *
 */
final class SessionEventLogLocalTimeWriter {

	private static final int EXPECTED_AVERAGE_EVENT_COUNT = 120;

	private static final Logger LOGGER = LoggerFactory.getLogger(SessionEventLogLocalTimeWriter.class);

	public static void main(final String[] args) throws JAXBException, IOException {
		final Path[] inpaths = Arrays.stream(args).map(String::trim).filter(path -> !path.isEmpty()).map(Paths::get)
				.toArray(Path[]::new);
		if (inpaths.length < 1) {
			throw new IllegalArgumentException(
					String.format("Usage: %s INPATHS...", SessionEventLogLocalTimeWriter.class.getSimpleName()));
		} else {
			for (final Path inpath : inpaths) {
				LOGGER.info("Will read batch job data from \"{}\".", inpath);
				processSessions(inpath);
			}
		}

	}

	private static void processSessions(final Path inpath) throws JAXBException, IOException {
		final Path[] infilePaths = Files.walk(inpath, FileVisitOption.FOLLOW_LINKS).filter(Files::isRegularFile)
				.filter(filePath -> filePath.getFileName().toString().endsWith(".properties")).toArray(Path[]::new);
		for (final Path infilePath : infilePaths) {
			LOGGER.info("Reading batch job properties from \"{}\".", infilePath);
			final Properties props = new Properties();
			try (final InputStream propsInstream = Files.newInputStream(infilePath)) {
				props.load(propsInstream);
			}
			final SessionDataManager sessionData = SessionDataManager.create(infilePath);
			final Map<String, Path> playerEventLogs = sessionData.getPlayerData().getPlayerEventLogs();
			final Table<String, String, Event> playerEventsById = HashBasedTable.create(playerEventLogs.keySet().size(),
					EXPECTED_AVERAGE_EVENT_COUNT);
			for (final Entry<String, Path> playerEventLog : sessionData.getPlayerData().getPlayerEventLogs()
					.entrySet()) {
				final Path eventLogPath = playerEventLog.getValue();
				LOGGER.debug("Reading event log at \"{}\".", eventLogPath);
				try (Stream<Event> loggedEvents = LoggedEvents.readLoggedEvents(eventLogPath)) {
					// TODO: Finish
				}
			}
		}
	}

	private SessionEventLogLocalTimeWriter() {
	}

}
