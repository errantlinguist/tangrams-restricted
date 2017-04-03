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
package se.kth.speech.coin.tangrams;

import java.nio.file.Path;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.function.Supplier;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.io.DirectoryZipArchiver;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 3 Apr 2017
 *
 */
final class SessionLogArchiver implements Supplier<Path> {

	private static final Logger LOGGER = LoggerFactory.getLogger(SessionLogArchiver.class);

	private final String playerId;

	private final Path rootLogDirPath;

	private final Date systemLoggingStartTime;

	private final Supplier<? extends Path> timestampedLogDirPathSupplier;

	SessionLogArchiver(final Path rootLogDirPath, final Date systemLoggingStartTime,
			final Supplier<? extends Path> timestampedLogDirPathSupplier, final String playerId) {
		this.rootLogDirPath = rootLogDirPath;
		this.systemLoggingStartTime = systemLoggingStartTime;
		this.timestampedLogDirPathSupplier = timestampedLogDirPathSupplier;
		this.playerId = playerId;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Supplier#get()
	 */
	@Override
	public Path get() {
		final String archiveFilename = new SimpleDateFormat("yyyyMMdd-HHmm").format(systemLoggingStartTime) + "-"
				+ playerId + ".zip";
		final Path result = rootLogDirPath.resolve(archiveFilename);
		System.out.println(String.format("Archiving session logs to \"%s\"...", result));
		LOGGER.info("Archiving session logs to \"{}\"...", result);
		new DirectoryZipArchiver().accept(timestampedLogDirPathSupplier.get(), result);
		LOGGER.info("Finished archiving session logs to \"{}\".", result);
		System.out.println(String.format("Finished archiving session logs to \"%s\".", result));
		return result;
	}

}
