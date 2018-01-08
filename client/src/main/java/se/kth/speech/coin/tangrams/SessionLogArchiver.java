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
package se.kth.speech.coin.tangrams;

import java.io.PrintStream;
import java.nio.file.Path;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.Locale;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.io.DirectoryZipArchiver;
import se.kth.speech.io.FileNames;

/**
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since 3 Apr 2017
 *
 */
final class SessionLogArchiver implements Supplier<Path> {

	private static final Logger LOGGER = LoggerFactory.getLogger(SessionLogArchiver.class);

	private final DirectoryZipArchiver archiver;

	private final Locale filenameLocale;

	private final PrintStream msgOutput;

	private final String playerId;

	private final Path rootLogDirPath;

	private final Date systemLoggingStartTime;

	private final Supplier<? extends Path> timestampedLogDirPathSupplier;

	SessionLogArchiver(final Path rootLogDirPath, final Date systemLoggingStartTime,
			final Supplier<? extends Path> timestampedLogDirPathSupplier, final String playerId,
			final PrintStream msgOutput, final Locale filenameLocale) {
		this.rootLogDirPath = rootLogDirPath;
		this.systemLoggingStartTime = systemLoggingStartTime;
		this.timestampedLogDirPathSupplier = timestampedLogDirPathSupplier;
		this.playerId = playerId;
		this.msgOutput = msgOutput;
		this.filenameLocale = filenameLocale;
		final Function<Path, String> playerIdAppender = relSourceFilePath -> {
			final Path filenamePath = relSourceFilePath.getFileName();
			final String[] filenameParts = FileNames.splitBase(filenamePath.toString());

			String result;
			final int filenameBaseIdx = 0;
			if (filenameParts[filenameBaseIdx].endsWith(playerId)) {
				// Do nothing
				result = relSourceFilePath.toString();
			} else {
				filenameParts[filenameBaseIdx] = filenameParts[filenameBaseIdx] + '-' + playerId;
				result = Arrays.stream(filenameParts).collect(Collectors.joining("."));
			}
			return result;
		};
		archiver = new DirectoryZipArchiver(playerIdAppender);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Supplier#get()
	 */
	@Override
	public Path get() {
		final String archiveFilename = new SimpleDateFormat("yyyyMMdd-HHmm", filenameLocale)
				.format(systemLoggingStartTime) + "-" + playerId + ".zip";
		final Path result = rootLogDirPath.resolve(archiveFilename);
		msgOutput.println(String.format("Archiving session logs to \"%s\"...", result));
		LOGGER.info("Archiving session logs to \"{}\"...", result);
		archiver.accept(timestampedLogDirPathSupplier.get(), result);
		LOGGER.info("Finished archiving session logs to \"{}\".", result);
		msgOutput.println(String.format("Finished archiving session logs to \"%s\".", result));
		return result;
	}

}
