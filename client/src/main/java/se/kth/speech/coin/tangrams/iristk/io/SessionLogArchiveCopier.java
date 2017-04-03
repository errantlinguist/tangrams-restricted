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
package se.kth.speech.coin.tangrams.iristk.io;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.function.Consumer;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class SessionLogArchiveCopier implements Consumer<Path> {

	private static final Logger LOGGER = LoggerFactory.getLogger(SessionLogArchiveCopier.class);

	private final Path copyDirPath;

	public SessionLogArchiveCopier(final Path copyDirPath) {
		this.copyDirPath = copyDirPath;
	}

	@Override
	public void accept(final Path filePath) {
		final Path filename = filePath.getFileName();
		final Path targetPath = copyDirPath.resolve(filename);
		System.out.println(String.format("Copying session log archive to \"%s\".", targetPath));
		LOGGER.info("Copying session log archive to \"{}\".", targetPath);
		try {
			final Path result = Files.copy(filePath, targetPath, StandardCopyOption.COPY_ATTRIBUTES);
			LOGGER.info("Finished copying session log archive to \"{}\".", result);
			System.out.println(String.format("Finished copying session log archive to \"%s\".", result));
		} catch (final IOException e) {
			throw new UncheckedIOException(e);
		}

	}

}