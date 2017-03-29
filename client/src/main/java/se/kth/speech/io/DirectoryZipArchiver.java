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
package se.kth.speech.coin.tangrams.io;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.UncheckedIOException;
import java.nio.file.FileVisitOption;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.BasicFileAttributeView;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.concurrent.ExecutorService;
import java.util.function.BiConsumer;
import java.util.stream.Stream;
import java.util.zip.Adler32;
import java.util.zip.CheckedOutputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 29 Mar 2017
 * @see <a href=
 *      "http://www.oracle.com/technetwork/articles/java/compress-1565076.html">Oracle
 *      Technology Network</a>
 *
 */
public final class SessionLogDirZipArchiver implements BiConsumer<Path, Path> {

	private static final int BUFFER_SIZE = 2048;

	private static final Logger LOGGER = LoggerFactory.getLogger(SessionLogDirZipArchiver.class);

	private final ExecutorService executorService;

	public SessionLogDirZipArchiver(final ExecutorService executorService) {
		this.executorService = executorService;
	}

	@Override
	public void accept(final Path indir, final Path outfile) throws UncheckedIOException {
		executorService.submit(() -> {
			final byte[] buffer = new byte[2048];

			try (OutputStream os = Files.newOutputStream(outfile, StandardOpenOption.CREATE_NEW,
					StandardOpenOption.WRITE)) {
				final CheckedOutputStream checksum = new CheckedOutputStream(os, new Adler32());
				final ZipOutputStream out = new ZipOutputStream(checksum);
				try (Stream<Path> filePaths = Files.walk(indir, FileVisitOption.FOLLOW_LINKS)) {
					filePaths.filter(Files::isRegularFile).forEach(filePath -> {
						final Path relIndir = indir.relativize(filePath);
						final String entryName = relIndir.toString();
						final ZipEntry entry = new ZipEntry(entryName);
						entry.setMethod(ZipEntry.DEFLATED);
						try {
							out.putNextEntry(entry);
							entry.setSize(Files.size(filePath));
							// Write metadata
							final BasicFileAttributeView attrView = Files.getFileAttributeView(filePath,
									BasicFileAttributeView.class);
							if (attrView == null) {
								LOGGER.debug(
										"No file attributes found for file at path \"{}\"; Zipped entry won't contain all attributes for it.",
										filePath);
								entry.setComment("File system attributes not available.");
								entry.setLastModifiedTime(Files.getLastModifiedTime(filePath));
							} else {
								final BasicFileAttributes attrs = attrView.readAttributes();
								entry.setCreationTime(attrs.creationTime());
								entry.setLastAccessTime(attrs.lastAccessTime());
								entry.setLastModifiedTime(attrs.lastModifiedTime());
							}
							// Write file content
							try (BufferedInputStream instream = new BufferedInputStream(Files.newInputStream(filePath),
									BUFFER_SIZE)) {
								int count;
								while ((count = instream.read(buffer, 0, BUFFER_SIZE)) != -1) {
									out.write(buffer, 0, count);
								}
								// FIXME: The last file in the archive has a
								// size of "0" and is unreadable
							}
						} catch (final IOException e) {
							throw new UncheckedIOException(e);
						}
					});
				} finally {
					out.flush();
				}
			} catch (final IOException e) {
				throw new UncheckedIOException(e);
			}
		});
	}
}
