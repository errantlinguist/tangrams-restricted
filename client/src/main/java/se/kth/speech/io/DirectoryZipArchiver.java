/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.client.
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
package se.kth.speech.io;

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
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.stream.Stream;
import java.util.zip.Adler32;
import java.util.zip.CheckedOutputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since 29 Mar 2017
 * @see <a href=
 *      "http://www.oracle.com/technetwork/articles/java/compress-1565076.html">Oracle
 *      Technology Network</a>
 *
 */
public final class DirectoryZipArchiver implements BiConsumer<Path, Path> {

	private static final int BUFFER_SIZE = 8192;

	private static final Function<Path, String> DEFAULT_ENTRY_NAME_FACTORY = Path::toString;

	private static final Logger LOGGER = LoggerFactory.getLogger(DirectoryZipArchiver.class);

	private final Function<? super Path, String> zippedFileEntryNameFactory;

	public DirectoryZipArchiver() {
		this(DEFAULT_ENTRY_NAME_FACTORY);
	}

	public DirectoryZipArchiver(final Function<? super Path, String> zippedFileEntryNameFactory) {
		this.zippedFileEntryNameFactory = zippedFileEntryNameFactory;
	}

	@Override
	public void accept(final Path indir, final Path outfile) throws UncheckedIOException {
		final byte[] buffer = new byte[BUFFER_SIZE];

		try (OutputStream os = Files.newOutputStream(outfile, StandardOpenOption.CREATE_NEW,
				StandardOpenOption.WRITE)) {
			final CheckedOutputStream checksum = new CheckedOutputStream(os, new Adler32());
			try (final ZipOutputStream out = new ZipOutputStream(checksum)) {
				out.setMethod(ZipEntry.DEFLATED);
				try (Stream<Path> filePaths = Files.walk(indir, FileVisitOption.FOLLOW_LINKS)) {
					filePaths.filter(Files::isRegularFile).forEach(filePath -> {
						final Path relFilePath = indir.relativize(filePath);
						final String entryName = zippedFileEntryNameFactory.apply(relFilePath);
						final ZipEntry entry = new ZipEntry(entryName);
						try {
							out.putNextEntry(entry);
							// Write metadata
							final BasicFileAttributeView attrView = Files.getFileAttributeView(filePath,
									BasicFileAttributeView.class);
							if (attrView == null) {
								LOGGER.warn(
										"No file attributes found for file at path \"{}\"; Zipped entry might not have correct metadata.",
										filePath);
								entry.setComment("File attributes not available at time of archiving.");
							} else {
								final BasicFileAttributes attrs = attrView.readAttributes();
								entry.setCreationTime(attrs.creationTime());
								entry.setLastAccessTime(attrs.lastAccessTime());
								entry.setLastModifiedTime(attrs.lastModifiedTime());
								entry.setSize(attrs.size());
							}
							// Write file content
							try (BufferedInputStream instream = new BufferedInputStream(Files.newInputStream(filePath),
									BUFFER_SIZE)) {
								for (int count = instream.read(buffer, 0, BUFFER_SIZE); count > -1; count = instream
										.read(buffer, 0, BUFFER_SIZE)) {
									out.write(buffer, 0, count);
								}
							}
						} catch (final IOException e) {
							throw new UncheckedIOException(e);
						}
					});
				}
			}
		} catch (final IOException e) {
			throw new UncheckedIOException(e);
		}
	}
}
