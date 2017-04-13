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
package se.kth.speech.io;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.BasicFileAttributeView;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.FileTime;
import java.util.Comparator;
import java.util.Optional;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 13 Apr 2017
 *
 */
public final class FileDataAge {

	private static final Logger LOGGER = LoggerFactory.getLogger(FileDataAge.class);

	public static FileTime getFileDataAge(final BasicFileAttributes attrs) {
		final FileTime creationTime = attrs.creationTime();
		final FileTime lastModTime = attrs.lastModifiedTime();
		return Stream.of(creationTime, lastModTime).max(Comparator.naturalOrder()).get();
	}

	public static FileTime getFileDataAge(final BasicFileAttributeView attrView) throws IOException {
		return getFileDataAge(attrView.readAttributes());
	}

	public static Optional<FileTime> getFileDataAge(final Path path) throws IOException {
		final BasicFileAttributeView attrView = Files.getFileAttributeView(path, BasicFileAttributeView.class);
		final Optional<FileTime> result;
		if (attrView == null) {
			LOGGER.debug("File attribute view not available for path \"{}\".", path);
			result = Optional.empty();
		} else {
			result = Optional.of(getFileDataAge(attrView));
		}
		return result;
	}

	private FileDataAge() {

	}

}
