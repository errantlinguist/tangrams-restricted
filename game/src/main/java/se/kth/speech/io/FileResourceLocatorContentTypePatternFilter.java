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
package se.kth.speech.io;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.function.Predicate;
import java.util.regex.Pattern;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 7 Mar 2017
 *
 */
public final class FileResourceLocatorContentTypePatternFilter implements Predicate<String> {

	private final Pattern contentTypePattern;

	public FileResourceLocatorContentTypePatternFilter(final Pattern contentTypePattern) {
		this.contentTypePattern = contentTypePattern;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Predicate#test(java.lang.Object)
	 */
	@Override
	public boolean test(final String resourceLoc) {
		final boolean result;
		final Path resourceLocPath = Paths.get(resourceLoc);
		if (Files.isDirectory(resourceLocPath)) {
			result = false;
		} else {
			try {
				final String mimeType = Files.probeContentType(resourceLocPath);
				if (mimeType == null){
					result = false;
				} else {
					result = contentTypePattern.matcher(mimeType).matches();	
				}
			} catch (final IOException e) {
				throw new UncheckedIOException(e);
			}
		}
		return result;
	}

}
