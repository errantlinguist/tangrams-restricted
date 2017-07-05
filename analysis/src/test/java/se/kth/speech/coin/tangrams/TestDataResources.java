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
package se.kth.speech.coin.tangrams;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UncheckedIOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 17 May 2017
 *
 */
public final class TestDataResources {

	private static final String ROOT_DIR;

	public static final String SESSION_DATA_DIR;

	static {
		ROOT_DIR = "test-data";
		SESSION_DATA_DIR = createResourceLocator("sessions");
	}

	public static String createResourceLocator(final String resourceName) {
		return ROOT_DIR + "/" + resourceName;
	}

	public static List<Path> createSessionDataDirectoryPathList() {
		try {
			return createSubdirectoryPathList(TestDataResources.class, SESSION_DATA_DIR);
		} catch (final IOException e) {
			throw new UncheckedIOException(e);
		}
	}

	private static List<Path> createSubdirectoryPathList(final Class<?> loadingClass, final String dirname)
			throws IOException {
		final List<Path> result = new ArrayList<>();
		try (BufferedReader br = new BufferedReader(new InputStreamReader(loadingClass.getResourceAsStream(dirname)))) {
			for (String line = br.readLine(); line != null; line = br.readLine()) {
				final String resLocName = dirname + "/" + line;
				final URL resLoc = loadingClass.getResource(resLocName);
				final Path resPath = Paths.get(resLoc.toURI());
				if (Files.isDirectory(resPath)) {
					result.add(resPath);
				}
			}
		} catch (final URISyntaxException e) {
			// Should never happen if the logic above is correct
			throw new AssertionError(e);
		}
		return result;
	}

	private TestDataResources() {
	}

}
