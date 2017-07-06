/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.game.
 *
 *  tangrams is free software: you can redistribute it and/or modify
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
import java.io.InputStream;
import java.net.JarURLConnection;
import java.net.URL;
import java.net.URLConnection;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Jul 6, 2017
 *
 */
public final class ClasspathResources {

	/**
	 * Safely closes an {@link InputStream} created from a directory on the
	 * classpath.
	 *
	 * @param url
	 *            A {@link URL} denoting the opened resource.
	 * @param instream
	 *            An {@link InputStream} derived from the opened resource.
	 * @throws IOException
	 *             If an I/O error occurs.
	 *
	 * @see <a href="https://bugs.openjdk.java.net/browse/JDK-8080094">OpenJDK
	 *      bug report</a>
	 */
	public static void closeDirectoryInputStream(final URL url, final InputStream instream) throws IOException {
		final URLConnection connection = url.openConnection();
		if (connection instanceof JarURLConnection) {
			final JarURLConnection jar = (JarURLConnection) connection;
			if (jar.getUseCaches()) {
				jar.getJarFile().close();
			}
		} else {
			instream.close();
		}
	}

	private ClasspathResources() {
	}

}
