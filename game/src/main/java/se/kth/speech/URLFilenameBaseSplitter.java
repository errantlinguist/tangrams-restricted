/*
 *  This file is part of game.
 *
 *  game is free software: you can redistribute it and/or modify
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
package se.kth.speech;

import java.net.URL;
import java.util.function.Function;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 23 Mar 2017
 *
 */
public final class URLFilenameBaseSplitter implements Function<URL, String> {

	private final FilenameBaseSplitter filenameBaseSplitter;

	public URLFilenameBaseSplitter() {
		filenameBaseSplitter = new FilenameBaseSplitter();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Function#apply(java.lang.Object)
	 */
	@Override
	public String apply(final URL url) {
		final String urlStr = url.toString();
		final String filename = urlStr.substring(urlStr.lastIndexOf('/') + 1);
		return filenameBaseSplitter.apply(filename);
	}

}
