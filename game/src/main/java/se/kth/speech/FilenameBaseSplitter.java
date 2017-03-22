/*
 *  This file is part of com.github.errantlinguist.client.
 *
 *  se.kth.speech.coin.tangrams.client is free software: you can redistribute it and/or modify
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

import java.util.function.Function;
import java.util.regex.Pattern;

/**
 * @author <a href="mailto:tcshore@kth.se>Todd Shore</a>
 * @since 5 Mar 2017
 *
 */
public final class FilenameBaseSplitter implements Function<String, String> {

	/**
	 * @see <a href=
	 *      "http://stackoverflow.com/a/4546093/1391325">StackOverflow</a>
	 */
	private static final Pattern FILE_EXT_SPLITTING_PATTERN = Pattern.compile("\\.(?!.*\\.)");

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.util.function.Function#apply(java.lang.Object)
	 */
	@Override
	public String apply(final String path) {
		final String[] filenameParts = FILE_EXT_SPLITTING_PATTERN.split(path);
		return filenameParts[0];
	}

}
