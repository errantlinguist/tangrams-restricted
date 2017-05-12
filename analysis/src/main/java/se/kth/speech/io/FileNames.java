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

import java.util.regex.Pattern;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 12 May 2017
 *
 */
public final class FileNames {

	public static final Pattern ILLEGAL_CHAR_PATTERN;

	public static final char[] ILLEGAL_CHARACTERS;

	static {
		// http://stackoverflow.com/a/894133/1391325
		ILLEGAL_CHARACTERS = new char[] { '/', '\n', '\r', '\t', '\0', '\f', '`', '?', '*', '\\', '<', '>', '|', '\"',
				':' };
		final String regex = "[" + Pattern.quote(new String(ILLEGAL_CHARACTERS)) + "]";
		ILLEGAL_CHAR_PATTERN = Pattern.compile(regex);
	}
	
	public static String sanitize(CharSequence fileName, String replacement){
		return ILLEGAL_CHAR_PATTERN.matcher(fileName).replaceAll(replacement);
	}

	private FileNames() {
	}

}
