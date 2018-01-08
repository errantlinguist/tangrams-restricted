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
package se.kth.speech.nlp;

/**
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since 29 May 2017
 *
 */
public final class Disfluencies {

	public static boolean isDisfluency(final CharSequence token) {
		final char tokenTruncationMarker = '-';
		final boolean result;
		if (token.length() < 1) {
			result = false;
		} else if (token.charAt(0) == tokenTruncationMarker) {
			result = true;
		} else if (token.charAt(token.length() - 1) == tokenTruncationMarker) {
			result = true;
		} else {
			result = false;
		}
		return result;
	}

	private Disfluencies() {
	}

}
