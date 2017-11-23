/*
 * The Alphanum Algorithm is an improved sorting algorithm for strings
 * containing numbers.  Instead of sorting numbers in ASCII order like
 * a standard sort, this algorithm sorts numbers in numeric order.
 *
 * The Alphanum Algorithm is discussed at http://www.DaveKoelle.com
 *
 * Released under the MIT License - https://opensource.org/licenses/MIT
 *
 * Copyright 2007-2017 David Koelle
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
 * OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
 * USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
package se.kth.speech;

import java.util.Comparator;

/**
 * <p>
 * This is an updated version with enhancements made by Daniel Migowski, Andre
 * Bogus, and David Koelle. Updated by David Koelle in 2017.
 * </p>
 *
 * To use this class: Use the static "sort" method from the
 * {@code java.util.Collections} class: <code>Collections.sort(your list, new
 * AlphanumComparator());</code>
 *
 * <p>
 * This class was converted into a singleton which is <a href=
 * "https://en.wikipedia.org/wiki/Initialization-on-demand_holder_idiom">initialized
 * on demand</a> by Todd Shore on Nov 23, 2017.
 * </p>
 *
 * @author Daniel Migowski
 * @author Andre Bogus
 * @author David Koelle
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @version Nov 23, 2017
 */
public final class AlphanumComparator implements Comparator<String> {

	/**
	 * {@code SingletonHolder} is loaded on the first execution of
	 * {@link AlphanumComparator#getInstance()} or access of
	 * {@link SingletonHolder#INSTANCE}.
	 *
	 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
	 * @since Nov 23, 2017
	 * @see <a href=
	 *      "https://en.wikipedia.org/wiki/Initialization-on-demand_holder_idiom">&ldquo;Initialization-on-demand
	 *      holder idiom&rdquo; on Wikipedia</a>
	 *
	 */
	private static class SingletonHolder {
		/**
		 * The singleton {@link AlphanumComparator} instance.
		 */
		private static final AlphanumComparator INSTANCE = new AlphanumComparator();

		private SingletonHolder() {

		}
	}

	/**
	 *
	 * @return The singleton {@link AlphanumComparator} instance.
	 */
	public static AlphanumComparator getInstance() {
		return SingletonHolder.INSTANCE;
	}

	private AlphanumComparator() {
		// Avoid instantiation
	}

	@Override
	public int compare(final String s1, final String s2) {
		if (s1 == null || s2 == null) {
			return 0;
		}

		int thisMarker = 0;
		int thatMarker = 0;
		final int s1Length = s1.length();
		final int s2Length = s2.length();

		while (thisMarker < s1Length && thatMarker < s2Length) {
			final String thisChunk = getChunk(s1, s1Length, thisMarker);
			thisMarker += thisChunk.length();

			final String thatChunk = getChunk(s2, s2Length, thatMarker);
			thatMarker += thatChunk.length();

			// If both chunks contain numeric characters, sort them numerically
			int result = 0;
			if (isDigit(thisChunk.charAt(0)) && isDigit(thatChunk.charAt(0))) {
				// Simple chunk comparison by length.
				final int thisChunkLength = thisChunk.length();
				result = thisChunkLength - thatChunk.length();
				// If equal, the first different number counts
				if (result == 0) {
					for (int i = 0; i < thisChunkLength; i++) {
						result = thisChunk.charAt(i) - thatChunk.charAt(i);
						if (result != 0) {
							return result;
						}
					}
				}
			} else {
				result = thisChunk.compareTo(thatChunk);
			}

			if (result != 0) {
				return result;
			}
		}

		return s1Length - s2Length;
	}

	/**
	 * Length of string is passed in for improved efficiency (only need to
	 * calculate it once)
	 **/
	private final String getChunk(final String s, final int slength, int marker) {
		final StringBuilder chunk = new StringBuilder();
		char c = s.charAt(marker);
		chunk.append(c);
		marker++;
		if (isDigit(c)) {
			while (marker < slength) {
				c = s.charAt(marker);
				if (!isDigit(c)) {
					break;
				}
				chunk.append(c);
				marker++;
			}
		} else {
			while (marker < slength) {
				c = s.charAt(marker);
				if (isDigit(c)) {
					break;
				}
				chunk.append(c);
				marker++;
			}
		}
		return chunk.toString();
	}

	private final boolean isDigit(final char ch) {
		return ch >= 48 && ch <= 57;
	}
}
