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
package se.kth.speech;

import java.util.List;
import java.util.Random;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 7 Mar 2017
 *
 */
public final class RandomCollections {

	public static <T> T getRandomElement(final List<? extends T> list, final Random rnd) {
		return list.get(rnd.nextInt(list.size()));
	}
	
	public static <T> T getRandomElement(final T[] array, final Random rnd) {
		return array[rnd.nextInt(array.length)];
	}

	private RandomCollections() {

	}

}
