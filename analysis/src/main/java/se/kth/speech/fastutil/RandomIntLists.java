/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.analysis.
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
package se.kth.speech.fastutil;

import java.util.Random;

import it.unimi.dsi.fastutil.ints.IntList;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since May 27, 2017
 *
 */
public final class RandomIntLists {

	public static int getRandomValue(final IntList vals, final Random rnd) {
		final int idx = rnd.nextInt(vals.size());
		return vals.getInt(idx);
	}

	private RandomIntLists() {

	}
}