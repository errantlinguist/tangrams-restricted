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

import it.unimi.dsi.fastutil.ints.IntArrayList;
import it.unimi.dsi.fastutil.ints.IntCollection;
import it.unimi.dsi.fastutil.ints.IntList;
import it.unimi.dsi.fastutil.ints.IntOpenHashSet;
import it.unimi.dsi.fastutil.ints.IntSet;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since May 26, 2017
 *
 */
public final class IntSets {

	public static IntSet createComplementSet(final IntCollection vals, final int complementVal) {
		final IntSet result = new IntOpenHashSet(vals.size() - 1);
		for (final int val : vals) {
			if (val != complementVal) {
				result.add(val);
			}
		}
		return result;
	}

	/**
	 *
	 * @param vals
	 *            A list of <strong>unique</strong> values,i.e.&bnsp;an ordered
	 *            set of values.
	 * @param rnd
	 *            The {@link Random} instance to use for randomizing the values
	 *            chosen.
	 * @param size
	 *            The size of the result set.
	 * @param complementVal
	 *            The value not to include in the result set.
	 * @return A new {@link IntSet} of values randomly picked from the supplied
	 *         list, ensuring that it does not contain the provided complement
	 *         value while still being of the provided size.
	 */
	public static IntSet createRandomSubset(final IntList vals, final Random rnd, final int size,
			final int complementVal) {
		assert vals.size() < size;
		// if (vals.size() < size) {
		// throw new IllegalArgumentException(
		// String.format("Provided list has fewer values (%d) than supplied
		// subset result size of %d.",
		// vals.size(), size));
		// }
		final IntArrayList randomAccessVals = createComplementList(vals, complementVal);
		if (randomAccessVals.size() < size) {
			throw new IllegalArgumentException(String.format(
					"Provided list has fewer non-complement values (%d) than supplied subset result size of %d.",
					randomAccessVals.size(), size));
		}
//		IntLists.shuffle(randomAccessVals, rnd);
		final IntSet result = new IntOpenHashSet(size);
		while (result.size() < size) {
			final int val = getRandomValue(randomAccessVals, rnd);
				result.add(val);
		}
		return result;
	}

	public static int getRandomValue(final IntList vals, final Random rnd) {
		final int idx = rnd.nextInt(vals.size());
		return vals.getInt(idx);
	}

	private static IntArrayList createComplementList(final IntList vals, final int complementVal) {
		final IntArrayList result = new IntArrayList(vals.size());
		for (final int val : vals) {
			if (val != complementVal) {
				result.add(val);
			}
		}
		return result;
	}

	private IntSets() {

	}

}
