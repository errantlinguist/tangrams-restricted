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
package se.kth.speech.fastutil;

import it.unimi.dsi.fastutil.ints.Int2IntMap;
import it.unimi.dsi.fastutil.ints.IntOpenHashSet;
import it.unimi.dsi.fastutil.ints.IntSet;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 19 May 2017
 *
 */
public final class IntMaps {

	public static IntSet createMaxValueKeySet(final Int2IntMap map) {
		IntSet result = new IntOpenHashSet();
		final int maxCount = Integer.MIN_VALUE;
		for (final Int2IntMap.Entry goldStdReferentIdCount : map.int2IntEntrySet()) {
			final int refId = goldStdReferentIdCount.getIntKey();
			final int count = goldStdReferentIdCount.getIntValue();
			if (count > maxCount) {
				result = new IntOpenHashSet();
				result.add(refId);
			} else if (count == maxCount) {
				result.add(refId);
			}
		}
		return result;
	}

	private IntMaps() {
	}

}
