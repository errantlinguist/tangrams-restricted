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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.cross_validation;

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.regex.Pattern;

import it.unimi.dsi.fastutil.objects.Object2IntMap;
import it.unimi.dsi.fastutil.objects.Object2IntOpenHashMap;
import it.unimi.dsi.fastutil.objects.Object2ObjectMap;
import it.unimi.dsi.fastutil.objects.Object2ObjectOpenHashMap;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 14 Nov 2017
 *
 */
final class UtteranceReferringLanguageMapReader {

	private static final String COL_SEP = "\t";

	private static final Pattern WHITESPACE_PATTERN = Pattern.compile("\\s+");

	private static <T> Object2IntMap<T> createIndexMap(final List<? extends T> list) {
		final Object2IntMap<T> result = new Object2IntOpenHashMap<>(list.size());
		final ListIterator<? extends T> iter = list.listIterator();
		while (iter.hasNext()) {
			final int idx = iter.nextIndex();
			final T next = iter.next();
			result.put(next, idx);
		}
		return result;
	}

	private final String refLangCol;

	private final String uttColName;

	public UtteranceReferringLanguageMapReader(final String uttColName, final String refLangCol) {
		this.uttColName = uttColName;
		this.refLangCol = refLangCol;
	}

	public Object2ObjectMap<List<String>, List<String>> apply(final List<String> lines) {
		return apply(lines.iterator(), lines.size());
	}

	private Object2ObjectMap<List<String>, List<String>> apply(final Iterator<String> lines,
			final int expectedUniqueTokenSeqCount) {
		final Object2ObjectOpenHashMap<List<String>, List<String>> result = new Object2ObjectOpenHashMap<>(
				expectedUniqueTokenSeqCount + 1, 1.0f);

		final String headerStr = lines.next();
		final Object2IntMap<String> colIdxs = createIndexMap(Arrays.asList(headerStr.split(COL_SEP)));
		final int uttColIdx = colIdxs.getInt(uttColName);
		final int refLangIdx = colIdxs.getInt(refLangCol);

		while (lines.hasNext()) {
			final String line = lines.next();
			final String[] rowCells = line.split(COL_SEP);
			final String uttStr = rowCells[uttColIdx];
			final List<String> uttTokSeq = Arrays.asList(WHITESPACE_PATTERN.split(uttStr));
			final String refLangStr = rowCells[refLangIdx];
			final List<String> refLangTokens = Arrays.asList(WHITESPACE_PATTERN.split(refLangStr));
			result.put(uttTokSeq, refLangTokens);
		}

		result.trim();
		return result;
	}

}
