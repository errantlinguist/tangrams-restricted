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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumMap;
import java.util.EnumSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.Predicate;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.Lists;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 19 Nov 2017
 *
 */
public final class WordClassifierTrainingParameterTableParser {

	private static final String COL_SEP = "\t";

	private static final String DEFAULT_COMMENT_LINE_PREFIX = "#";

	private static final Logger LOGGER = LoggerFactory.getLogger(WordClassifierTrainingParameterTableParser.class);

	private static Map<WordClassifierTrainingParameter, Integer> createParamColumnIndexMap(
			final Map<String, Integer> colIdxs) {
		final Map<WordClassifierTrainingParameter, Integer> result = new EnumMap<>(
				WordClassifierTrainingParameter.class);

		for (final Entry<String, Integer> colIdx : colIdxs.entrySet()) {
			final String colName = colIdx.getKey();
			try {
				final WordClassifierTrainingParameter param = WordClassifierTrainingParameter.valueOf(colName);
				final Integer idx = colIdx.getValue();
				final Integer oldIdx = result.put(param, idx);
				if (oldIdx != null) {
					throw new IllegalArgumentException(
							String.format("Found duplicate column for %s. Already-processed index: %d; New index: %d",
									param, oldIdx, idx));
				}
			} catch (final IllegalArgumentException e) {
				LOGGER.debug("Column \"{}\" could not be parsed as a {}; Skipping.", colName,
						WordClassifierTrainingParameter.class.getSimpleName());
			}
		}

		if (result.size() < WordClassifierTrainingParameter.values().length) {
			final EnumSet<WordClassifierTrainingParameter> missingParams = EnumSet
					.allOf(WordClassifierTrainingParameter.class);
			missingParams.removeAll(result.keySet());
			throw new IllegalArgumentException(
					String.format("Could not find columns for all %s values. Missing values: %s",
							WordClassifierTrainingParameter.class.getSimpleName(), missingParams));
		} else {
			return result;
		}
	}

	private final Predicate<? super String> lineFilter;

	public WordClassifierTrainingParameterTableParser() {
		this(DEFAULT_COMMENT_LINE_PREFIX);
	}

	public WordClassifierTrainingParameterTableParser(final Predicate<? super String> lineFilter) {
		this.lineFilter = lineFilter;
	}

	public WordClassifierTrainingParameterTableParser(final String commentLinePrefix) {
		this(line -> !line.startsWith(commentLinePrefix));
	}

	public List<Map<WordClassifierTrainingParameter, Object>> apply(final List<String> lines) {
		return apply(lines.iterator(), lines.size() - 1);
	}

	private List<Map<WordClassifierTrainingParameter, Object>> apply(final Iterator<String> lines,
			final int expectedResultSize) {
		final List<Map<WordClassifierTrainingParameter, Object>> result = new ArrayList<>(expectedResultSize);

		final Map<String, Integer> colIdxs = Lists.createIndexMap(Arrays.asList(lines.next().split(COL_SEP)));
		final Map<WordClassifierTrainingParameter, Integer> paramIdxs = createParamColumnIndexMap(colIdxs);
		while (lines.hasNext()) {
			final String line = lines.next();
			if (lineFilter.test(line)) {
				final String[] rowCells = line.split(COL_SEP);
				final Map<WordClassifierTrainingParameter, Object> paramValues = new EnumMap<>(
						WordClassifierTrainingParameter.class);
				paramIdxs.forEach((param, idx) -> {
					final String paramRowCell = rowCells[idx];
					final Object paramValue = param.parseValue(paramRowCell);
					paramValues.put(param, paramValue);
				});
				result.add(paramValues);
			}
		}

		assert result.size() <= expectedResultSize;
		return result;
	}

}