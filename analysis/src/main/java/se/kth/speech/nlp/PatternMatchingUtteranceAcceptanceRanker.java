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
package se.kth.speech.nlp;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UncheckedIOException;
import java.lang.ref.SoftReference;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.function.ToDoubleFunction;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import it.unimi.dsi.fastutil.objects.Object2DoubleAVLTreeMap;
import it.unimi.dsi.fastutil.objects.Object2DoubleSortedMap;
import it.unimi.dsi.fastutil.objects.ObjectSortedSet;
import se.kth.speech.coin.tangrams.analysis.Utterance;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Jul 13, 2017
 *
 */
public final class PatternMatchingUtteranceAcceptanceRanker implements ToDoubleFunction<Utterance> {

	private static final Pattern COLUMN_DELIMITER_PATTERN = Pattern.compile("\\t");

	private static final Logger LOGGER = LoggerFactory.getLogger(PatternMatchingUtteranceAcceptanceRanker.class);

	private static volatile SoftReference<Object2DoubleSortedMap<List<String>>> singletonAcceptanceRanksRef = new SoftReference<>(
			null);

	private static final Pattern WHITESPACE_PATTERN = Pattern.compile("\\s+");

	private static Object2DoubleSortedMap<List<String>> fetchAcceptanceRanks() {
		Object2DoubleSortedMap<List<String>> result = singletonAcceptanceRanksRef.get();
		if (result == null) {
			synchronized (PatternMatchingUtteranceAcceptanceRanker.class) {
				result = singletonAcceptanceRanksRef.get();
				if (result == null) {
					result = loadAcceptanceRankMap();
					singletonAcceptanceRanksRef = new SoftReference<>(result);
				}
			}
		}
		return result;
	}

	private static Object2DoubleSortedMap<List<String>> loadAcceptanceRankMap() {
		final Comparator<List<String>> tokenSeqComparator = new Comparator<List<String>>() {

			@Override
			public int compare(final List<String> o1, final List<String> o2) {
				// Get negative of result because longest sequences should come
				// first
				int result = Integer.compare(o2.size(), o1.size());
				if (result == 0) {
					final Iterator<String> o1Iter = o1.iterator();
					final Iterator<String> o2Iter = o2.iterator();
					while (o1Iter.hasNext()) {
						final String t1 = o1Iter.next();
						final String t2 = o2Iter.next();
						result = t1.compareTo(t2);
						if (result != 0) {
							break;
						}
					}
				}
				return result;
			}

		};
		final Object2DoubleSortedMap<List<String>> result = new Object2DoubleAVLTreeMap<>(tokenSeqComparator);
		result.defaultReturnValue(Double.NaN);
		try (final BufferedReader reader = new BufferedReader(new InputStreamReader(
				PatternMatchingUtteranceAcceptanceRanker.class.getResourceAsStream("acceptance-patterns.tsv")))) {
			for (String line = reader.readLine(); line != null; line = reader.readLine()) {
				final String[] row = COLUMN_DELIMITER_PATTERN.split(line);
				final double acceptanceRank = Double.parseDouble(row[0]);
				final List<String> tokens = Arrays.asList(WHITESPACE_PATTERN.split(row[1]));
				result.put(tokens, acceptanceRank);
			}
		} catch (final IOException e) {
			throw new UncheckedIOException(e);
		}
		LOGGER.info("Loaded acceptance data map of size {}.", result.size());
		return result;
	}

	private final Object2DoubleSortedMap<List<String>> acceptanceRanks;

	public PatternMatchingUtteranceAcceptanceRanker() {
		this(fetchAcceptanceRanks());
	}

	private PatternMatchingUtteranceAcceptanceRanker(final Object2DoubleSortedMap<List<String>> acceptanceRanks) {
		this.acceptanceRanks = acceptanceRanks;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.ToDoubleFunction#applyAsDouble(java.lang.Object)
	 */
	@Override
	public double applyAsDouble(final Utterance utt) {
		final List<List<String>> maxLengthAcceptanceTokenSeqs = createMaxLengthMatchingTokenSeqList(utt.getTokens());
		final double sentRankSum = maxLengthAcceptanceTokenSeqs.stream().mapToDouble(acceptanceRanks::getDouble).sum();
		assert !Double.isNaN(sentRankSum);
		final double result;
		if (sentRankSum < 0) {
			result = -1.0;
		} else if (sentRankSum > 0) {
			result = 1.0;
		} else {
			result = 0.0;
		}
		return result;
	}

	private List<List<String>> createMaxLengthMatchingTokenSeqList(final List<String> tokens) {
		List<List<String>> result = new ArrayList<>();
		int maxTokenSeqLength = Integer.MIN_VALUE;

		final ObjectSortedSet<List<String>> allAcceptanceTokenSeqs = acceptanceRanks.keySet();
		final ObjectSortedSet<List<String>> eqOrShorterTokenSeqs = allAcceptanceTokenSeqs.tailSet(tokens);
		assert allAcceptanceTokenSeqs.contains(tokens) == eqOrShorterTokenSeqs.contains(tokens);
		for (final List<String> acceptanceTokenSeq : eqOrShorterTokenSeqs) {
			// https://stackoverflow.com/a/32865087
			if (Collections.indexOfSubList(tokens, acceptanceTokenSeq) > -1) {
				final int tokenSeqLength = acceptanceTokenSeq.size();
				if (maxTokenSeqLength < tokenSeqLength) {
					// Reset the set of longest matching sequences
					result = new ArrayList<>();
					result.add(acceptanceTokenSeq);
					maxTokenSeqLength = tokenSeqLength;
				} else if (maxTokenSeqLength == tokenSeqLength) {
					result.add(acceptanceTokenSeq);
				} else if (!result.isEmpty()) {
					// The next elements can only be shorter than the ones
					// which
					// have already been seen; Break out of the loop
					break;
				}
			}
		}

		return result;
	}

}
