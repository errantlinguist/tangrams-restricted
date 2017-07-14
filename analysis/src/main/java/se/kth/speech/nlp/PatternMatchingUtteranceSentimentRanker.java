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
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.ToDoubleFunction;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import it.unimi.dsi.fastutil.objects.Object2DoubleMap;
import it.unimi.dsi.fastutil.objects.Object2DoubleOpenHashMap;
import se.kth.speech.coin.tangrams.analysis.Utterance;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Jul 13, 2017
 *
 */
public final class PatternMatchingUtteranceSentimentRanker implements ToDoubleFunction<Utterance> {

	private static final Pattern COLUMN_DELIMITER_PATTERN = Pattern.compile("\\t");

	private static final Logger LOGGER = LoggerFactory.getLogger(PatternMatchingUtteranceSentimentRanker.class);

	private static SoftReference<Object2DoubleMap<List<String>>> singletonSentimentRanksRef = new SoftReference<>(null);

	private static final Pattern WHITESPACE_PATTERN = Pattern.compile("\\s+");

	private static Object2DoubleMap<List<String>> fetchSentimentRanks() {
		Object2DoubleMap<List<String>> result = singletonSentimentRanksRef.get();
		if (result == null) {
			synchronized (PatternMatchingUtteranceSentimentRanker.class) {
				result = singletonSentimentRanksRef.get();
				if (result == null) {
					result = loadSentimentRankMap();
					singletonSentimentRanksRef = new SoftReference<>(result);
				}
			}
		}
		return result;
	}

	private static Object2DoubleMap<List<String>> loadSentimentRankMap() {
		final Object2DoubleMap<List<String>> result = new Object2DoubleOpenHashMap<>();
		result.defaultReturnValue(Double.NaN);
		try (final BufferedReader reader = new BufferedReader(new InputStreamReader(
				PatternMatchingUtteranceSentimentRanker.class.getResourceAsStream("sentiment-patterns.tsv")))) {
			for (String line = reader.readLine(); line != null; line = reader.readLine()) {
				final String[] row = COLUMN_DELIMITER_PATTERN.split(line);
				final double sentimentRank = Double.parseDouble(row[0]);
				final List<String> tokens = Arrays.asList(WHITESPACE_PATTERN.split(row[1]));
				result.put(tokens, sentimentRank);
			}
		} catch (final IOException e) {
			throw new UncheckedIOException(e);
		}
		LOGGER.info("Loaded sentiment data map of size {}.", result.size());
		return result;
	}

	private final Object2DoubleMap<List<String>> sentimentRanks;

	public PatternMatchingUtteranceSentimentRanker() {
		this(fetchSentimentRanks());
	}

	private PatternMatchingUtteranceSentimentRanker(final Object2DoubleMap<List<String>> sentimentRanks) {
		this.sentimentRanks = sentimentRanks;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.ToDoubleFunction#applyAsDouble(java.lang.Object)
	 */
	@Override
	public double applyAsDouble(final Utterance utt) {
		final Set<List<String>> maxLengthSentimentTokenSeqs = createMaxLengthMatchingTokenSeqSet(utt.getTokens());
		final double sentRankSum = maxLengthSentimentTokenSeqs.stream().mapToDouble(sentimentRanks::getDouble).sum();
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

	private Set<List<String>> createMaxLengthMatchingTokenSeqSet(final List<String> tokens) {
		Set<List<String>> result = new HashSet<>();
		int maxTokenSeqLength = Integer.MIN_VALUE;

		for (final List<String> sentimentTokenSeq : sentimentRanks.keySet()) {
			// https://stackoverflow.com/a/32865087
			if (Collections.indexOfSubList(tokens, sentimentTokenSeq) > -1) {
				final int tokenSeqLength = sentimentTokenSeq.size();
				if (maxTokenSeqLength < tokenSeqLength) {
					// Reset the set of longest matching sequences
					result = new HashSet<>();
					result.add(sentimentTokenSeq);
					maxTokenSeqLength = tokenSeqLength;
				} else if (maxTokenSeqLength == tokenSeqLength) {
					result.add(sentimentTokenSeq);
				}
			}
		}

		return result;
	}

}
