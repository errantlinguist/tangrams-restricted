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
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import java.util.function.ToDoubleFunction;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import it.unimi.dsi.fastutil.objects.Object2DoubleMap;
import it.unimi.dsi.fastutil.objects.Object2DoubleOpenHashMap;
import it.unimi.dsi.fastutil.objects.Object2IntMap;
import it.unimi.dsi.fastutil.objects.Object2IntOpenHashMap;
import se.kth.speech.coin.tangrams.analysis.Utterance;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Jul 13, 2017
 *
 */
public final class PatternMatchingUtteranceSentimentRanker implements ToDoubleFunction<Utterance> {

	private static final Pattern COLUMN_DELIMITER_PATTERN = Pattern.compile("\\t");

	private static final Logger LOGGER = LoggerFactory.getLogger(PatternMatchingUtteranceSentimentRanker.class);

	private static SoftReference<Object2DoubleMap<String>> singletonSentimentRanksRef = new SoftReference<>(null);

	private static final Pattern WHITESPACE_PATTERN = Pattern.compile("\\s+");

	private static Object2IntMap<String> createTokenSequenceLengthMap(final Collection<String> tokenSeqs) {
		final Object2IntMap<String> result = new Object2IntOpenHashMap<>(tokenSeqs.size());
		for (final String tokenSeq : tokenSeqs) {
			final String[] tokens = WHITESPACE_PATTERN.split(tokenSeq);
			result.put(tokenSeq, tokens.length);
		}
		return result;
	}

	private static Object2DoubleMap<String> fetchSentimentRanks() {
		Object2DoubleMap<String> result = singletonSentimentRanksRef.get();
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

	private static Object2DoubleMap<String> loadSentimentRankMap() {
		final Object2DoubleMap<String> result = new Object2DoubleOpenHashMap<>();
		result.defaultReturnValue(Double.NaN);
		try (final BufferedReader reader = new BufferedReader(new InputStreamReader(
				PatternMatchingUtteranceSentimentRanker.class.getResourceAsStream("sentiment-patterns.tsv")))) {
			for (String line = reader.readLine(); line != null; line = reader.readLine()) {
				final String[] row = COLUMN_DELIMITER_PATTERN.split(line);
				final double sentimentRank = Double.parseDouble(row[0]);
				result.put(row[1], sentimentRank);
			}
		} catch (final IOException e) {
			throw new UncheckedIOException(e);
		}
		LOGGER.info("Loaded sentiment data map of size {}.", result.size());
		return result;
	}

	private final Object2DoubleMap<String> sentimentRanks;

	private final Object2IntMap<String> tokenSeqLengths;

	public PatternMatchingUtteranceSentimentRanker() {
		this(fetchSentimentRanks());
	}

	private PatternMatchingUtteranceSentimentRanker(final Object2DoubleMap<String> sentimentRanks) {
		this.sentimentRanks = sentimentRanks;
		tokenSeqLengths = createTokenSequenceLengthMap(sentimentRanks.keySet());
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.ToDoubleFunction#applyAsDouble(java.lang.Object)
	 */
	@Override
	public double applyAsDouble(final Utterance utt) {
		final String tokenRepr = utt.getTokenStr();
		final Set<String> maxLengthSentimentTokenSeqs = createMaxLengthMatchingTokenSeqSet(tokenRepr);
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

	private Set<String> createMaxLengthMatchingTokenSeqSet(final String tokenRepr) {
		Set<String> result = new HashSet<>();
		int maxTokenSeqLength = Integer.MIN_VALUE;

		for (final String sentimentTokenSeq : sentimentRanks.keySet()) {
			if (tokenRepr.contains(sentimentTokenSeq)) {
				final int tokenSeqLength = tokenSeqLengths.getInt(sentimentTokenSeq);
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
