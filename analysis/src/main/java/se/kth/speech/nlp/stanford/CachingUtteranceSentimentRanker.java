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
package se.kth.speech.nlp.stanford;

import java.util.List;
import java.util.function.ToDoubleFunction;

import edu.stanford.nlp.ling.CoreAnnotations.SentencesAnnotation;
import edu.stanford.nlp.pipeline.Annotation;
import edu.stanford.nlp.pipeline.Annotator;
import edu.stanford.nlp.sentiment.RNNOptions;
import edu.stanford.nlp.sentiment.SentimentCoreAnnotations.SentimentClass;
import edu.stanford.nlp.util.CoreMap;
import it.unimi.dsi.fastutil.objects.Object2DoubleMap;
import it.unimi.dsi.fastutil.objects.Object2DoubleOpenHashMap;
import it.unimi.dsi.fastutil.objects.Object2IntMap;
import it.unimi.dsi.fastutil.objects.Object2IntOpenHashMap;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Jul 9, 2017
 *
 */
public final class CachingUtteranceSentimentRanker implements ToDoubleFunction<Utterance> {

	private static final Object2IntMap<String> SENTIMENT_LABEL_WEIGHTS = createSentimentClassWeightMap();

	/**
	 *
	 * @see edu.stanford.nlp.sentiment.RNNOptions#DEFAULT_CLASS_NAMES
	 */
	private static Object2IntMap<String> createSentimentClassWeightMap() {
		final String[] classNames = RNNOptions.DEFAULT_CLASS_NAMES;
		final Object2IntMap<String> result = new Object2IntOpenHashMap<>(classNames.length);
		result.defaultReturnValue(0);
		result.put("Very negative", -2);
		result.put("Negative", -1);
		result.put("Neutral", 0);
		result.put("Positive", 1);
		result.put("Very positive", 2);
		return result;
	}

	private final Annotator annotator;

	private final Object2DoubleMap<Utterance> uttWeightCache;

	public CachingUtteranceSentimentRanker(final Annotator annotator, final int expectedUniqueUttCount) {
		this.annotator = annotator;
		uttWeightCache = new Object2DoubleOpenHashMap<>(expectedUniqueUttCount);
	}

	@Override
	public double applyAsDouble(final Utterance utt) {
		final double result;
		if (uttWeightCache.containsKey(utt)) {
			result = uttWeightCache.getDouble(utt);
		} else {
			// Calculate the weight without any locking because it's okay if
			// weight is sometimes calculated more than once per utt inst-- this
			// function is stateless
			result = calculateUttSentimentRank(utt);
			uttWeightCache.put(utt, result);
		}
		return result;
	}

	private double calculateUttSentimentRank(final Utterance utt) {
		final Annotation annot = new Annotation(utt.getTokenStr());
		annotator.annotate(annot);
		final List<CoreMap> sents = annot.get(SentencesAnnotation.class);
		final double result;
		if (sents.isEmpty()) {
			result = 0.0;
		} else {
			int rankSum = 0;
			// traversing the words in the current sentence
			for (final CoreMap sent : sents) {
				final String sentimentClass = sent.get(SentimentClass.class);
				rankSum += SENTIMENT_LABEL_WEIGHTS.getInt(sentimentClass);
			}
			result = rankSum / (double) sents.size();
		}
		return result;
	}

}
