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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers;

import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.function.Function;

import javax.annotation.concurrent.NotThreadSafe;
import javax.inject.Inject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import it.unimi.dsi.fastutil.objects.Object2DoubleMap;
import it.unimi.dsi.fastutil.objects.Object2DoubleOpenHashMap;
import se.kth.speech.coin.tangrams.analysis.GameContext;
import se.kth.speech.coin.tangrams.analysis.features.ClassificationException;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeature;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeatureExtractionContextFactory;
import se.kth.speech.coin.tangrams.analysis.features.weka.AttributeValues;
import weka.classifiers.Classifier;
import weka.core.Instance;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 18 May 2017
 *
 */
@NotThreadSafe
public final class ReferentConfidenceMapFactory {

	private static final String INST_CLASS_VAL = Boolean.TRUE.toString();

	private static final Logger LOGGER = LoggerFactory.getLogger(ReferentConfidenceMapFactory.class);

	private static final Optional<ReferentConfidenceData> NULL_RESULT = Optional.empty();

	@Inject
	private EntityFeatureExtractionContextFactory extCtxFactory;

	private final WordClassDiscountingSmoother smoother;

	private final Function<? super EntityFeature.Extractor.Context, ? extends Instance> testInstFactory;

	public ReferentConfidenceMapFactory( // NO_UCD (unused code)
			final Function<? super EntityFeature.Extractor.Context, ? extends Instance> testInstFactory,
			final WordClassDiscountingSmoother smoother) {
		this.testInstFactory = testInstFactory;
		this.smoother = smoother;
	}

	/**
	 *
	 * @param tokens
	 *            A {@link Object2DoubleMap} of unique token types
	 *            (i.e.&nbsp;words) observed and their respective weights
	 *            (e.g.&nbsp;how often each was observed times a supplied weight
	 *            factor).
	 * @param uttCtx
	 *            The {@link GameContext} in which the given tokens were
	 *            observed.
	 * @param wordClassifiers
	 *            A {@link Function} returning a {@link Classifier} to use for
	 *            classification for a given token type.
	 * @return An {@link Optional optional} {@link ReferentConfidenceData}
	 *         instance representing the confidence of each possible referent
	 *         being the actual referent.
	 * @throws ClassificationException
	 *             If an {@link Exception} occurs during
	 *             {@link Classifier#distributionForInstance(Instance)
	 *             classification} for any individual entity.
	 */
	public Optional<ReferentConfidenceData> apply(final Object2DoubleMap<String> tokens, final GameContext uttCtx,
			final Function<? super String, ? extends Classifier> wordClassifiers) throws ClassificationException {
		final Optional<ReferentConfidenceData> result;

		LOGGER.debug("Getting entity reference confidence measures for linguistic tokens: {}.", tokens);
		if (tokens.isEmpty()) {
			result = NULL_RESULT;
		} else {
			final Map<String, WeightedClassifier> weightedClassifiers = smoother
					.createWeightedClassifierMap(tokens.object2DoubleEntrySet(), wordClassifiers);

			final int entityCount = uttCtx.getEntityCount();
			final double[] referentConfidenceVals = new double[entityCount];
			final Object2DoubleMap<String> wordClassWeights = new Object2DoubleOpenHashMap<>(
					weightedClassifiers.size() + 1, 1.0f);
			wordClassWeights.defaultReturnValue(Double.NaN);

			final double totalWeight = weightedClassifiers.values().stream().mapToDouble(WeightedClassifier::getWeight)
					.sum();
			for (int entityId = 0; entityId < entityCount; ++entityId) {
				// Create a game context for classifying the entity with the
				// given ID
				final EntityFeature.Extractor.Context extContext = extCtxFactory.apply(uttCtx, entityId);
				final Instance testInst = testInstFactory.apply(extContext);
				double confidenceSum = 0.0;
				for (final Entry<String, WeightedClassifier> entry : weightedClassifiers.entrySet()) {
					final String wordClass = entry.getKey();
					final WeightedClassifier weightedClassifier = entry.getValue();
					final Classifier classifier = weightedClassifier.getClassifier();
					final double weight = weightedClassifier.getWeight();
					wordClassWeights.put(wordClass, weight);
					try {
						final double[] classValProbs = classifier.distributionForInstance(testInst);
						final double classValProb = AttributeValues.findNominalClassValueProbability(testInst,
								classValProbs, INST_CLASS_VAL);
						confidenceSum += classValProb * weight;
					} catch (final Exception e) {
						throw new ClassificationException(e);
					}
				}
				final double normalizedConfidence = confidenceSum / totalWeight;
				referentConfidenceVals[entityId] = normalizedConfidence;
			}
			assert wordClassWeights.size() == weightedClassifiers.size();

			final String oovClassName = smoother.getOovClassName();
			result = Optional.of(new ReferentConfidenceData(referentConfidenceVals, wordClassWeights, oovClassName));
		}
		
		return result;
	}

}
