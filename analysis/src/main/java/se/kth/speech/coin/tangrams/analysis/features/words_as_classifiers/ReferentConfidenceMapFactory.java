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

import java.util.List;
import java.util.function.Function;

import javax.inject.Inject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import it.unimi.dsi.fastutil.ints.Int2DoubleMap;
import it.unimi.dsi.fastutil.ints.Int2DoubleOpenHashMap;
import it.unimi.dsi.fastutil.ints.IntList;
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
public final class ReferentConfidenceMapFactory {

	private static final Logger LOGGER = LoggerFactory.getLogger(ReferentConfidenceMapFactory.class);

	@Inject
	private EntityFeatureExtractionContextFactory extCtxFactory;

	@Inject
	private WordClassDiscountingSmoother smoother;

	private final Function<? super EntityFeature.Extractor.Context, ? extends Instance> testInstFactory;

	private final Function<? super String, ? extends Classifier> wordClassifiers;

	public ReferentConfidenceMapFactory(
			final Function<? super String, ? extends Classifier> wordClassifiers,
			final Function<? super EntityFeature.Extractor.Context, ? extends Instance> testInstFactory) {
		this.wordClassifiers = wordClassifiers;
		this.testInstFactory = testInstFactory;
	}

	public Int2DoubleMap apply(final List<String> tokens, final GameContext uttCtx)
			throws ClassificationException {
		LOGGER.debug("Getting entity reference confidence measures for linguistic tokens: {}.", tokens);
		// TODO: Cache mapping of word classes -> classifiers?
		final List<Classifier> classifiers = smoother.createNGramClassifierList(tokens, wordClassifiers);
		final IntList entityIds = uttCtx.getEntityIds();
		final Int2DoubleMap result = new Int2DoubleOpenHashMap(entityIds.size());
		for (final int entityId : entityIds) {
			// Create a game context for classifying the entity with the
			// given ID
			final EntityFeature.Extractor.Context extContext = extCtxFactory.apply(uttCtx, entityId);
			final Instance testInst = testInstFactory.apply(extContext);
			double confidenceSum = 0.0;
			for (final Classifier classifier : classifiers) {
				try {
					final double[] classValProbs = classifier.distributionForInstance(testInst);
					final double classValProb = AttributeValues.findNominalClassValueProbability(testInst,
							classValProbs, Boolean.TRUE.toString());
					confidenceSum += classValProb;
				} catch (final Exception e) {
					throw new ClassificationException(e);
				}
			}
			final double normalizedConfidence = confidenceSum / classifiers.size();
			result.put(entityId, normalizedConfidence);
		}
		return result;
	}

}
