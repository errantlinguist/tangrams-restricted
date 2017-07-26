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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training;

import java.util.Collection;
import java.util.Map;
import java.util.function.Function;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Maps;

import it.unimi.dsi.fastutil.objects.Object2IntMap;
import it.unimi.dsi.fastutil.objects.Object2IntOpenHashMap;
import se.kth.speech.coin.tangrams.analysis.SessionEventDialogueManager;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeature;
import se.kth.speech.coin.tangrams.analysis.features.weka.EntityInstanceAttributeContext;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.WordClasses;
import weka.core.DenseInstance;
import weka.core.Instance;
import weka.core.Instances;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since May 26, 2017
 *
 */
public abstract class AbstractSizeEstimatingInstancesMapFactory implements TrainingInstancesFactory {

	private static final Logger LOGGER = LoggerFactory.getLogger(AbstractSizeEstimatingInstancesMapFactory.class);

	private static int estimateVocabTypeCount(final Collection<?> sessionData) {
		final double estimate = Math.log(sessionData.size() + 1) * 850;
		int result = Integer.MAX_VALUE;
		try {
			result = Math.toIntExact(Math.round(Math.ceil(estimate)));
		} catch (final ArithmeticException e) {
			LOGGER.debug("Vocab type count estimate error.", e);
		}
		return result;
	}

	private static int estimateVocabTypeTokenCount(final Collection<?> sessionData) {
		// Number of sessions * estimated number of dialogues per session *
		// estimated number of utterances per dialogue * estimated number of
		// tokens (i.e. n-grams) per utterance
		final long estimate = sessionData.size() * 50 * 4 * 10;
		int result = Integer.MAX_VALUE;
		try {
			result = Math.toIntExact(estimate);
		} catch (final ArithmeticException e) {
			LOGGER.debug("Vocab type token count estimate error.", e);
		}
		return result;
	}

	private final EntityInstanceAttributeContext entityInstAttrCtx;

	protected AbstractSizeEstimatingInstancesMapFactory(final EntityInstanceAttributeContext entityInstAttrCtx) {
		this.entityInstAttrCtx = entityInstAttrCtx;
	}

	@Override
	public WordClassificationData apply(final Collection<SessionEventDialogueManager> sessionEventDiagMgrs) {
		final int estClassCount = estimateVocabTypeCount(sessionEventDiagMgrs);
		final Map<String, Instances> classInstances = Maps.newHashMapWithExpectedSize(estClassCount);
		final Function<String, Instances> classInstancesFetcher = className -> classInstances.computeIfAbsent(className,
				key -> {
					final Instances instances = new Instances(WordClasses.createRelationName(key),
							entityInstAttrCtx.getAttrs(), estimateVocabTypeTokenCount(sessionEventDiagMgrs));
					instances.setClass(entityInstAttrCtx.getClassAttr());
					return instances;
				});
		final Object2IntMap<String> classObservationCounts = new Object2IntOpenHashMap<>(estClassCount);
		classObservationCounts.defaultReturnValue(0);
		final WordClassificationData result = new WordClassificationData(classInstances, classObservationCounts,
				classInstancesFetcher);
		for (final SessionEventDialogueManager sessionEventDiagMgr : sessionEventDiagMgrs) {
			addTrainingData(sessionEventDiagMgr, result);
		}
		return result;
	}

	protected abstract void addTrainingData(SessionEventDialogueManager sessionEventDiagMgr,
			WordClassificationData trainingData);

	protected Instance createTokenInstance(final Instances classInsts,
			final EntityFeature.Extractor.Context extractionContext, final String classValue) {
		final Instance result = new DenseInstance(entityInstAttrCtx.getAttrs().size());
		result.setDataset(classInsts);
		entityInstAttrCtx.getExtractor().accept(result, extractionContext);
		result.setClassValue(classValue);
		return result;
	}

}
