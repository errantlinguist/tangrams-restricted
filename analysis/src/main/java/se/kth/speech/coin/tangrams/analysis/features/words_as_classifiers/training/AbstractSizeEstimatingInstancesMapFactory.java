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

import javax.inject.Inject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Maps;

import se.kth.speech.coin.tangrams.analysis.SessionEventDialogueManager;
import se.kth.speech.coin.tangrams.analysis.features.weka.EntityInstanceAttributeContext;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.WordClasses;
import weka.core.Instances;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since May 26, 2017
 *
 */
public abstract class AbstractSizeEstimatingInstancesMapFactory
		implements Function<Collection<SessionEventDialogueManager>, Map<String, Instances>> {

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

	private static int estimateVocabTypeTokenCount(final String token, final Collection<?> sessionData) {
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

	@Inject
	protected EntityInstanceAttributeContext entityInstAttrCtx;

	@Override
	public Map<String, Instances> apply(final Collection<SessionEventDialogueManager> sessionEventDiagMgrs) {
		final Map<String, Instances> result = Maps
				.newHashMapWithExpectedSize(estimateVocabTypeCount(sessionEventDiagMgrs));
		final Function<String, Instances> classInstancesFetcher = className -> result.computeIfAbsent(className, k -> {
			final Instances instances = new Instances(WordClasses.createRelationName(k), entityInstAttrCtx.getAttrs(),
					estimateVocabTypeTokenCount(k, sessionEventDiagMgrs));
			instances.setClass(entityInstAttrCtx.getClassAttr());
			return instances;
		});
		for (final SessionEventDialogueManager sessionEventDiagMgr : sessionEventDiagMgrs) {
			addTrainingData(sessionEventDiagMgr, classInstancesFetcher);
		}
		return result;
	}

	protected abstract void addTrainingData(SessionEventDialogueManager sessionEventDiagMgr,
			Function<? super String, ? extends Instances> classInstancesGetter);

}
