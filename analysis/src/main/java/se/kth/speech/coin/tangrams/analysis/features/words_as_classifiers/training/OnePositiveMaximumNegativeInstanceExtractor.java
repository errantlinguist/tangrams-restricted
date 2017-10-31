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

import java.util.ArrayList;
import java.util.List;
import java.util.Map.Entry;
import java.util.stream.Stream;

import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.coin.tangrams.analysis.GameHistory;
import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeature;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeature.Extractor.Context;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeatureExtractionContextFactory;
import se.kth.speech.coin.tangrams.analysis.features.weka.EntityInstanceAttributeContext;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues.EventDialogueTransformer;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;
import weka.core.Instance;
import weka.core.Instances;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since May 26, 2017
 * @see
 *      <ul>
 *      <li><a href="http://anthology.aclweb.org/W/W15/W15-0124.pdf">Casey
 *      Kennington, Livia Dia, &amp; David Schlangen. &ldquo;A Discriminative
 *      Model for Perceptually-Grounded Incremental Reference Resolution.&rdquo;
 *      In <em>Proceedings of IWCS 2015</em><a>.</li>
 *      <li><a href="http://www.aclweb.org/anthology/P15-1029">Casey Kennington,
 *      &amp; David Schlangen. &ldquo;Simple Learning and Compositional
 *      Application of Perceptually Grounded Word Meanings for Incremental
 *      Reference Resolution&rdquo;. In <em>Proceedings of the 53<sup>rd</sup>
 *      Annual Meeting of the Association for Computational Linguistics and the
 *      7<sup>th</sup> International Joint Conference on Natural Language
 *      Processing</em><a>.</li>
 *      </ul>
 *
 */
public final class OnePositiveMaximumNegativeInstanceExtractor extends AbstractInstanceExtractor {

	private static final Logger LOGGER = LoggerFactory.getLogger(OnePositiveMaximumNegativeInstanceExtractor.class);

	private static final String NEGATIVE_EXAMPLE_LABEL = Boolean.FALSE.toString();

	private static final String POSITIVE_EXAMPLE_LABEL = Boolean.TRUE.toString();

	private static Stream<String> getWordClasses(final List<Utterance> utts) {
		return utts.stream().map(Utterance::getTokens).flatMap(List::stream);
	}

	private final EventDialogueTransformer diagTransformer;

	private final BooleanTrainingContextsFactory trainingCtxsFactory;

	public OnePositiveMaximumNegativeInstanceExtractor(final EntityInstanceAttributeContext entityInstAttrCtx,
			final EventDialogueTransformer diagTransformer, final EntityFeatureExtractionContextFactory extCtxFactory) {
		this(entityInstAttrCtx, diagTransformer, new BooleanTrainingContextsFactory(extCtxFactory));
	}

	private OnePositiveMaximumNegativeInstanceExtractor(final EntityInstanceAttributeContext entityInstAttrCtx,
			final EventDialogueTransformer diagTransformer, final BooleanTrainingContextsFactory trainingCtxsFactory) {
		super(entityInstAttrCtx);
		this.diagTransformer = diagTransformer;
		this.trainingCtxsFactory = trainingCtxsFactory;
	}

	private void addWeightedExamples(final String wordClass, final WordClassificationData trainingData,
			final List<Context> trainingContexts, final double weight, final String classValue) {
		assert weight > 0.0;
		final Instances classInsts = trainingData.fetchWordInstances(wordClass);
		final List<Entry<Instance, String>> trainingInsts = new ArrayList<>(trainingContexts.size());
		for (final Context trainingContext : trainingContexts) {
			final Instance trainingInst = createTokenInstance(classInsts, trainingContext, classValue);
			trainingInst.setWeight(weight);
			trainingInsts.add(Pair.of(trainingInst, classValue));
		}
		// Add examples
		trainingData.addObservation(wordClass, trainingInsts.stream());
	}

	@Override
	protected void addTrainingData(final EventDialogue uttDialogue, final GameHistory history,
			final WordClassificationData trainingData, final double positiveExampleWeightFactor,
			final double negativeExampleWeightFactor) {
		uttDialogue.getFirstEvent().ifPresent(event -> {
			LOGGER.debug("Extracting features for utterances for event: {}", event);
			final EventDialogue transformedDiag = diagTransformer.apply(uttDialogue);
			final List<Utterance> utts = transformedDiag.getUtterances();
			if (utts.isEmpty()) {
				LOGGER.debug("No utterances to train with for {}.", transformedDiag);
			} else {
				// Just use the game context for the first utterance for all
				// utterances processed for the given dialogue
				final Utterance firstUtt = utts.get(0);
				LOGGER.debug("Creating positive and negative examples for entity selected by player \"{}\".",
						event.getGameAttrs().get(GameManagementEvent.Attribute.PLAYER_ID));
				final BooleanTrainingContexts trainingContexts = trainingCtxsFactory.apply(firstUtt, history);
				final double observationWeight = 1.0;
				// Instances for referent entity
				final List<EntityFeature.Extractor.Context> positiveCtxs = trainingContexts.getPositive();
				getWordClasses(utts).forEach(token -> addWeightedExamples(token, trainingData, positiveCtxs,
						observationWeight * positiveExampleWeightFactor, POSITIVE_EXAMPLE_LABEL));
				// Instances for non-referent entities
				getWordClasses(utts)
						.forEach(token -> addWeightedExamples(token, trainingData, trainingContexts.getNegative(),
								observationWeight * negativeExampleWeightFactor, NEGATIVE_EXAMPLE_LABEL));
			}
		});
	}
}