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

import java.util.Arrays;
import java.util.List;
import java.util.Random;
import java.util.function.BiFunction;
import java.util.stream.Stream;

import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import it.unimi.dsi.fastutil.ints.IntList;
import it.unimi.dsi.fastutil.objects.Object2IntMap;
import it.unimi.dsi.fastutil.objects.Object2IntMaps;
import it.unimi.dsi.fastutil.objects.Object2IntOpenHashMap;
import se.kth.speech.coin.tangrams.analysis.GameContext;
import se.kth.speech.coin.tangrams.analysis.GameHistory;
import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeature;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeature.Extractor.Context;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeatureExtractionContextFactory;
import se.kth.speech.coin.tangrams.analysis.features.weka.EntityInstanceAttributeContext;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.UtteranceGameContexts;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues.EventDialogueTransformer;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;
import se.kth.speech.fastutil.RandomIntLists;
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
public final class OnePositiveOneNegativeInstanceExtractor extends AbstractInstanceExtractor {

	private static final class BooleanTrainingContexts {

		private final EntityFeature.Extractor.Context neg;

		private final EntityFeature.Extractor.Context pos;

		private BooleanTrainingContexts(final EntityFeature.Extractor.Context pos,
				final EntityFeature.Extractor.Context neg) {
			this.pos = pos;
			this.neg = neg;
		}
	}

	/**
	 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
	 * @since Jul 13, 2017
	 *
	 */
	private static final class BooleanTrainingContextsFactory
			implements BiFunction<Utterance, GameHistory, BooleanTrainingContexts> {

		private final EntityFeatureExtractionContextFactory extCtxFactory;

		private final Random rnd;

		private BooleanTrainingContextsFactory(final EntityFeatureExtractionContextFactory extCtxFactory,
				final Random rnd) {
			this.extCtxFactory = extCtxFactory;
			this.rnd = rnd;
		}

		@Override
		public BooleanTrainingContexts apply(final Utterance utt, final GameHistory history) {
			final GameContext uttCtx = UtteranceGameContexts.createSingleContext(utt, history);
			final int selectedEntityId = uttCtx.findLastSelectedEntityId().getAsInt();
			LOGGER.debug("Creating positive and negative examples for entity ID \"{}\".", selectedEntityId);
			return new BooleanTrainingContexts(extCtxFactory.apply(uttCtx, selectedEntityId),
					extCtxFactory.apply(uttCtx, findRandomEntityId(uttCtx, selectedEntityId)));
		}

		private int findRandomEntityId(final GameContext ctx, final int complementId) {
			final IntList entityIds = ctx.getEntityIds();
			int result = RandomIntLists.getRandomValue(entityIds, rnd);
			while (result == complementId) {
				result = RandomIntLists.getRandomValue(entityIds, rnd);
			}
			return result;
		}

	}

	private static final Object2IntMap<String> EMPTY_WORD_CLASS_OBSERVATION_MAP = Object2IntMaps.emptyMap();

	private static final Logger LOGGER = LoggerFactory.getLogger(OnePositiveOneNegativeInstanceExtractor.class);

	private static final String NEGATIVE_EXAMPLE_LABEL = Boolean.FALSE.toString();

	private static final String POSITIVE_EXAMPLE_LABEL = Boolean.TRUE.toString();

	private static Stream<String> getWordClasses(final List<Utterance> utts) {
		return utts.stream().map(Utterance::getTokens).flatMap(List::stream);
	}

	private final EventDialogueTransformer diagTransformer;

	private final BooleanTrainingContextsFactory trainingCtxsFactory;

	public OnePositiveOneNegativeInstanceExtractor(final EntityInstanceAttributeContext entityInstAttrCtx,
			final EventDialogueTransformer diagTransformer, final EntityFeatureExtractionContextFactory extCtxFactory,
			final Random rnd) {
		this(entityInstAttrCtx, diagTransformer, new BooleanTrainingContextsFactory(extCtxFactory, rnd));
	}

	private OnePositiveOneNegativeInstanceExtractor(final EntityInstanceAttributeContext entityInstAttrCtx,
			final EventDialogueTransformer diagTransformer, final BooleanTrainingContextsFactory trainingCtxsFactory) {
		super(entityInstAttrCtx);
		this.diagTransformer = diagTransformer;
		this.trainingCtxsFactory = trainingCtxsFactory;
	}

	private void addWeightedExamples(final String wordClass, final WordClassificationData trainingData,
			final Context trainingContext, final double weight, final String classValue) {
		assert weight > 0.0;
		final Instances classInsts = trainingData.fetchWordInstances(wordClass);
		final Instance trainingInst = createTokenInstance(classInsts, trainingContext, classValue);
		trainingInst.setWeight(weight);
		// Add example
		trainingData.addWordClassExamples(wordClass, Stream.of(Pair.of(trainingInst, classValue)));
	}

	@Override
	protected Object2IntMap<String> addTrainingData(final EventDialogue eventDialogue, final GameHistory history,
			final WordClassificationData trainingData, final double positiveExampleWeightFactor,
			final double negativeExampleWeightFactor) {
		return eventDialogue.getFirstEvent().map(event -> {
			LOGGER.debug("Extracting features for utterances for event: {}", event);
			final EventDialogue transformedDiag = diagTransformer.apply(eventDialogue);
			final List<Utterance> utts = transformedDiag.getUtterances();

			final Object2IntMap<String> wordClassObservationCounts;
			if (utts.isEmpty()) {
				LOGGER.debug("No utterances to train with for {}.", transformedDiag);
				wordClassObservationCounts = EMPTY_WORD_CLASS_OBSERVATION_MAP;
			} else {
				// Just use the game context for the first utterance for all
				// utterances processed for the given dialogue
				final Utterance firstUtt = utts.get(0);
				final GameContext uttCtx = UtteranceGameContexts.createSingleContext(firstUtt, history);
				final int selectedEntityId = uttCtx.findLastSelectedEntityId().getAsInt();
				LOGGER.debug(
						"Creating positive and negative examples for entity ID \"{}\", which is selected by player \"{}\".",
						selectedEntityId, event.getGameAttrs().get(GameManagementEvent.Attribute.PLAYER_ID));
				final BooleanTrainingContexts trainingContexts = trainingCtxsFactory.apply(firstUtt, history);
				final double observationWeight = 1.0;
				final List<String> wordClassObservations = Arrays.asList(getWordClasses(utts).toArray(String[]::new));
				wordClassObservationCounts = new Object2IntOpenHashMap<>(wordClassObservations.size() / 2);
				// Instances for referent entity
				wordClassObservations.forEach(token -> addWeightedExamples(token, trainingData, trainingContexts.pos,
						observationWeight * positiveExampleWeightFactor, POSITIVE_EXAMPLE_LABEL));
				// Instances for non-referent entities
				wordClassObservations.forEach(token -> addWeightedExamples(token, trainingData, trainingContexts.neg,
						observationWeight * negativeExampleWeightFactor, NEGATIVE_EXAMPLE_LABEL));

				wordClassObservations.forEach(
						token -> wordClassObservationCounts.put(token, wordClassObservationCounts.getInt(token) + 1));
				trainingData.addWordClassObservationCounts(wordClassObservationCounts);
				trainingData.incrementTrainingInstancesChangeCounts(wordClassObservationCounts.keySet());
			}
			return wordClassObservationCounts;
		}).orElse(EMPTY_WORD_CLASS_OBSERVATION_MAP);
	}
}
