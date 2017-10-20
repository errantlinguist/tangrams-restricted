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
import java.util.Collection;
import java.util.List;
import java.util.Map.Entry;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.function.ToDoubleFunction;

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
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues.DialogicEventDialogueUtteranceSorter;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues.EntityReferringLanguageWordClasses;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues.EventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues.UtteranceRelation;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;
import weka.core.Instance;
import weka.core.Instances;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since May 30, 2017
 *
 */
public final class DialogicInstanceExtractor extends AbstractInstanceExtractor {

	private static final Logger LOGGER = LoggerFactory.getLogger(DialogicInstanceExtractor.class);

	private static final String NEGATIVE_EXAMPLE_LABEL = Boolean.FALSE.toString();

	private static final String POSITIVE_EXAMPLE_LABEL = Boolean.TRUE.toString();

	private final EventDialogueTransformer diagTransformer;

	private final Function<? super Collection<UtteranceRelation>, EntityReferringLanguageWordClasses> entityRefLangExFactory;

	private final BooleanTrainingContextsFactory trainingCtxsFactory;

	private final ToDoubleFunction<? super Utterance> uttAcceptanceRanker;

	private final BiConsumer<? super EventDialogue, ? super List<UtteranceRelation>> uttRelHandler;

	public DialogicInstanceExtractor(final EntityInstanceAttributeContext entityInstAttrCtx,
			final EventDialogueTransformer diagTransformer, final EntityFeatureExtractionContextFactory extCtxFactory,
			final ToDoubleFunction<? super Utterance> uttAcceptanceRanker,
			final Function<? super Collection<UtteranceRelation>, EntityReferringLanguageWordClasses> entityRefLangExFactory,
			final BiConsumer<? super EventDialogue, ? super List<UtteranceRelation>> uttRelHandler) {
		this(entityInstAttrCtx, diagTransformer, new BooleanTrainingContextsFactory(extCtxFactory), uttAcceptanceRanker,
				entityRefLangExFactory, uttRelHandler);
	}

	private DialogicInstanceExtractor(final EntityInstanceAttributeContext entityInstAttrCtx,
			final EventDialogueTransformer diagTransformer, final BooleanTrainingContextsFactory trainingCtxsFactory,
			final ToDoubleFunction<? super Utterance> uttAcceptanceRanker,
			final Function<? super Collection<UtteranceRelation>, EntityReferringLanguageWordClasses> entityRefLangExFactory,
			final BiConsumer<? super EventDialogue, ? super List<UtteranceRelation>> uttRelHandler) {
		super(entityInstAttrCtx);
		this.diagTransformer = diagTransformer;
		this.trainingCtxsFactory = trainingCtxsFactory;
		this.uttAcceptanceRanker = uttAcceptanceRanker;
		this.entityRefLangExFactory = entityRefLangExFactory;
		this.uttRelHandler = uttRelHandler;
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
	protected void addTrainingData(final EventDialogue eventDialogue, final GameHistory history,
			final WordClassificationData trainingData) {
		eventDialogue.getFirstEvent().ifPresent(event -> {
			LOGGER.debug("Extracting features for utterances for event: {}", event);
			final EventDialogue transformedDiag = diagTransformer.apply(eventDialogue);
			final List<Utterance> allUtts = transformedDiag.getUtterances();
			if (allUtts.isEmpty()) {
				LOGGER.debug("No utterances to train with for {}.", transformedDiag);
			} else {
				// Just use the game context for the first utterance for all
				// utterances processed for the given dialogue
				LOGGER.debug("Creating positive and negative examples for entity selected by player \"{}\".",
						event.getGameAttrs().get(GameManagementEvent.Attribute.PLAYER_ID));
				final BooleanTrainingContexts trainingContexts = trainingCtxsFactory.apply(allUtts.get(0), history);
				final DialogicEventDialogueUtteranceSorter uttSorter = new DialogicEventDialogueUtteranceSorter(
						uttAcceptanceRanker);
				final List<UtteranceRelation> uttRels = uttSorter.apply(allUtts, event);
				uttRelHandler.accept(eventDialogue, uttRels);

				final EntityReferringLanguageWordClasses entityRefLangExs = entityRefLangExFactory.apply(uttRels);
				{
					// Instances for referent entity
					final List<EntityFeature.Extractor.Context> positiveCtxs = trainingContexts.getPositive();
					entityRefLangExs.getRefPosExamples().object2DoubleEntrySet().stream()
							.forEach(langEx -> addWeightedExamples(langEx.getKey(), trainingData, positiveCtxs,
									langEx.getDoubleValue(), POSITIVE_EXAMPLE_LABEL));
					entityRefLangExs.getRefNegExamples().object2DoubleEntrySet().stream()
							.forEach(langEx -> addWeightedExamples(langEx.getKey(), trainingData, positiveCtxs,
									langEx.getDoubleValue(), NEGATIVE_EXAMPLE_LABEL));
				}
				{
					// Instances for non-referent entities
					final List<EntityFeature.Extractor.Context> negativeCtxs = trainingContexts.getNegative();
					entityRefLangExs.getOtherEntityNegativeExamples().object2DoubleEntrySet().stream()
							.forEach(langEx -> addWeightedExamples(langEx.getKey(), trainingData, negativeCtxs,
									langEx.getDoubleValue(), NEGATIVE_EXAMPLE_LABEL));
				}
			}
		});
	}
}
