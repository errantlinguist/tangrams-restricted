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
import java.util.function.ToDoubleFunction;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import it.unimi.dsi.fastutil.ints.IntList;
import se.kth.speech.MutablePair;
import se.kth.speech.coin.tangrams.analysis.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.GameContext;
import se.kth.speech.coin.tangrams.analysis.GameHistory;
import se.kth.speech.coin.tangrams.analysis.SessionEventDialogueManager;
import se.kth.speech.coin.tangrams.analysis.Utterance;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeature;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeature.Extractor.Context;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeatureExtractionContextFactory;
import se.kth.speech.coin.tangrams.analysis.features.weka.EntityInstanceAttributeContext;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.UtteranceGameContexts;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.EventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.SentimentAnalyzingEventDialogueUtteranceSorter;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.SentimentAnalyzingEventDialogueUtteranceSorter.ExampleHandler;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.UtteranceMatchers;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;
import weka.core.Instance;
import weka.core.Instances;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since May 30, 2017
 *
 */
public final class SentimentAnalyzingInstancesFactory extends AbstractSizeEstimatingInstancesMapFactory {

	private static class BooleanTrainingContexts {

		private final List<EntityFeature.Extractor.Context> negative;

		private final List<EntityFeature.Extractor.Context> positive;

		private BooleanTrainingContexts(final List<EntityFeature.Extractor.Context> positive,
				final List<EntityFeature.Extractor.Context> negative) {
			this.positive = positive;
			this.negative = negative;
		}

	}

	private static final Logger LOGGER = LoggerFactory.getLogger(SentimentAnalyzingInstancesFactory.class);

	private static final String NEGATIVE_EXAMPLE_LABEL = Boolean.FALSE.toString();

	private static final String POSITIVE_EXAMPLE_LABEL = Boolean.TRUE.toString();

	private final EventDialogueTransformer diagTransformer;

	private final EntityFeatureExtractionContextFactory extCtxFactory;

	private final ToDoubleFunction<? super Utterance> uttSentimentRanker;

	public SentimentAnalyzingInstancesFactory(final EntityInstanceAttributeContext entityInstAttrCtx,
			final EventDialogueTransformer diagTransformer, final EntityFeatureExtractionContextFactory extCtxFactory,
			final ToDoubleFunction<? super Utterance> uttSentimentRanker) {
		super(entityInstAttrCtx);
		this.diagTransformer = diagTransformer;
		this.extCtxFactory = extCtxFactory;
		this.uttSentimentRanker = uttSentimentRanker;
	}

	private void addWeightedExamples(final String wordClass, final WordClassificationData trainingData,
			final List<Context> trainingContexts, final double weight, final String classValue) {
		assert weight > 0.0;
		final Instances classInsts = trainingData.fetchWordInstances(wordClass);
		final List<Entry<Instance, String>> trainingInsts = new ArrayList<>(trainingContexts.size());
		for (final Context trainingContext : trainingContexts) {
			final Instance trainingInst = createTokenInstance(classInsts, trainingContext, classValue);
			trainingInsts.add(new MutablePair<>(trainingInst, classValue));
			trainingInst.setWeight(weight);
		}
		// Add examples
		trainingData.addObservation(wordClass, trainingInsts.stream());
	}

	private BooleanTrainingContexts createTrainingContexts(final GameContext uttCtx, final int selectedEntityId) {
		final IntList entityIds = uttCtx.getEntityIds();
		final List<EntityFeature.Extractor.Context> positive = new ArrayList<>(1);
		final List<EntityFeature.Extractor.Context> negative = new ArrayList<>(entityIds.size() - 1);
		for (final int entityId : uttCtx.getEntityIds()) {
			final EntityFeature.Extractor.Context context = extCtxFactory.apply(uttCtx, entityId);
			final boolean examplePolarity = entityId == selectedEntityId;
			if (examplePolarity) {
				positive.add(context);
			} else {
				negative.add(context);
			}
		}
		return new BooleanTrainingContexts(positive, negative);
	}

	private BooleanTrainingContexts createTrainingContexts(final Utterance utt, final GameHistory history) {
		final GameContext uttCtx = UtteranceGameContexts.createSingleContext(utt, history);
		final int selectedEntityId = uttCtx.findLastSelectedEntityId().get();
		LOGGER.debug("Creating positive and negative examples for entity ID \"{}\".", selectedEntityId);
		return createTrainingContexts(uttCtx, selectedEntityId);
	}

	@Override
	protected void addTrainingData(final SessionEventDialogueManager sessionEventDiagMgr,
			final WordClassificationData trainingData) {
		final String gameId = sessionEventDiagMgr.getGameId();
		LOGGER.debug("Processing game \"{}\".", gameId);
		final GameHistory history = sessionEventDiagMgr.getGameHistory();

		final List<EventDialogue> uttDialogues = sessionEventDiagMgr.getUttDialogues();
		uttDialogues.forEach(uttDialogue -> {
			uttDialogue.getFirstEvent().ifPresent(event -> {
				LOGGER.debug("Extracting features for utterances for event: {}", event);
				final EventDialogue transformedDiag = diagTransformer.apply(uttDialogue);
				final List<Utterance> allUtts = transformedDiag.getUtts();
				if (allUtts.isEmpty()) {
					LOGGER.debug("No utterances to train with for {}.", transformedDiag);
				} else {
					// Just use the game context for the first utterance for all
					// utterances processed for the given dialogue
					LOGGER.debug("Creating positive and negative examples for entity selected by player \"{}\".",
							event.getString(GameManagementEvent.Attribute.PLAYER_ID.toString()));
					final BooleanTrainingContexts trainingContexts = createTrainingContexts(allUtts.get(0), history);
					final ExampleHandler referentPositiveExampleHandler = (wordClass, weight) -> addWeightedExamples(
							wordClass, trainingData, trainingContexts.positive, weight, POSITIVE_EXAMPLE_LABEL);
					final ExampleHandler referentNegativeExampleHandler = (wordClass, weight) -> addWeightedExamples(
							wordClass, trainingData, trainingContexts.positive, weight, NEGATIVE_EXAMPLE_LABEL);
					final ExampleHandler otherEntityNegativeExampleHandler = (wordClass, weight) -> addWeightedExamples(
							wordClass, trainingData, trainingContexts.negative, weight, NEGATIVE_EXAMPLE_LABEL);
					final SentimentAnalyzingEventDialogueUtteranceSorter uttSorter = new SentimentAnalyzingEventDialogueUtteranceSorter(
							uttSentimentRanker, referentPositiveExampleHandler, referentNegativeExampleHandler,
							otherEntityNegativeExampleHandler);
					uttSorter.accept(allUtts.listIterator(),
							UtteranceMatchers.createEventSubmitterUtteranceMatcher(event));
				}
			});
		});
	}
}
