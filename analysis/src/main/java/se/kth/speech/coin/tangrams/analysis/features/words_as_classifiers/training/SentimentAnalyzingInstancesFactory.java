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
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.MutablePair;
import se.kth.speech.coin.tangrams.analysis.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.GameHistory;
import se.kth.speech.coin.tangrams.analysis.SessionEventDialogueManager;
import se.kth.speech.coin.tangrams.analysis.Utterance;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeature;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeature.Extractor.Context;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeatureExtractionContextFactory;
import se.kth.speech.coin.tangrams.analysis.features.weka.EntityInstanceAttributeContext;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.EventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.SentimentAnalyzingEventDialogueUtteranceSorter;
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

	private static final Logger LOGGER = LoggerFactory.getLogger(SentimentAnalyzingInstancesFactory.class);

	private static final String NEGATIVE_EXAMPLE_LABEL = Boolean.FALSE.toString();

	private static final String POSITIVE_EXAMPLE_LABEL = Boolean.TRUE.toString();

	private static Stream<String> getWordClasses(final List<Utterance> utts) {
		return utts.stream().map(Utterance::getTokens).flatMap(List::stream);
	}

	private final EventDialogueTransformer diagTransformer;

	private final BooleanTrainingContextsFactory trainingCtxsFactory;

	private final ToDoubleFunction<? super Utterance> uttSentimentRanker;

	public SentimentAnalyzingInstancesFactory(final EntityInstanceAttributeContext entityInstAttrCtx,
			final EventDialogueTransformer diagTransformer, final EntityFeatureExtractionContextFactory extCtxFactory,
			final ToDoubleFunction<? super Utterance> uttSentimentRanker) {
		this(entityInstAttrCtx, diagTransformer, new BooleanTrainingContextsFactory(extCtxFactory), uttSentimentRanker);
	}

	private SentimentAnalyzingInstancesFactory(final EntityInstanceAttributeContext entityInstAttrCtx,
			final EventDialogueTransformer diagTransformer, final BooleanTrainingContextsFactory trainingCtxsFactory,
			final ToDoubleFunction<? super Utterance> uttSentimentRanker) {
		super(entityInstAttrCtx);
		this.diagTransformer = diagTransformer;
		this.trainingCtxsFactory = trainingCtxsFactory;
		this.uttSentimentRanker = uttSentimentRanker;
	}

	private void addWeightedExamples(final String wordClass, final WordClassificationData trainingData,
			final List<Context> trainingContexts, final double weight, final String classValue) {
		assert weight > 0.0;
		final Instances classInsts = trainingData.fetchWordInstances(wordClass);
		final List<Entry<Instance, String>> trainingInsts = new ArrayList<>(trainingContexts.size());
		for (final Context trainingContext : trainingContexts) {
			final Instance trainingInst = createTokenInstance(classInsts, trainingContext, classValue);
			trainingInst.setWeight(weight);
			trainingInsts.add(new MutablePair<>(trainingInst, classValue));
		}
		// Add examples
		trainingData.addObservation(wordClass, trainingInsts.stream());
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
					final BooleanTrainingContexts trainingContexts = trainingCtxsFactory.apply(allUtts.get(0), history);
					final SentimentAnalyzingEventDialogueUtteranceSorter uttSorter = new SentimentAnalyzingEventDialogueUtteranceSorter(
							uttSentimentRanker);
					final SentimentAnalyzingEventDialogueUtteranceSorter.Result sortedUtts = uttSorter.apply(allUtts,
							UtteranceMatchers.createEventSubmitterUtteranceMatcher(event));
					final double observationWeight = 1.0;
					{
						// Instances for referent entity
						final List<EntityFeature.Extractor.Context> positiveCtxs = trainingContexts.getPositive();
						getWordClasses(sortedUtts.getRefPosExamples()).forEach(token -> addWeightedExamples(token,
								trainingData, positiveCtxs, observationWeight, POSITIVE_EXAMPLE_LABEL));
						getWordClasses(sortedUtts.getRefNegExamples()).forEach(token -> addWeightedExamples(token,
								trainingData, positiveCtxs, observationWeight, NEGATIVE_EXAMPLE_LABEL));
					}
					// Instances for non-referent entities
					sortedUtts.getOtherEntityNegativeExamples().stream().map(Utterance::getTokens).flatMap(List::stream)
							.forEach(token -> addWeightedExamples(token, trainingData, trainingContexts.getNegative(),
									observationWeight, NEGATIVE_EXAMPLE_LABEL));
				}
			});
		});
	}
}
