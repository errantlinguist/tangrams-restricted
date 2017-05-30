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
import se.kth.speech.coin.tangrams.analysis.features.EntityFeatureExtractionContextFactory;
import se.kth.speech.coin.tangrams.analysis.features.weka.EntityInstanceAttributeContext;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.UtteranceGameContexts;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.EventDialogueTransformer;
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
public final class OnePositiveMaximumNegativeInstancesFactory extends AbstractSizeEstimatingInstancesMapFactory {

	private static final Logger LOGGER = LoggerFactory.getLogger(OnePositiveMaximumNegativeInstancesFactory.class);

	private final EventDialogueTransformer diagTransformer;

	private final EntityFeatureExtractionContextFactory extCtxFactory;

	public OnePositiveMaximumNegativeInstancesFactory(final EntityInstanceAttributeContext entityInstAttrCtx,
			final EventDialogueTransformer diagTransformer, final EntityFeatureExtractionContextFactory extCtxFactory) {
		super(entityInstAttrCtx);
		this.diagTransformer = diagTransformer;
		this.extCtxFactory = extCtxFactory;
	}

	private List<Entry<EntityFeature.Extractor.Context, String>> createTrainingContexts(final GameContext uttCtx,
			final int selectedEntityId) {
		final IntList entityIds = uttCtx.getEntityIds();
		final List<Entry<EntityFeature.Extractor.Context, String>> result = new ArrayList<>(entityIds.size());
		for (final int entityId : uttCtx.getEntityIds()) {
			final EntityFeature.Extractor.Context context = extCtxFactory.apply(uttCtx, entityId);
			final boolean examplePolarity = entityId == selectedEntityId;
			final String classValue = Boolean.toString(examplePolarity);
			result.add(new MutablePair<>(context, classValue));
		}
		return result;
	}

	private List<Entry<EntityFeature.Extractor.Context, String>> createTrainingContexts(final Utterance utt,
			final GameHistory history) {
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
					final Utterance firstUtt = allUtts.get(0);
					LOGGER.debug("Creating positive and negative examples for entity selected by player \"{}\".",
							event.getString(GameManagementEvent.Attribute.PLAYER_ID.toString()));
					final List<Entry<EntityFeature.Extractor.Context, String>> trainingContexts = createTrainingContexts(
							firstUtt, history);
					final Stream<String> wordClasses = allUtts.stream().map(Utterance::getTokens).flatMap(List::stream);
					wordClasses.forEach(wordClass -> {
						final Instances classInsts = trainingData.fetchWordInstances(wordClass);
						final Stream<Entry<Instance, String>> trainingInsts = trainingContexts.stream()
								.map(trainingContext -> {
									final String classValue = trainingContext.getValue();
									return new MutablePair<>(
											createTokenInstance(classInsts, trainingContext.getKey(), classValue),
											classValue);
								});
						// Add examples
						trainingData.addObservation(wordClass, trainingInsts);
					});
				}
			});
		});
	}
}
