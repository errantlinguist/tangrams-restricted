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
import java.util.function.Function;
import java.util.stream.Stream;

import javax.inject.Inject;

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
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.UtteranceGameContexts;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;
import weka.core.DenseInstance;
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

	@Inject
	private Function<? super EventDialogue, EventDialogue> diagTransformer;

	@Inject
	private EntityFeatureExtractionContextFactory extCtxFactory;

	private List<Entry<EntityFeature.Extractor.Context, String>> createContexts(final GameContext uttCtx,
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

	private Instance createTokenInstance(final Instances classInsts,
			final EntityFeature.Extractor.Context extractionContext, final String classValue) {
		final Instance result = new DenseInstance(entityInstAttrCtx.getAttrs().size());
		result.setDataset(classInsts);
		entityInstAttrCtx.getExtractor().accept(result, extractionContext);
		result.setClassValue(classValue);
		return result;
	}

	@Override
	protected void addTrainingData(final SessionEventDialogueManager sessionEventDiagMgr,
			final WordClassificationData trainingData) {
		final String gameId = sessionEventDiagMgr.getGameId();
		LOGGER.debug("Processing game \"{}\".", gameId);
		final GameHistory history = sessionEventDiagMgr.getGameHistory();

		final Stream<EventDialogue> uttDialogues = sessionEventDiagMgr.createUttDialogues();
		uttDialogues.forEachOrdered(uttDialogue -> {
			uttDialogue.getLastEvent().ifPresent(event -> {
				LOGGER.debug("Extracting features for utterances for event: {}", event);
				final EventDialogue transformedDiag = diagTransformer.apply(uttDialogue);
				final List<Utterance> allUtts = transformedDiag.getUtts();
				if (allUtts.isEmpty()) {
					LOGGER.debug("No utterances to train with for {}.", transformedDiag);
				} else {
					// Just use the game context for the first utterance for all
					// utterances processed for the given dialogue
					final Utterance firstUtt = allUtts.get(0);
					final GameContext uttCtx = UtteranceGameContexts.createSingleContext(firstUtt, history);
					final int selectedEntityId = uttCtx.findLastSelectedEntityId().get();
					LOGGER.debug(
							"Creating positive and negative examples for entity ID \"{}\", which is selected by player \"{}\".",
							selectedEntityId, event.getString(GameManagementEvent.Attribute.PLAYER_ID.toString()));
					final List<Entry<EntityFeature.Extractor.Context, String>> contexts = createContexts(uttCtx,
							selectedEntityId);
					final Stream<String> wordClasses = transformedDiag.getUtts().stream().map(Utterance::getTokens)
							.flatMap(List::stream);
					wordClasses.forEach(wordClass -> {
						final Instances classInsts = trainingData.fetchWordInstances(wordClass);
						final Stream<Instance> trainingInsts = contexts.stream()
								.map(context -> createTokenInstance(classInsts, context.getKey(), context.getValue()));
						// Add examples
						trainingData.addObservation(wordClass, trainingInsts);
					});
				}
			});
		});
	}
}
