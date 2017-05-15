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
package se.kth.speech.coin.tangrams.analysis.features;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.function.Function;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Maps;

import se.kth.speech.coin.tangrams.analysis.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.GameContext;
import se.kth.speech.coin.tangrams.analysis.GameContextModelFactory;
import se.kth.speech.coin.tangrams.analysis.GameHistory;
import se.kth.speech.coin.tangrams.analysis.SessionEventDialogueManager;
import se.kth.speech.coin.tangrams.analysis.TemporalGameContexts;
import se.kth.speech.coin.tangrams.analysis.Utterance;
import se.kth.speech.coin.tangrams.content.IconImages;
import weka.core.Attribute;
import weka.core.DenseInstance;
import weka.core.Instance;
import weka.core.Instances;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Apr 17, 2017
 * @see <a href="http://anthology.aclweb.org/W/W15/W15-0124.pdf">Casey
 *      Kennington, Livia Dia, &amp; David Schlangen. &ldquo;A Discriminative
 *      Model for Perceptually-Grounded Incremental Reference Resolution.&rdquo;
 *      In <em>Proceedings of IWCS 2015</em><a>.
 *
 */
public final class WordsAsClassifiersInstancesMapFactory
		implements Function<Collection<SessionEventDialogueManager>, Map<String, Instances>> {

	private static class MultiClassDataCollector {

		private static GameContext createGameContext(final Utterance dialogueUtt, final GameHistory history,
				final String perspectivePlayerId) {
			LOGGER.debug(
					"Creating a context based on the logged game history, which is then seen from the perspective of player \"{}\".",
					perspectivePlayerId);
			final GameContext[] ctxs = TemporalGameContexts
					.create(history, dialogueUtt.getStartTime(), dialogueUtt.getEndTime(), perspectivePlayerId)
					.toArray(GameContext[]::new);
			if (ctxs.length > 1) {
				LOGGER.warn("More than one game context found for {}; Only using the first one.", dialogueUtt);
			}
			return ctxs[0];
		}

		private final Function<? super String, Instances> classInstanceGetter;

		private final Function<GameContext, Integer> negativeExampleEntityIdGetter;

		private final int numAttributes;

		private MultiClassDataCollector(final Function<? super String, Instances> classInstanceGetter,
				final int numAttributes, final Function<GameContext, Integer> negativeExampleEntityIdGetter) {
			this.negativeExampleEntityIdGetter = negativeExampleEntityIdGetter;
			this.classInstanceGetter = classInstanceGetter;
			this.numAttributes = numAttributes;
		}

		private void accept(final SessionEventDialogueManager sessionEventDiagMgr) {
			final Set<String> playerIds = sessionEventDiagMgr.getPlayerSourceIds().keySet();
			final EntityFeatureExtractionContextFactory extractionContextFactory = new EntityFeatureExtractionContextFactory(
					new GameContextModelFactory(sessionEventDiagMgr.getUniqueGameModelDescriptionCount()),
					IMG_EDGE_COUNTER);

			for (final Entry<String, GameHistory> gameHistory : sessionEventDiagMgr.getGameHistories().entrySet()) {
				final String gameId = gameHistory.getKey();
				LOGGER.debug("Processing game \"{}\".", gameId);
				final GameHistory history = gameHistory.getValue();
				for (final String perspectivePlayerId : playerIds) {
					LOGGER.info("Processing game from perspective of player \"{}\".", perspectivePlayerId);
					final List<EventDialogue> uttDialogues = sessionEventDiagMgr.createUttDialogues(history,
							perspectivePlayerId);
					uttDialogues.forEach(uttDialogue -> {
						final List<Utterance> dialogueUtts = uttDialogue.getUtts();
						dialogueUtts.forEach(dialogueUtt -> {
							final String uttPlayerId = dialogueUtt.getSpeakerId();
							if (perspectivePlayerId.equals(uttPlayerId)) {
								final GameContext uttCtx = createGameContext(dialogueUtt, history, perspectivePlayerId);
								uttCtx.findLastSelectedEntityId().ifPresent(selectedEntityId -> {
									LOGGER.debug(
											"Creating positive and negative examples for entity ID \"{}\", which is selected by player \"{}\".",
											selectedEntityId, perspectivePlayerId);
									// Add positive training examples
									final EntityFeature.Extractor.Context positiveContext = extractionContextFactory
											.createExtractionContext(uttCtx, selectedEntityId);
									addTokenInstances(dialogueUtt, uttCtx, positiveContext, Boolean.TRUE.toString());
									// Add negative training examples
									final EntityFeature.Extractor.Context negativeContext = extractionContextFactory
											.createExtractionContext(uttCtx,
													negativeExampleEntityIdGetter.apply(uttCtx));
									addTokenInstances(dialogueUtt, uttCtx, negativeContext, Boolean.FALSE.toString());
								});
							} else {
								LOGGER.debug(
										"Skipping the extraction of features for utterance with segment ID \"{}\" because the utterance is from player \"{}\" rather than the player whose perspective is being used for extracting features (\"{}\")",
										dialogueUtt.getSegmentId(), uttPlayerId, perspectivePlayerId);
							}
						});
					});
				}
			}
		}

		private void addTokenInstances(final Utterance utt, final GameContext uttContext,
				final EntityFeature.Extractor.Context extractionContext, final String classValue) {
			utt.getTokens().forEach(token -> {
				final Instances classInstances = classInstanceGetter.apply(token);
				final Instance inst = new DenseInstance(numAttributes);
				inst.setDataset(classInstances);
				EXTRACTOR.accept(inst, extractionContext);
				inst.setClassValue(classValue);
				classInstances.add(inst);
			});
		}
	}

	public static final String CLASS_RELATION_PREFIX = "referent_for_token-";

	private static final ArrayList<Attribute> ATTRS;

	private static final Attribute CLASS_ATTR;

	private static final String CLASS_ATTR_NAME = "IS_REFERENT";

	private static final EntityFeature.Extractor EXTRACTOR;

	private static final ImageEdgeCounter IMG_EDGE_COUNTER = new ImageEdgeCounter();

	private static final Logger LOGGER = LoggerFactory.getLogger(WordsAsClassifiersInstancesMapFactory.class);

	static {
		final List<String> shapeFeatureVals = new ArrayList<>(IconImages.getImageResources().keySet());
		shapeFeatureVals.sort(Comparator.naturalOrder());
		EXTRACTOR = new EntityFeature.Extractor(shapeFeatureVals);
		final Map<EntityFeature, Attribute> featureAttrs = EXTRACTOR.getFeatureAttrs();
		ATTRS = new ArrayList<>(featureAttrs.size() + 1);
		ATTRS.addAll(featureAttrs.values());
		CLASS_ATTR = new Attribute(CLASS_ATTR_NAME, Arrays.asList(Boolean.TRUE.toString(), Boolean.FALSE.toString()));
		ATTRS.add(CLASS_ATTR);
	}

	/**
	 * @return the classAttrName
	 */
	public static String getClassAttrName() {
		return CLASS_ATTR_NAME;
	}

	private static int estimateVocabTokenCount(final String token, final Collection<?> sessionData) {
		return sessionData.size() * 10;
	}

	private static int estimateVocabTypeCount(final Collection<?> sessionData) {
		return Math.toIntExact(Math.round(Math.ceil(Math.log(sessionData.size() * 850))));
	}

	private final Function<GameContext, Integer> negativeExampleEntityIdGetter;

	public WordsAsClassifiersInstancesMapFactory(final Function<GameContext, Integer> negativeExampleEntityIdGetter) {
		this.negativeExampleEntityIdGetter = negativeExampleEntityIdGetter;
	}

	@Override
	public Map<String, Instances> apply(final Collection<SessionEventDialogueManager> sessionEventDiagMgrs) {
		final Map<String, Instances> result = Maps
				.newHashMapWithExpectedSize(estimateVocabTypeCount(sessionEventDiagMgrs));
		final Function<String, Instances> classInstanceFetcher = className -> result.computeIfAbsent(className, k -> {
			final Instances instances = new Instances(CLASS_RELATION_PREFIX + k, ATTRS,
					estimateVocabTokenCount(k, sessionEventDiagMgrs));
			instances.setClass(CLASS_ATTR);
			return instances;
		});
		final MultiClassDataCollector coll = new MultiClassDataCollector(classInstanceFetcher, ATTRS.size(),
				negativeExampleEntityIdGetter);
		for (final SessionEventDialogueManager sessionEventDiagMgr : sessionEventDiagMgrs) {
			coll.accept(sessionEventDiagMgr);
		}
		return result;
	}

}
