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

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.ToIntFunction;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import javax.inject.Inject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Maps;

import se.kth.speech.coin.tangrams.analysis.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.GameContext;
import se.kth.speech.coin.tangrams.analysis.GameHistory;
import se.kth.speech.coin.tangrams.analysis.SessionEventDialogueManager;
import se.kth.speech.coin.tangrams.analysis.TemporalGameContexts;
import se.kth.speech.coin.tangrams.analysis.Utterance;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;
import weka.core.DenseInstance;
import weka.core.Instance;
import weka.core.Instances;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Apr 17, 2017
 * @see <a href="http://www.aclweb.org/anthology/P15-1029">Casey Kennington,
 *      &amp; David Schlangen. &ldquo;Simple Learning and Compositional
 *      Application of Perceptually Grounded Word Meanings for Incremental
 *      Reference Resolution&rdquo;. In <em>Proceedings of the 53<sup>rd</sup>
 *      Annual Meeting of the Association for Computational Linguistics and the
 *      7<sup>th</sup> International Joint Conference on Natural Language
 *      Processing</em><a>.
 *
 */
public final class WordsAsClassifiersInstancesMapFactory
		implements Function<Collection<SessionEventDialogueManager>, Map<String, Instances>> {

	private class MultiClassDataCollector implements Consumer<SessionEventDialogueManager> {

		private final Function<? super String, Instances> classInstanceGetter;

		private MultiClassDataCollector(final Function<? super String, Instances> classInstanceGetter) {
			this.classInstanceGetter = classInstanceGetter;
		}

		@Override
		public void accept(final SessionEventDialogueManager sessionEventDiagMgr) {

			final String gameId = sessionEventDiagMgr.getGameId();
			LOGGER.debug("Processing game \"{}\".", gameId);

			final List<EventDialogue> uttDialogues = sessionEventDiagMgr.createUttDialogues();
			uttDialogues.forEach(uttDialogue -> {
				uttDialogue.getLastEvent().ifPresent(event -> {
					LOGGER.debug("Extracting features for utterances for event: {}", event);
					final String submittingPlayerId = event
							.getString(GameManagementEvent.Attribute.PLAYER_ID.toString());
					final Stream<Utterance> dialogueUttsFromInstructor = uttDialogue.getUtts().stream()
							.filter(dialogueUtt -> {
								final String uttPlayerId = dialogueUtt.getSpeakerId();
								return submittingPlayerId.equals(uttPlayerId);
							});
					dialogueUttsFromInstructor.forEach(dialogueUtt -> {
						final GameHistory history = sessionEventDiagMgr.getGameHistory();
						final GameContext uttCtx = createGameContext(dialogueUtt, history, submittingPlayerId);
						final int selectedEntityId = uttCtx.findLastSelectedEntityId().get();
						LOGGER.debug(
								"Creating positive and negative examples for entity ID \"{}\", which is selected by player \"{}\".",
								selectedEntityId, submittingPlayerId);
						// Add positive training examples
						final EntityFeature.Extractor.Context positiveContext = extCtxFactory.apply(uttCtx,
								selectedEntityId);
						addTokenInstances(dialogueUtt, positiveContext, Boolean.TRUE.toString());
						// Add negative training examples
						final EntityFeature.Extractor.Context negativeContext = extCtxFactory.apply(uttCtx,
								negativeExampleEntityIdGetter.applyAsInt(uttCtx));
						addTokenInstances(dialogueUtt, negativeContext, Boolean.FALSE.toString());
					});
				});
			});
		}

		private void addTokenInstances(final Utterance utt, final EntityFeature.Extractor.Context extractionContext,
				final String classValue) {
			utt.getTokens().forEach(token -> {
				final Instances classInstances = classInstanceGetter.apply(token);
				final Instance inst = new DenseInstance(entityInstAttrCtx.getAttrs().size());
				inst.setDataset(classInstances);
				entityInstAttrCtx.getExtractor().accept(inst, extractionContext);
				inst.setClassValue(classValue);
				classInstances.add(inst);
			});
		}
	}

	private static final Pattern CLASS_RELATION_NAME_PATTERN;

	private static final String CLASS_RELATION_PREFIX;

	private static final Logger LOGGER = LoggerFactory.getLogger(WordsAsClassifiersInstancesMapFactory.class);

	static {
		CLASS_RELATION_PREFIX = "referent_for_token-";
		CLASS_RELATION_NAME_PATTERN = Pattern.compile(Pattern.quote(CLASS_RELATION_PREFIX) + "(.+)");
	}

	public static String createRelationName(final String className) {
		return CLASS_RELATION_PREFIX + className;
	}

	public static String parseRelationClassName(final String relName) {
		final Matcher classRelNameMatcher = CLASS_RELATION_NAME_PATTERN.matcher(relName);
		if (classRelNameMatcher.matches()) {
			return classRelNameMatcher.group(1);
		} else {
			throw new IllegalArgumentException(
					String.format("Could not parse a class name from relation name \"%s\".", relName));
		}
	}

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

	private static int estimateVocabTokenCount(final String token, final Collection<?> sessionData) {
		return sessionData.size() * 25;
	}

	private static int estimateVocabTypeCount(final Collection<?> sessionData) {
		return Math.toIntExact(Math.round(Math.ceil(Math.log(sessionData.size()) * 850)));
	}

	@Inject
	private EntityInstanceAttributeContext entityInstAttrCtx;

	@Inject
	private EntityFeatureExtractionContextFactory extCtxFactory;

	@Inject
	private ToIntFunction<GameContext> negativeExampleEntityIdGetter;

	@Override
	public Map<String, Instances> apply(final Collection<SessionEventDialogueManager> sessionEventDiagMgrs) {
		final Map<String, Instances> result = Maps
				.newHashMapWithExpectedSize(estimateVocabTypeCount(sessionEventDiagMgrs));
		final Function<String, Instances> classInstanceFetcher = className -> result.computeIfAbsent(className, k -> {
			final Instances instances = new Instances(createRelationName(k), entityInstAttrCtx.getAttrs(),
					estimateVocabTokenCount(k, sessionEventDiagMgrs));
			instances.setClass(entityInstAttrCtx.getClassAttr());
			return instances;
		});
		final MultiClassDataCollector coll = new MultiClassDataCollector(classInstanceFetcher);
		for (final SessionEventDialogueManager sessionEventDiagMgr : sessionEventDiagMgrs) {
			coll.accept(sessionEventDiagMgr);
		}
		return result;
	}

}
