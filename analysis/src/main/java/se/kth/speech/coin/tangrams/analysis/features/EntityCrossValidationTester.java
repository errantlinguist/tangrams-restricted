/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.analysis.
 *
 *  se.kth.speech.coin.tangrams-restricted.analysis is free software: you can redistribute it and/or modify
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

import java.io.IOException;
import java.util.List;
import java.util.ListIterator;
import java.util.concurrent.ExecutionException;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.stream.Stream;

import javax.inject.Inject;
import javax.xml.bind.JAXBException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;

import se.kth.speech.coin.tangrams.analysis.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.GameContext;
import se.kth.speech.coin.tangrams.analysis.GameHistory;
import se.kth.speech.coin.tangrams.analysis.SessionDataManager;
import se.kth.speech.coin.tangrams.analysis.SessionEventDialogueManager;
import se.kth.speech.coin.tangrams.analysis.Utterance;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;
import weka.classifiers.Classifier;
import weka.core.DenseInstance;
import weka.core.Instance;
import weka.core.Instances;

public final class EntityCrossValidationTester {

	private static final Logger LOGGER = LoggerFactory.getLogger(EntityCrossValidationTester.class);

	@Inject
	private EntityInstanceAttributeContext entInstAttrCtx;

	@Inject
	private BiFunction<ListIterator<Utterance>, GameHistory, Stream<EventDialogue>> eventDiagFactory;

	@Inject
	private BiFunction<? super GameContext, ? super Integer, EntityFeature.Extractor.Context> extCtxFactory;

	private final LoadingCache<SessionDataManager, SessionEventDialogueManager> sessionDiagMgrs = CacheBuilder
			.newBuilder().build(new CacheLoader<SessionDataManager, SessionEventDialogueManager>() {

				@Override
				public SessionEventDialogueManager load(final SessionDataManager key)
						throws JAXBException, IOException {
					return new SessionEventDialogueManager(key, eventDiagFactory);
				}

			});

	@Inject
	private WordClassDiscountingSmoother smoother;

	private Instances testInsts;

	private Function<? super String, ? extends Classifier> wordClassifiers;

	/**
	 * @param testInsts
	 *            the testInsts to set
	 */
	public void setTestInsts(final Instances testInsts) {
		this.testInsts = testInsts;
	}

	/**
	 * @param wordClassifiers
	 *            the wordClassifiers to set
	 */
	public void setWordClassifiers(final Function<? super String, ? extends Classifier> wordClassifiers) {
		this.wordClassifiers = wordClassifiers;
	}

	public void test(final SessionDataManager testSessionData) throws ExecutionException {
		final SessionEventDialogueManager sessionEventDiagMgr = sessionDiagMgrs.get(testSessionData);
		final List<EventDialogue> uttDiags = sessionEventDiagMgr.createUttDialogues();
		for (final EventDialogue uttDiag : uttDiags) {
			testEntities(uttDiag, sessionEventDiagMgr.getGameHistory());
		}
	}

	private Instance createEntityTestInstance(final EntityFeature.Extractor.Context extractionContext) {
		final Instance result = new DenseInstance(entInstAttrCtx.getAttrs().size());
		result.setDataset(testInsts);
		entInstAttrCtx.getExtractor().accept(result, extractionContext);
		result.setClassMissing();
		return result;
	}

	private List<String> createUttWordClassList(final Utterance utt) {
		final List<String> tokens = utt.getTokens();
		// Unigrams
		return tokens;
	}

	private void findReferredEntity(final Utterance utt, final GameContext uttCtx) throws ClassificationException {
		LOGGER.info("Finding entity referred by {}.", utt);
		final List<String> wordClasses = createUttWordClassList(utt);
		final List<Classifier> classifiers = smoother.createNGramClassifierList(wordClasses, wordClassifiers);
		for (final Integer entityId : uttCtx.getEntityIds()) {
			// Create a game context for classifying the entity with the
			// given ID
			final EntityFeature.Extractor.Context extContext = extCtxFactory.apply(uttCtx, entityId);
			final Instance testInst = createEntityTestInstance(extContext);
			for (final Classifier classifier : classifiers) {
				try {
					classifier.classifyInstance(testInst);
				} catch (final Exception e) {
					throw new ClassificationException(e);
				}
			}
		}
	}

	private void testEntities(final EventDialogue uttDiag, final GameHistory history) {
		uttDiag.getLastEvent().ifPresent(event -> {
			LOGGER.debug("Creating test data for event: {}", event);
			final String submittingPlayerId = event.getString(GameManagementEvent.Attribute.PLAYER_ID.toString());
			final List<Utterance> dialogueUtts = uttDiag.getUtts();
			dialogueUtts.forEach(dialogueUtt -> {
				final String uttPlayerId = dialogueUtt.getSpeakerId();
				if (submittingPlayerId.equals(uttPlayerId)) {
					final GameContext uttCtx = UtteranceGameContexts.createGameContext(dialogueUtt, history,
							submittingPlayerId);
					try {
						findReferredEntity(dialogueUtt, uttCtx);
					} catch (final ClassificationException e) {
						throw new RuntimeException(e);
					}
				} else {
					LOGGER.debug(
							"Skipping the extraction of features for utterance with segment ID \"{}\" because the utterance is from player \"{}\" rather than the player whose perspective is being used for extracting features (\"{}\")",
							dialogueUtt.getSegmentId(), uttPlayerId, submittingPlayerId);
				}
			});
		});
	}
}