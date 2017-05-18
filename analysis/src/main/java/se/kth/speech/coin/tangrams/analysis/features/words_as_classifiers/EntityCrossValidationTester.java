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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.concurrent.ExecutionException;
import java.util.function.Function;
import java.util.function.Supplier;

import javax.inject.Inject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.cache.LoadingCache;

import iristk.system.Event;
import it.unimi.dsi.fastutil.doubles.Double2ObjectSortedMap;
import it.unimi.dsi.fastutil.ints.Int2DoubleMap;
import it.unimi.dsi.fastutil.ints.Int2DoubleMaps;
import it.unimi.dsi.fastutil.ints.Int2DoubleOpenHashMap;
import it.unimi.dsi.fastutil.ints.IntList;
import it.unimi.dsi.fastutil.ints.IntOpenHashSet;
import it.unimi.dsi.fastutil.ints.IntSet;
import se.kth.speech.MutablePair;
import se.kth.speech.coin.tangrams.analysis.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.GameContext;
import se.kth.speech.coin.tangrams.analysis.GameHistory;
import se.kth.speech.coin.tangrams.analysis.SessionDataManager;
import se.kth.speech.coin.tangrams.analysis.SessionEventDialogueManager;
import se.kth.speech.coin.tangrams.analysis.Utterance;
import se.kth.speech.coin.tangrams.analysis.features.ClassificationException;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeature;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeatureExtractionContextFactory;
import se.kth.speech.coin.tangrams.analysis.features.weka.EntityInstanceAttributeContext;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;
import se.kth.speech.math.NBestRankings;
import weka.classifiers.Classifier;
import weka.core.DenseInstance;
import weka.core.Instance;
import weka.core.Instances;

public final class EntityCrossValidationTester {

	public static final class EventDialogueTestResults {

		private final List<Entry<Utterance, UtteranceTestResults>> uttTestResults;

		private EventDialogueTestResults(final List<Entry<Utterance, UtteranceTestResults>> uttTestResults) {
			this.uttTestResults = uttTestResults;
		}

		public void add(final EventDialogueTestResults other) {
			uttTestResults.addAll(other.uttTestResults);
		}

		public double meanReciprocalRank() {
			final double rrSum = sumReciprocalRank();
			return rrSum / totalUtterancesTested();
		}

		public double meanTokensPerUtterance() {
			final int totalTokens = totalTokens();
			return totalTokens / (double) uttTestResults.size();
		}

		public int totalTokens() {
			return uttTestResults.stream().map(Entry::getKey).map(Utterance::getTokens).mapToInt(List::size).sum();
		}

		public int totalUtterancesTested() {
			return uttTestResults.size();
		}

		private double sumReciprocalRank() {
			double result = 0.0;
			// TODO: Find a better way to calculate MRR, which avoids cumulative
			// floating-point precision errors
			for (final Entry<Utterance, UtteranceTestResults> uttTestResultsEntry : uttTestResults) {
				final UtteranceTestResults testResults = uttTestResultsEntry.getValue();
				final double uttRR = testResults.reciprocalRank();
				result += uttRR;
			}
			return result;
		}

	}

	public static final class SessionTestResults {

		private final List<Entry<EventDialogue, EventDialogueTestResults>> diagTestResults;

		public SessionTestResults(final List<Entry<EventDialogue, EventDialogueTestResults>> diagResults) {
			diagTestResults = diagResults;
		}

		public void add(final SessionTestResults other) {
			diagTestResults.addAll(other.diagTestResults);
		}

		/**
		 * @return the diagResults
		 */
		public List<Entry<EventDialogue, EventDialogueTestResults>> getDiagResults() {
			return diagTestResults;
		}

		public double meanReciprocalRank() {
			final double rrSum = sumReciprocalRank();
			return rrSum / totalUtterancesTested();
		}

		public double meanUttsPerDialogue() {
			final int totalUtts = totalUtterancesTested();
			return totalUtts / (double) totalDiagsTested();
		}

		public int totalDiagsTested() {
			return diagTestResults.size();
		}

		public int totalUtterancesTested() {
			return diagTestResults.stream().map(Entry::getValue)
					.mapToInt(EventDialogueTestResults::totalUtterancesTested).sum();
		}

		private double sumReciprocalRank() {
			double result = 0.0;
			// TODO: Find a better way to calculate MRR, which avoids cumulative
			// floating-point precision errors
			for (final Entry<EventDialogue, EventDialogueTestResults> diagTestResultsEntry : diagTestResults) {
				final EventDialogueTestResults diagTestResults = diagTestResultsEntry.getValue();
				final double diagRRSum = diagTestResults.sumReciprocalRank();
				result += diagRRSum;
			}
			return result;
		}

	}

	public static final class UtteranceTestResults {

		private final Int2DoubleMap entityReferenceConfidenceVals;

		private final int goldStandardEntityId;

		private final double rank;

		private UtteranceTestResults(final Int2DoubleMap entityReferenceConfidenceVals, final int goldStandardEntityId,
				final double rank) {
			this.entityReferenceConfidenceVals = entityReferenceConfidenceVals;
			this.goldStandardEntityId = goldStandardEntityId;
			this.rank = rank;
		}

		/**
		 * @return the entityReferenceConfidenceVals
		 */
		public Int2DoubleMap getEntityReferenceConfidenceVals() {
			return Int2DoubleMaps.unmodifiable(entityReferenceConfidenceVals);
		}

		/**
		 * @return the goldStandardEntityId
		 */
		public int getGoldStandardEntityId() {
			return goldStandardEntityId;
		}

		/**
		 * @return the rank
		 */
		public double getRank() {
			return rank;
		}

		public double reciprocalRank() {
			return 1.0 / rank;
		}
	}

	private static final Logger LOGGER = LoggerFactory.getLogger(EntityCrossValidationTester.class);

	private static double findNominalClassValueProbability(final Instance inst, final double[] classValProbs,
			final String classValue) {
		final List<Object> classVals = Collections.list(inst.classAttribute().enumerateValues());
		final int idx = classVals.indexOf(classValue);
		return classValProbs[idx];
	}

	@Inject
	private EntityInstanceAttributeContext entInstAttrCtx;

	@Inject
	private EntityFeatureExtractionContextFactory extCtxFactory;

	@Inject
	private Supplier<LoadingCache<SessionDataManager, SessionEventDialogueManager>> sessionDiagMgrCacheSupplier;

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

	public SessionTestResults testSession(final SessionDataManager sessionData)
			throws ExecutionException, ClassificationException {
		final SessionEventDialogueManager sessionEventDiagMgr = sessionDiagMgrCacheSupplier.get().get(sessionData);
		final List<EventDialogue> uttDiags = sessionEventDiagMgr.createUttDialogues();
		final SessionTestResults result = new SessionTestResults(new ArrayList<>(uttDiags.size()));
		for (final EventDialogue uttDiag : uttDiags) {
			final Optional<EventDialogueTestResults> optTestResults = testDialogue(uttDiag,
					sessionEventDiagMgr.getGameHistory());
			if (optTestResults.isPresent()) {
				final EventDialogueTestResults results = optTestResults.get();
				result.diagTestResults.add(new MutablePair<>(uttDiag, results));
			} else {
				LOGGER.debug("No utterances tested for {}.", uttDiag);
			}
		}
		return result;
	}

	private Instance createEntityTestInstance(final EntityFeature.Extractor.Context extractionContext) {
		final Instance result = new DenseInstance(entInstAttrCtx.getAttrs().size());
		result.setDataset(testInsts);
		entInstAttrCtx.getExtractor().accept(result, extractionContext);
		result.setClassMissing();
		return result;
	}

	private Int2DoubleMap createReferredEntityConfidenceMap(final Utterance utt, final GameContext uttCtx)
			throws ClassificationException {
		LOGGER.debug("Getting entity reference confidence measures for {}.", utt);
		final List<String> wordClasses = createUttWordClassList(utt);
		// TODO: Cache mapping of word classes -> classifiers?
		final List<Classifier> classifiers = smoother.createNGramClassifierList(wordClasses, wordClassifiers);
		final IntList entityIds = uttCtx.getEntityIds();
		final Int2DoubleMap result = new Int2DoubleOpenHashMap(entityIds.size());
		for (final int entityId : entityIds) {
			// Create a game context for classifying the entity with the
			// given ID
			final EntityFeature.Extractor.Context extContext = extCtxFactory.apply(uttCtx, entityId);
			final Instance testInst = createEntityTestInstance(extContext);
			double confidenceSum = 0.0;
			for (final Classifier classifier : classifiers) {
				try {
					final double[] classValProbs = classifier.distributionForInstance(testInst);
					final double classValProb = findNominalClassValueProbability(testInst, classValProbs,
							Boolean.TRUE.toString());
					confidenceSum += classValProb;
				} catch (final Exception e) {
					throw new ClassificationException(e);
				}
			}
			final double normalizedConfidence = confidenceSum / classifiers.size();
			result.put(entityId, normalizedConfidence);
		}
		return result;
	}

	private List<String> createUttWordClassList(final Utterance utt) {
		final List<String> tokens = utt.getTokens();
		// Unigrams
		// TODO: Implement converting token list into n-gram list for
		// higher-order classification
		return tokens;
	}

	private Optional<EventDialogueTestResults> testDialogue(final EventDialogue uttDiag, final GameHistory history)
			throws ClassificationException {
		final Optional<EventDialogueTestResults> result;

		final Optional<Event> optLastEvent = uttDiag.getLastEvent();
		if (optLastEvent.isPresent()) {
			final Event event = optLastEvent.get();
			LOGGER.debug("Creating test data for event: {}", event);
			final String submittingPlayerId = event.getString(GameManagementEvent.Attribute.PLAYER_ID.toString());
			final Utterance[] dialogueUttsFromInstructor = uttDiag.getUtts().stream().filter(dialogueUtt -> {
				final String uttPlayerId = dialogueUtt.getSpeakerId();
				return submittingPlayerId.equals(uttPlayerId);
			}).toArray(Utterance[]::new);
			if (dialogueUttsFromInstructor.length < 1) {
				result = Optional.empty();
			} else {
				final EventDialogueTestResults diagTestResults = new EventDialogueTestResults(
						new ArrayList<>(dialogueUttsFromInstructor.length));
				for (final Utterance dialogueUtt : dialogueUttsFromInstructor) {
					final GameContext uttCtx = UtteranceGameContexts.createGameContext(dialogueUtt, history,
							submittingPlayerId);
					final UtteranceTestResults uttTestResults = testUtterance(dialogueUtt, uttCtx);
					diagTestResults.uttTestResults.add(new MutablePair<>(dialogueUtt, uttTestResults));
				}
				result = Optional.of(diagTestResults);
			}
		} else {
			result = Optional.empty();
		}
		return result;
	}

	private UtteranceTestResults testUtterance(final Utterance dialogueUtt, final GameContext uttCtx)
			throws ClassificationException {
		final Int2DoubleMap entityReferenceConfidenceVals = createReferredEntityConfidenceMap(dialogueUtt, uttCtx);
		final Double2ObjectSortedMap<IntSet> nbestGroups = NBestRankings.createNbestGroupMap(
				entityReferenceConfidenceVals.int2DoubleEntrySet(), confidenceVal -> new IntOpenHashSet(1));
		final int goldStandardEntityId = uttCtx.findLastSelectedEntityId().get();
		final double rank = NBestRankings.findAveragedRank(nbestGroups.values(), goldStandardEntityId);
		assert rank <= uttCtx.getEntityCount();
		LOGGER.debug("Rank of correct entity: {}", rank);
		return new UtteranceTestResults(entityReferenceConfidenceVals, goldStandardEntityId, rank);
	}

}