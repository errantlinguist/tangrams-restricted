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

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.ExecutionException;
import java.util.function.Function;
import java.util.function.Supplier;

import javax.inject.Inject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.cache.LoadingCache;

import iristk.system.Event;
import it.unimi.dsi.fastutil.doubles.Double2ObjectRBTreeMap;
import it.unimi.dsi.fastutil.doubles.Double2ObjectSortedMap;
import it.unimi.dsi.fastutil.doubles.DoubleComparators;
import it.unimi.dsi.fastutil.ints.Int2DoubleMap;
import it.unimi.dsi.fastutil.ints.Int2DoubleOpenHashMap;
import it.unimi.dsi.fastutil.ints.IntCollection;
import it.unimi.dsi.fastutil.ints.IntList;
import it.unimi.dsi.fastutil.ints.IntOpenHashSet;
import it.unimi.dsi.fastutil.ints.IntSet;
import it.unimi.dsi.fastutil.objects.ObjectIterable;
import se.kth.speech.coin.tangrams.analysis.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.GameContext;
import se.kth.speech.coin.tangrams.analysis.GameHistory;
import se.kth.speech.coin.tangrams.analysis.SessionDataManager;
import se.kth.speech.coin.tangrams.analysis.SessionEventDialogueManager;
import se.kth.speech.coin.tangrams.analysis.Utterance;
import se.kth.speech.coin.tangrams.analysis.features.weka.EntityInstanceAttributeContext;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;
import weka.classifiers.Classifier;
import weka.core.DenseInstance;
import weka.core.Instance;
import weka.core.Instances;

public final class EntityCrossValidationTester {

	public static final class EventDialogueTestResults {

		private double rankSum;

		private int totalUtterancesTested;

		public void add(final EventDialogueTestResults other) {
			rankSum += other.rankSum;
			totalUtterancesTested += other.totalUtterancesTested;
		}

		public double getMeanRank() {
			return rankSum / totalUtterancesTested;
		}

		/**
		 * @return the rankSum
		 */
		public double getRankSum() {
			return rankSum;
		}

		/**
		 * @return the totalUtterancesTested
		 */
		public int getTotalUtterancesTested() {
			return totalUtterancesTested;
		}
	}

	public static final class SessionTestResults {

		private final EventDialogueTestResults diagResults = new EventDialogueTestResults();

		private int totalDiagsTested;

		public void add(final SessionTestResults other) {
			diagResults.add(other.diagResults);
			totalDiagsTested += other.totalDiagsTested;
		}

		/**
		 * @return the diagResults
		 */
		public EventDialogueTestResults getDiagResults() {
			return diagResults;
		}

		public double getMeanUttsPerDialogue() {
			return diagResults.totalUtterancesTested / (double) totalDiagsTested;
		}

		public int getTotalDiagsTested() {
			return totalDiagsTested;
		}

	}

	private static final Logger LOGGER = LoggerFactory.getLogger(EntityCrossValidationTester.class);

	private static Double2ObjectSortedMap<IntSet> createNbestGroupMap(
			final Collection<Int2DoubleMap.Entry> entityReferenceConfidenceVals) {
		final Double2ObjectSortedMap<IntSet> result = new Double2ObjectRBTreeMap<>(
				DoubleComparators.OPPOSITE_COMPARATOR);
		for (final Int2DoubleMap.Entry entityReferenceConfidenceVal : entityReferenceConfidenceVals) {
			final double confidenceVal = entityReferenceConfidenceVal.getDoubleValue();
			IntSet entityIds = result.get(confidenceVal);
			if (entityIds == null) {
				entityIds = new IntOpenHashSet();
				result.put(confidenceVal, entityIds);
			}
			entityIds.add(entityReferenceConfidenceVal.getIntKey());
		}
		return result;
	}

	private static double findAveragedRank(final ObjectIterable<? extends IntCollection> nbestGroups, final int entityId) {
		double bestRankForTiedGroup = 1.0;
		IntCollection tiedEntityIds = null;
		for (final IntCollection nbestGroup : nbestGroups) {
			if (nbestGroup.contains(entityId)) {
				tiedEntityIds = nbestGroup;
				break;
			}
			bestRankForTiedGroup += nbestGroup.size();
		}

		final double result;
		if (tiedEntityIds == null) {
			throw new IllegalArgumentException("ID not found.");
		} else {
			final double worstRankForTiedGroup = bestRankForTiedGroup + (tiedEntityIds.size() - 1);
			// Average the ranks for sets of ties
			result = (bestRankForTiedGroup + worstRankForTiedGroup) / 2;
		}
		return result;
	}

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

	public SessionTestResults testSession(final SessionDataManager sessionData) throws ExecutionException {
		final SessionEventDialogueManager sessionEventDiagMgr = sessionDiagMgrCacheSupplier.get().get(sessionData);
		final List<EventDialogue> uttDiags = sessionEventDiagMgr.createUttDialogues();
		final SessionTestResults result = new SessionTestResults();
		for (final EventDialogue uttDiag : uttDiags) {
			final Optional<EventDialogueTestResults> optTestResults = testDialogue(uttDiag,
					sessionEventDiagMgr.getGameHistory());
			if (optTestResults.isPresent()) {
				result.totalDiagsTested++;

				final EventDialogueTestResults results = optTestResults.get();
				{
					final double mean = results.getMeanRank();
					LOGGER.debug("Mean rank of {} for {} utterance(s) tested in {}.", mean,
							results.totalUtterancesTested, uttDiag);
					assert !Double.isNaN(mean);
				}
				result.diagResults.add(results);
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
		// TODO: Implement converting token list into n-gram list for higher-order classification
		return tokens;
	}

	private Optional<EventDialogueTestResults> testDialogue(final EventDialogue uttDiag, final GameHistory history) {
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
				final EventDialogueTestResults testResults = new EventDialogueTestResults();
				for (final Utterance dialogueUtt : dialogueUttsFromInstructor) {
					testResults.totalUtterancesTested++;
					final GameContext uttCtx = UtteranceGameContexts.createGameContext(dialogueUtt, history,
							submittingPlayerId);
					try {
						final Int2DoubleMap entityReferenceConfidenceVals = createReferredEntityConfidenceMap(
								dialogueUtt, uttCtx);
						final Double2ObjectSortedMap<IntSet> nbestGroups = createNbestGroupMap(
								entityReferenceConfidenceVals.int2DoubleEntrySet());
						final double rank = findAveragedRank(nbestGroups.values(),
								uttCtx.findLastSelectedEntityId().get());
						assert rank <= uttCtx.getEntityCount();
						LOGGER.debug("Rank of correct entity: {}", rank);
						testResults.rankSum += rank;
					} catch (final ClassificationException e) {
						throw new RuntimeException(e);
					}
				}
				result = Optional.of(testResults);
			}
		} else {
			result = Optional.empty();
		}
		return result;
	}
}