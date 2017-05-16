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
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NavigableMap;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.concurrent.ExecutionException;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.inject.Inject;
import javax.xml.bind.JAXBException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.google.common.collect.Maps;

import iristk.system.Event;
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

	public static final class EventDialogueTestResults {

		private int rankSum;

		private int totalUtterancesTested;

		public void add(final EventDialogueTestResults other) {
			rankSum += other.rankSum;
			totalUtterancesTested += other.totalUtterancesTested;
		}

		public double getMeanRank() {
			return rankSum / (double) totalUtterancesTested;
		}

		/**
		 * @return the rankSum
		 */
		public int getRankSum() {
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

	private static final Comparator<Entry<Integer, Double>> NBEST_COMPARATOR;

	static {
		final Comparator<Entry<Integer, Double>> scoreCmp = Comparator.comparing(Entry::getValue);
		NBEST_COMPARATOR = scoreCmp.reversed();
	}

	private static NavigableMap<Double, Set<Integer>> createNbestMap(
			final Collection<Entry<Integer, Double>> entityReferenceConfidenceVals) {
		final NavigableMap<Double, Set<Integer>> result = new TreeMap<>();
		for (final Entry<Integer, Double> val : entityReferenceConfidenceVals) {
			final Set<Integer> entityIds = result.computeIfAbsent(val.getValue(), k -> new HashSet<>());
			entityIds.add(val.getKey());
		}
		return result;
	}

	private static int findNbestRank(final NavigableMap<Double, Set<Integer>> nbestMap, final Integer entityId) {
		int result = 1;
		boolean found = false;
		for (final Entry<Double, Set<Integer>> nbest : nbestMap.entrySet()) {
			if (nbest.getValue().contains(entityId)) {
				found = true;
				break;
			}
			result++;
		}
		if (!found) {
			throw new IllegalArgumentException("ID not found.");
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

	public SessionTestResults testSession(final SessionDataManager sessionData) throws ExecutionException {
		final SessionEventDialogueManager sessionEventDiagMgr = sessionDiagMgrs.get(sessionData);
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

	private Map<Integer, Double> createReferredEntityConfidenceMap(final Utterance utt, final GameContext uttCtx)
			throws ClassificationException {
		LOGGER.debug("Getting entity reference confidence measures for {}.", utt);
		final List<String> wordClasses = createUttWordClassList(utt);
		final List<Classifier> classifiers = smoother.createNGramClassifierList(wordClasses, wordClassifiers);
		final List<Integer> entityIds = uttCtx.getEntityIds();
		final Map<Integer, Double> result = Maps.newHashMapWithExpectedSize(entityIds.size());
		for (final Integer entityId : entityIds) {
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
						final Map<Integer, Double> entityReferenceConfidenceVals = createReferredEntityConfidenceMap(
								dialogueUtt, uttCtx);
						// System.out.println(dialogueUtt);
						final List<Entry<Integer, Double>> nbestVals = entityReferenceConfidenceVals.entrySet().stream()
								.sorted(NBEST_COMPARATOR).collect(Collectors
										.toCollection(() -> new ArrayList<>(entityReferenceConfidenceVals.size())));
						// for (final Entry<Integer, Double> nbestVal :
						// nbestVals) {
						// System.out.println("score: " +
						// nbestVal.getValue() +
						// "\tentity: " + nbestVal.getKey());
						// }
						final NavigableMap<Double, Set<Integer>> nbestValMap = createNbestMap(nbestVals);
						final Optional<Integer> optEntityId = uttCtx.findLastSelectedEntityId();

						final int rank;
						if (optEntityId.isPresent()) {
							final Integer entityId = optEntityId.get();

							rank = findNbestRank(nbestValMap, entityId);
						} else {
							rank = nbestValMap.size() + 1;
						}
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