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
import java.util.Arrays;
import java.util.List;
import java.util.ListIterator;
import java.util.Map.Entry;
import java.util.function.Predicate;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import edu.stanford.nlp.ling.CoreAnnotations.SentencesAnnotation;
import edu.stanford.nlp.pipeline.Annotation;
import edu.stanford.nlp.pipeline.Annotator;
import edu.stanford.nlp.sentiment.RNNOptions;
import edu.stanford.nlp.sentiment.SentimentCoreAnnotations.SentimentClass;
import edu.stanford.nlp.util.CoreMap;
import it.unimi.dsi.fastutil.ints.IntList;
import it.unimi.dsi.fastutil.objects.Object2DoubleMap;
import it.unimi.dsi.fastutil.objects.Object2DoubleOpenHashMap;
import it.unimi.dsi.fastutil.objects.Object2IntMap;
import it.unimi.dsi.fastutil.objects.Object2IntOpenHashMap;
import se.kth.speech.Iterators;
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

	private static final Object2IntMap<String> SENTIMENT_LABEL_WEIGHTS = createSentimentClassWeightMap();

	/**
	 *
	 * @see edu.stanford.nlp.sentiment.RNNOptions#DEFAULT_CLASS_NAMES
	 */
	private static Object2IntMap<String> createSentimentClassWeightMap() {
		final String[] classNames = RNNOptions.DEFAULT_CLASS_NAMES;
		final Object2IntMap<String> result = new Object2IntOpenHashMap<>(classNames.length);
		result.defaultReturnValue(0);
		result.put("Very negative", -2);
		result.put("Negative", -1);
		result.put("Neutral", 0);
		result.put("Positive", 1);
		result.put("Very positive", 2);
		return result;
	}

	private final Annotator annotator;

	private final EventDialogueTransformer diagTransformer;

	private final EntityFeatureExtractionContextFactory extCtxFactory;

	private final Object2DoubleMap<Utterance> uttWeightCache;

	public SentimentAnalyzingInstancesFactory(final EntityInstanceAttributeContext entityInstAttrCtx,
			final EventDialogueTransformer diagTransformer, final EntityFeatureExtractionContextFactory extCtxFactory,
			final Annotator annotator) {
		super(entityInstAttrCtx);
		this.diagTransformer = diagTransformer;
		this.extCtxFactory = extCtxFactory;
		this.annotator = annotator;

		final int estimatedUniqueUttCount = 1000;
		uttWeightCache = new Object2DoubleOpenHashMap<>(estimatedUniqueUttCount);
	}

	private void addWeightedExamples(final String wordClass, final WordClassificationData trainingData,
			final Stream<Entry<EntityFeature.Extractor.Context, String>> trainingContexts, final double weight) {
		final Instances classInsts = trainingData.fetchWordInstances(wordClass);
		final Instance[] trainingInsts = trainingContexts
				.map(context -> createTokenInstance(classInsts, context.getKey(), context.getValue()))
				.toArray(Instance[]::new);
		for (final Instance trainingInst : trainingInsts) {
			trainingInst.setWeight(weight);
		}
		// Add examples
		trainingData.addObservation(wordClass, Arrays.stream(trainingInsts));
	}

	private double calculateUttSentimentRank(final Utterance utt) {
		final Annotation annot = new Annotation(utt.getTokenStr());
		annotator.annotate(annot);
		final List<CoreMap> sents = annot.get(SentencesAnnotation.class);
		final double result;
		if (sents.isEmpty()) {
			result = 0.0;
		} else {
			int rankSum = 0;
			// traversing the words in the current sentence
			for (final CoreMap sent : sents) {
				final String sentimentClass = sent.get(SentimentClass.class);
				rankSum += SENTIMENT_LABEL_WEIGHTS.getInt(sentimentClass);
			}
			result = rankSum / (double) sents.size();
		}
		return result;
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

	private double fetchUttSentimentRank(final Utterance utt) {
		final double result;
		if (uttWeightCache.containsKey(utt)) {
			result = uttWeightCache.getDouble(utt);
		} else {
			// Calculate the weight without any locking because it's okay if
			// weight is sometimes calculated more than once per utt inst-- this
			// function is stateless
			result = calculateUttSentimentRank(utt);
			uttWeightCache.put(utt, result);
		}
		return result;
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
					final List<Entry<EntityFeature.Extractor.Context, String>> trainingContexts = createTrainingContexts(
							allUtts.get(0), history);
					final Predicate<Utterance> instructorUttMatcher = UtteranceMatchers
							.createEventSubmitterUtteranceMatcher(event);

					final ListIterator<Utterance> uttIter = allUtts.listIterator();
					while (uttIter.hasNext()) {
						final Entry<Stream<Utterance>, Utterance> preInstructorUtts = Iterators
								.findElementsBeforeDelimiter(uttIter, instructorUttMatcher);
						final Utterance firstInstructorUtt = preInstructorUtts.getValue();

						final double firstInstructorUttSentimentRank = fetchUttSentimentRank(firstInstructorUtt);
						if (firstInstructorUttSentimentRank != 0) {
							// Use the other player's utterances which came
							// before this instructor utterance as weighted
							// examples
							final Stream<String> preInstructorWordClasses = preInstructorUtts.getKey()
									.map(Utterance::getTokens).flatMap(List::stream);
							preInstructorWordClasses.forEach(wordClass -> {
								LOGGER.debug("Adding word class \"{}\" from non-instructor utterance with weight {}.",
										wordClass, firstInstructorUttSentimentRank);
								addWeightedExamples(wordClass, trainingData, trainingContexts.stream(),
										firstInstructorUttSentimentRank);
							});
						}

						final double instructorObservationWeight = firstInstructorUttSentimentRank <= 0.0 ? 1.0
								: firstInstructorUttSentimentRank;
						firstInstructorUtt.getTokens().stream().forEach(wordClass -> {
							LOGGER.debug("Adding word class \"{}\" from instructor utterance with weight {}.",
									wordClass, instructorObservationWeight);
							addWeightedExamples(wordClass, trainingData, trainingContexts.stream(),
									instructorObservationWeight);
						});
					}
				}
			});
		});
	}
}
