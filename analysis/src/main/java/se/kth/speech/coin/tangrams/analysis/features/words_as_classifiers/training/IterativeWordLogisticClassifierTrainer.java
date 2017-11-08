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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executor;
import java.util.function.Function;
import java.util.stream.Stream;

import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import it.unimi.dsi.fastutil.objects.Object2IntMap;
import se.kth.speech.coin.tangrams.analysis.GameContext;
import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.WordClassDiscountingSmoother;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.WordClassDiscountingSmoother.DiscountedWordClasses;
import weka.classifiers.functions.Logistic;
import weka.core.Instances;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 20 Oct 2017
 *
 */
public final class IterativeWordLogisticClassifierTrainer
		implements EventDialogueContextWordClassifierTrainer<Logistic> {

	private static final Logger LOGGER = LoggerFactory.getLogger(IterativeWordLogisticClassifierTrainer.class);

	private static Logistic createTrainedClassifier(final String wordClass, final Instances trainingInsts)
			throws WordClassifierTrainingException {
		final Logistic result = new Logistic();
		try {
			result.buildClassifier(trainingInsts);
		} catch (final Exception e) {
			throw new WordClassifierTrainingException(wordClass, e);
		}
		return result;
	}

	private final Executor backgroundJobExecutor;

	private final AbstractInstanceExtractor instExtractor;

	/**
	 * The observation data for the word classes which were discounted for
	 * smoothing in the current word classifiers for the next dialogue to
	 * classify.
	 */
	private final Map<String, WordClassificationData.Datum> lastDiscountedWordClassData;

	/**
	 * The {@link CompletableFuture future} of the {@link Map} of word
	 * classifiers to use for the next dialogue being classified.
	 */
	private final CompletableFuture<ConcurrentMap<String, Logistic>> lastFutureWordClassifiers;

	private final double negativeExampleWeightFactor;

	private final double positiveExampleWeightFactor;

	private final WordClassDiscountingSmoother smoother;

	private final WordClassificationData totalTrainingData;

	public IterativeWordLogisticClassifierTrainer(final Executor backgroundJobExecutor,
			final WordClassDiscountingSmoother smoother, final WordClassificationData initialTrainingData,
			final AbstractInstanceExtractor instExtractor, final double positiveExampleWeightFactor,
			final double negativeExampleWeightFactor) {
		this.backgroundJobExecutor = backgroundJobExecutor;
		this.smoother = smoother;
		totalTrainingData = initialTrainingData;

		final Entry<CompletableFuture<ConcurrentMap<String, Logistic>>, Map<String, WordClassificationData.Datum>> wordClassifierTrainingResults = trainWordClassifiers(
				totalTrainingData);
		lastFutureWordClassifiers = wordClassifierTrainingResults.getKey();
		lastDiscountedWordClassData = wordClassifierTrainingResults.getValue();

		this.instExtractor = instExtractor;
		this.positiveExampleWeightFactor = positiveExampleWeightFactor;
		this.negativeExampleWeightFactor = negativeExampleWeightFactor;
	}

	@Override
	public Function<String, Logistic> apply(final EventDialogue diagToClassify, final GameContext ctx) {
		ConcurrentMap<String, Logistic> wordClassifiers = null;
		synchronized (lastFutureWordClassifiers) {
			try {
				wordClassifiers = lastFutureWordClassifiers.get();
			} catch (InterruptedException | ExecutionException e) {
				throw new TrainingException(e);
			}
		}
		// Return the completed map of all classifiers to the caller of
		// this
		// method so that the given dialogue can be classified
		final Function<String, Logistic> result = wordClassifiers::get;
		updateWordClassifierMap(wordClassifiers, diagToClassify, ctx);
		return result;
	}

	private CompletableFuture<ConcurrentMap<String, Logistic>> createWordClassifierMap(
			final Set<Entry<String, WordClassificationData.Datum>> classInstances) {
		final ConcurrentMap<String, Logistic> wordClassifiers = new ConcurrentHashMap<>(classInstances.size());
		final Stream.Builder<CompletableFuture<Void>> trainingJobs = Stream.builder();
		for (final Entry<String, WordClassificationData.Datum> classInstancesEntry : classInstances) {
			final String className = classInstancesEntry.getKey();
			LOGGER.debug("Training classifier for class \"{}\".", className);
			final WordClassificationData.Datum datum = classInstancesEntry.getValue();
			final Instances trainingInsts = datum.getTrainingInsts();
			LOGGER.debug("{} instance(s) for class \"{}\".", trainingInsts.size(), className);
			final CompletableFuture<Void> trainingJob = CompletableFuture.runAsync(() -> {
				final Logistic classifier = createTrainedClassifier(className, trainingInsts);
				final Logistic oldClassifier = wordClassifiers.put(className, classifier);
				assert oldClassifier == null;
				// if (oldClassifier != null) {
				// throw new IllegalArgumentException(
				// String.format("More than one file for word class \"%s\".",
				// className));
				// }

			}, backgroundJobExecutor);
			trainingJobs.add(trainingJob);
		}
		return CompletableFuture.allOf(trainingJobs.build().toArray(CompletableFuture[]::new))
				.thenApply(voidArg -> wordClassifiers);
	}

	private Entry<CompletableFuture<ConcurrentMap<String, Logistic>>, Map<String, WordClassificationData.Datum>> trainWordClassifiers(
			final WordClassificationData unsmoothedTrainingData) {
		final WordClassificationData smoothedTrainingData = new WordClassificationData(unsmoothedTrainingData);
		final DiscountedWordClasses discountedWordClasses = smoother.redistributeMass(smoothedTrainingData);
		final WordClassificationData.Datum oovClassDatum = discountedWordClasses.getOovClassDatum();
		LOGGER.debug("{} instance(s) for out-of-vocabulary class.", oovClassDatum.getTrainingInsts().size());
		return Pair.of(createWordClassifierMap(smoothedTrainingData.getClassData().entrySet()),
				discountedWordClasses.getDiscountedClassData());
	}

	private List<CompletableFuture<Void>> updateWordClassifierMap(
			final ConcurrentMap<String, Logistic> currentWordClassifiers, final EventDialogue diagToClassify,
			final GameContext ctx) {
		final Object2IntMap<String> dialogueWordObservationCounts = instExtractor.addTrainingData(diagToClassify,
				ctx.getHistory(), totalTrainingData, positiveExampleWeightFactor, negativeExampleWeightFactor);
		final WordClassificationData smoothedTrainingData = new WordClassificationData(totalTrainingData);
		// Smoother calculates which word class Instances objects should be
		// discounted, removes them from the classification data object and puts
		// it into the OOV label Instances object
		final DiscountedWordClasses smoothingResults = smoother.redistributeMass(smoothedTrainingData);
		LOGGER.debug("{} instance(s) for out-of-vocabulary class.",
				smoothingResults.getOovClassDatum().getTrainingInsts().size());
		final Map<String, WordClassificationData.Datum> discountedWordClassData = smoothingResults
				.getDiscountedClassData();
		final Map<String, WordClassificationData.Datum> smoothedClassData = smoothedTrainingData.getClassData();
		final List<CompletableFuture<Void>> result = new ArrayList<>(dialogueWordObservationCounts.size());
		// boolean oovClassChanged = false;
		// For each word class observed in the newly-added training data, check
		// if the corresponding classifier needs to be retrained
		for (final Object2IntMap.Entry<String> dialogueWordObservationCount : dialogueWordObservationCounts
				.object2IntEntrySet()) {
			final String wordClass = dialogueWordObservationCount.getKey();
			final WordClassificationData.Datum discountedWordClassDatum = discountedWordClassData.get(wordClass);
			// The word class data for the given word class as it was when
			// classifying the previous dialogue, given that it was discounted;
			// This will be null reference if not discounted in last
			// classification.
			final WordClassificationData.Datum lastDiscountedWordClassDatum = lastDiscountedWordClassData
					.get(wordClass);
			if (discountedWordClassDatum == null) {
				// More training instances have been added for the given,
				// non-discounted word class; The corresponding classifier needs
				// to be retrained
				LOGGER.debug("Training classifier for class \"{}\".", wordClass);
				final WordClassificationData.Datum datum = smoothedClassData.get(wordClass);
				final Instances trainingInsts = datum.getTrainingInsts();
				LOGGER.debug("{} instance(s) for class \"{}\".", trainingInsts.size(), wordClass);
				final CompletableFuture<Void> trainingJob = CompletableFuture.runAsync(() -> {
					final Logistic classifier = createTrainedClassifier(wordClass, trainingInsts);
					final Logistic oldClassifier = currentWordClassifiers.put(wordClass, classifier);
					assert oldClassifier != null;
					// if (oldClassifier != null) {
					// throw new IllegalArgumentException(
					// String.format("More than one file for word class
					// \"%s\".",
					// className));
					// }

				}, backgroundJobExecutor);
				result.add(trainingJob);

				if (lastDiscountedWordClassDatum != null) {
					// The class was used for discounting in the previous
					// classification; The classifier should be removed
					// oovClassChanged = true;
					currentWordClassifiers.remove(wordClass);
				}
			} else {
				// The given word class was discounted during smoothing. If it
				// was discounted while classifying this dialogue, it has to
				// have also been discounted in the previous dialogue since
				// training data can only be added, not removed.
				// if
				// (!discountedWordClassDatum.equals(lastDiscountedWordClassDatum)){
				// More training examples were added to the given word class
				// after the last classification even though it was still
				// discounted; The OOV class needs to be retrained
				// oovClassChanged = true;
				// }
			}

			// if (oovClassChanged){
			// final WordClassificationData.Datum oovClassDatum =
			// smoothedClassData.get(smoother.getOovClassName());
			// final Instances trainingInsts = oovClassDatum.getTrainingInsts();
			// LOGGER.info("Re-training OOV class, with {} instance(s)",
			// trainingInsts.size());
			// }
		}

		return result;
	}

}
