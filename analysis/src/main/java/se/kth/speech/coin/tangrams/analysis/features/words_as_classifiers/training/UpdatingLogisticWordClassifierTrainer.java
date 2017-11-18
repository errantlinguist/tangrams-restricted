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

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executor;
import java.util.concurrent.Future;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import it.unimi.dsi.fastutil.objects.Object2IntMap;
import it.unimi.dsi.fastutil.objects.Object2ObjectMap;
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
public final class UpdatingLogisticWordClassifierTrainer
		implements EventDialogueContextWordClassifierTrainer<Logistic> {

	private static class TrainedClassifierPutter implements Runnable {

		private final Instances trainingInsts;

		private final String wordClass;

		private final ConcurrentMap<String, ? super Logistic> wordClassifiers;

		private TrainedClassifierPutter(final String wordClass, final Instances trainingInsts,
				final ConcurrentMap<String, ? super Logistic> wordClassifiers) {
			this.wordClass = wordClass;
			this.trainingInsts = trainingInsts;
			this.wordClassifiers = wordClassifiers;
		}

		@Override
		public void run() {
			final Logistic classifier = createTrainedClassifier(wordClass, trainingInsts);
			wordClassifiers.put(wordClass, classifier);
		}

	}

	private static final Logger LOGGER = LoggerFactory.getLogger(UpdatingLogisticWordClassifierTrainer.class);

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
	 * The {@link TrainingResults} of the last training iteration run.
	 */
	private TrainingResults<Logistic> lastWordClassifierTrainingResults;

	private final double negativeExampleWeightFactor;

	private final double positiveExampleWeightFactor;

	private final WordClassDiscountingSmoother smoother;

	private final WordClassificationData totalTrainingData;

	public UpdatingLogisticWordClassifierTrainer(final Executor backgroundJobExecutor,
			final WordClassDiscountingSmoother smoother, final WordClassificationData initialTrainingData,
			final AbstractInstanceExtractor instExtractor, final double positiveExampleWeightFactor,
			final double negativeExampleWeightFactor) {
		this.backgroundJobExecutor = backgroundJobExecutor;
		this.smoother = smoother;
		totalTrainingData = initialTrainingData;

		lastWordClassifierTrainingResults = new ParallelizedLogisticWordClassifierTrainer(backgroundJobExecutor,
				smoother).apply(initialTrainingData);

		this.instExtractor = instExtractor;
		this.positiveExampleWeightFactor = positiveExampleWeightFactor;
		this.negativeExampleWeightFactor = negativeExampleWeightFactor;
	}

	@Override
	public ConcurrentMap<String, Logistic> apply(final EventDialogue diagToClassify, final GameContext ctx) {
		final ConcurrentMap<String, Logistic> result;

		synchronized (lastWordClassifierTrainingResults) {
			final Future<? extends ConcurrentMap<String, Logistic>> updatedClassifierMapFuture = lastWordClassifierTrainingResults
					.getFutureWordClassifiers();
			try {
				// Return the completed map of all classifiers to the caller of
				// this
				// method so that the given dialogue can be classified
				result = updatedClassifierMapFuture.get();
			} catch (InterruptedException | ExecutionException e) {
				throw new TrainingException(e);
			}

			lastWordClassifierTrainingResults = updateWordClassifierMap(result, diagToClassify, ctx);
		}
		return result;
	}

	private CompletableFuture<Void> retrainOovClass(
			final Object2ObjectMap<String, WordClassificationData.Datum> smoothedUpdatedClassData,
			final ConcurrentMap<String, Logistic> currentWordClassifiers) {
		final String oovClassName = smoother.getOovClassName();
		final WordClassificationData.Datum oovClassDatum = smoothedUpdatedClassData.get(oovClassName);
		final Instances trainingInsts = oovClassDatum.getTrainingInsts();
		LOGGER.debug("Re-training OOV class, with {} instance(s).", trainingInsts.size());
		assert !trainingInsts.isEmpty();
		final CompletableFuture<Void> result = CompletableFuture.runAsync(
				new TrainedClassifierPutter(oovClassName, trainingInsts, currentWordClassifiers),
				backgroundJobExecutor);
		return result;
	}

	private TrainingResults<Logistic> updateWordClassifierMap(
			final ConcurrentMap<String, Logistic> currentWordClassifiers, final EventDialogue diagToClassify,
			final GameContext ctx) {
		final Object2IntMap<String> updatedDialogueWordObservationCounts = instExtractor.addTrainingData(diagToClassify,
				ctx.getHistory(), totalTrainingData, positiveExampleWeightFactor, negativeExampleWeightFactor);
		// Create a new WordClassificationData instance in order to be able to re-apply
		// smoothing to the original data again
		final Object2ObjectMap<String, WordClassificationData.Datum> smoothedUpdatedClassData = new WordClassificationData(totalTrainingData)
				.getClassData();
		assert !smoothedUpdatedClassData.containsKey(null);
		
		// Smoother calculates which word class Instances objects should be
		// discounted, removes them from the classification data object and puts
		// it into the OOV label Instances object
		final DiscountedWordClasses smoothingResults = smoother.redistributeMass(smoothedUpdatedClassData);
		LOGGER.debug("{} instance(s) for out-of-vocabulary class.",
				smoothingResults.getOovClassDatum().getTrainingInstCount());
		final Object2ObjectMap<String, DiscountedWordClasses.Datum> updatedDiscountedWordClassData = smoothingResults
				.getDiscountedClassData();
		final Stream.Builder<CompletableFuture<Void>> wordClassTrainingJobs = Stream.builder();

		// Always re-train the OOV class because it's easier and likely not much
		// more than trying to determine if it needs to be, and it saves saving
		// e.g. Instances objects from the previous training iteration
		{
			final CompletableFuture<Void> oovClassTrainingJob = retrainOovClass(smoothedUpdatedClassData,
					currentWordClassifiers);
			wordClassTrainingJobs.add(oovClassTrainingJob);
		}

		// For each word class observed in the newly-added training data, check
		// if the corresponding classifier needs to be retrained
		for (final String updatedWordClass : updatedDialogueWordObservationCounts.keySet()) {
			LOGGER.info("Updating classifier for updated set of instances for word class \"{}\".", updatedWordClass);
			final DiscountedWordClasses.Datum updatedDiscountedWordClassDatum = updatedDiscountedWordClassData
					.get(updatedWordClass);
			// The word class data for the given word class as it was when
			// classifying the previous dialogue, given that it was discounted;
			// This will be null reference if not discounted in last
			// classification.
			final DiscountedWordClasses.Datum lastDiscountedWordClassDatum = lastWordClassifierTrainingResults
					.getDiscountedWordClasses().getDiscountedClassData().get(updatedWordClass);
			// If there is already a classifier trained for the given word
			// class, it cannot
			// have been discounted in the current training iteration
			// Conversely, if the word class data for the given word was found
			// in the
			// discounted word set for the last training iteration, there cannot
			// be a
			// classifier for it in the map
			assert currentWordClassifiers.containsKey(updatedWordClass) == (lastDiscountedWordClassDatum != null);

			if (updatedDiscountedWordClassDatum == null) {
				// Either more training instances have been added for the given,
				// non-discounted word class in the case that it has been
				// observed before, or
				// this is a newly-observed word class which was not discounted;
				// The
				// corresponding classifier needs
				// to be (re)trained
				LOGGER.debug("Training classifier for class \"{}\".", updatedWordClass);
				final WordClassificationData.Datum updatedWordClassDatum = smoothedUpdatedClassData
						.get(updatedWordClass);
				final Instances updatedTrainingInsts = updatedWordClassDatum.getTrainingInsts();
				LOGGER.debug("{} instance(s) for class \"{}\".", updatedTrainingInsts.size(), updatedWordClass);
				final CompletableFuture<Void> wordClassTrainingJob = CompletableFuture.runAsync(
						new TrainedClassifierPutter(updatedWordClass, updatedTrainingInsts, currentWordClassifiers),
						backgroundJobExecutor);
				wordClassTrainingJobs.add(wordClassTrainingJob);

			} else {
				// The given word class was discounted during smoothing. If it
				// was discounted while classifying this dialogue, it has to
				// have also been discounted in the previous dialogue since
				// training data can only be added, not removed --- unless the
				// word class had
				// not been observed until now. Either way, the map should not
				// have a
				// corresponding classifier
				assert !currentWordClassifiers.containsKey(updatedWordClass);
			}

		}

		final CompletableFuture<ConcurrentMap<String, Logistic>> updatedMapFuture = CompletableFuture
				.allOf(wordClassTrainingJobs.build().toArray(CompletableFuture[]::new))
				.thenApply(voidArg -> currentWordClassifiers);
		return new TrainingResults<>(updatedMapFuture, smoothingResults);
	}

}
