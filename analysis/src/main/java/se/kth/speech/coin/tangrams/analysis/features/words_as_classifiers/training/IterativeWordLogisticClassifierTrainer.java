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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.coin.tangrams.analysis.GameContext;
import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.WordClassDiscountingSmoother;
import weka.classifiers.functions.Logistic;
import weka.core.Instances;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 20 Oct 2017
 *
 */
public final class IterativeWordLogisticClassifierTrainer
		implements EventDialogueContextWordClassifierTrainer<Logistic> {

	private static class AsynchronousTrainer
			implements Function<Set<Entry<String, Instances>>, CompletableFuture<ConcurrentMap<String, Logistic>>> {

		private final Executor backgroundJobExecutor;

		private AsynchronousTrainer(final Executor backgroundJobExecutor) {
			this.backgroundJobExecutor = backgroundJobExecutor;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.util.function.Function#apply(java.lang.Object,
		 * java.lang.Object)
		 */
		@Override
		public CompletableFuture<ConcurrentMap<String, Logistic>> apply(
				final Set<Entry<String, Instances>> classInstances) {
			final ConcurrentMap<String, Logistic> result = new ConcurrentHashMap<>(classInstances.size());
			final Stream.Builder<CompletableFuture<Void>> trainingJobs = Stream.builder();
			for (final Entry<String, Instances> classInstancesEntry : classInstances) {
				final String className = classInstancesEntry.getKey();
				LOGGER.debug("Training classifier for class \"{}\".", className);
				final Instances trainingInsts = classInstancesEntry.getValue();
				LOGGER.debug("{} instance(s) for class \"{}\".", trainingInsts.size(), className);
				final CompletableFuture<Void> trainingJob = CompletableFuture.runAsync(() -> {
					final Logistic classifier = new Logistic();
					try {
						classifier.buildClassifier(trainingInsts);
					} catch (final Exception e) {
						throw new WordClassifierTrainingException(className, e);
					}
					final Logistic oldClassifier = result.put(className, classifier);
					assert oldClassifier == null;
					// if (oldClassifier != null) {
					// throw new IllegalArgumentException(
					// String.format("More than one file for word class
					// \"%s\".", className));
					// }

				}, backgroundJobExecutor);
				trainingJobs.add(trainingJob);
			}
			return CompletableFuture.allOf(trainingJobs.build().toArray(CompletableFuture[]::new))
					.thenApply(voidArg -> result);
		}
	}

	private static final Logger LOGGER = LoggerFactory.getLogger(IterativeWordLogisticClassifierTrainer.class);

	/**
	 * The {@link CompletableFuture future} of the {@link Map} of word
	 * classifiers to use for the next dialogue being classified.
	 */
	private CompletableFuture<ConcurrentMap<String, Logistic>> futureWordClassifiers;

	private final AbstractInstanceExtractor instExtractor;

	private final double negativeExampleWeightFactor;

	private final double positiveExampleWeightFactor;

	private final WordClassDiscountingSmoother smoother;

	private final WordClassificationData totalTrainingData;

	private final AsynchronousTrainer trainer;

	public IterativeWordLogisticClassifierTrainer(final Executor backgroundJobExecutor,
			final WordClassDiscountingSmoother smoother, final WordClassificationData initialTrainingData,
			final AbstractInstanceExtractor instExtractor, final double positiveExampleWeightFactor,
			final double negativeExampleWeightFactor) {
		trainer = new AsynchronousTrainer(backgroundJobExecutor);
		this.smoother = smoother;
		totalTrainingData = initialTrainingData;
		futureWordClassifiers = trainWordClassifiers(totalTrainingData);

		this.instExtractor = instExtractor;
		this.positiveExampleWeightFactor = positiveExampleWeightFactor;
		this.negativeExampleWeightFactor = negativeExampleWeightFactor;

	}

	@Override
	public Function<String, Logistic> apply(final EventDialogue diagToClassify, final GameContext ctx) {
		Function<String, Logistic> result = null;
		synchronized (futureWordClassifiers) {
			try {
				final ConcurrentMap<String, Logistic> wordClassifiers = futureWordClassifiers.get();
				// Return the completed map of all classifiers to the caller of
				// this
				// method so that the given dialogue can be classified
				result = wordClassifiers::get;
			} catch (InterruptedException | ExecutionException e) {
				throw new TrainingException(e);
			}
			instExtractor.addTrainingData(diagToClassify, ctx.getHistory(), totalTrainingData,
					positiveExampleWeightFactor, negativeExampleWeightFactor);
			futureWordClassifiers = trainWordClassifiers(totalTrainingData);
		}
		return result;
	}

	private CompletableFuture<ConcurrentMap<String, Logistic>> trainWordClassifiers(
			final WordClassificationData trainingData) {
		final WordClassificationData smoothedTrainingData = new WordClassificationData(trainingData);
		final Instances oovInstances = smoother.redistributeMass(smoothedTrainingData);
		LOGGER.debug("{} instance(s) for out-of-vocabulary class.", oovInstances.size());
		return trainer.apply(smoothedTrainingData.getClassInstances().entrySet());
	}

}
