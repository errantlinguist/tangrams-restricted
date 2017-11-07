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
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Sets;

import it.unimi.dsi.fastutil.objects.Object2IntMap;
import it.unimi.dsi.fastutil.objects.Object2IntOpenHashMap;
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

	private static Set<String> createDiscountedWordClassSet(final DiscountedWordClasses discountedWordClasses) {
		final List<Entry<String, Instances>> discountedWordClassInsts = discountedWordClasses.getDiscountedClassInsts();
		return discountedWordClassInsts.stream().map(Entry::getKey)
				.collect(Collectors.toCollection(() -> Sets.newHashSetWithExpectedSize(discountedWordClassInsts.size())));
	}

	private final Executor backgroundJobExecutor;

	private final AbstractInstanceExtractor instExtractor;

	/**
	 * The obsevation counts for the word classes which were discounted for
	 * smoothing in the current word classifiers for the next dialogue to
	 * classify.
	 */
	private final Object2IntMap<String> lastDiscountedWordClassObservationCounts;

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

		final Entry<CompletableFuture<ConcurrentMap<String, Logistic>>, Object2IntMap<String>> wordClassifierTrainingResults = trainWordClassifiers(
				totalTrainingData);
		lastFutureWordClassifiers = wordClassifierTrainingResults.getKey();
		lastDiscountedWordClassObservationCounts = wordClassifierTrainingResults.getValue();

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

	private Object2IntMap<String> createSmoothedWordClassObservationCountMap(
			final DiscountedWordClasses discountedWordClasses) {
		final Object2IntMap<String> wordClassObservationCounts = totalTrainingData.getClassObservationCounts();
		final List<Entry<String, Instances>> smoothedWordClassInsts = discountedWordClasses.getDiscountedClassInsts();
		final Object2IntMap<String> result = new Object2IntOpenHashMap<>(smoothedWordClassInsts.size());
		for (final Entry<String, Instances> entry : smoothedWordClassInsts) {
			final String wordClass = entry.getKey();
			final int observationCount = wordClassObservationCounts.getInt(wordClass);
			result.put(wordClass, observationCount);
		}
		return result;
	}

	private CompletableFuture<ConcurrentMap<String, Logistic>> createWordClassifierMap(
			final Set<Entry<String, Instances>> classInstances) {
		final ConcurrentMap<String, Logistic> wordClassifiers = new ConcurrentHashMap<>(classInstances.size());
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

	private List<Entry<String, CompletableFuture<Logistic>>> trainWordClassifiers(
			final Set<Entry<String, Instances>> classInstances) {
		final List<Entry<String, CompletableFuture<Logistic>>> result = new ArrayList<>(classInstances.size());
		for (final Entry<String, Instances> classInstancesEntry : classInstances) {
			final String className = classInstancesEntry.getKey();
			LOGGER.debug("Training classifier for class \"{}\".", className);
			final Instances trainingInsts = classInstancesEntry.getValue();
			LOGGER.debug("{} instance(s) for class \"{}\".", trainingInsts.size(), className);
			final CompletableFuture<Logistic> trainingJob = CompletableFuture.supplyAsync(() -> {
				final Logistic classifier = new Logistic();
				try {
					classifier.buildClassifier(trainingInsts);
				} catch (final Exception e) {
					throw new WordClassifierTrainingException(className, e);
				}
				return classifier;
			}, backgroundJobExecutor);
			result.add(Pair.of(className, trainingJob));
		}
		return result;
	}

	private Entry<CompletableFuture<ConcurrentMap<String, Logistic>>, Object2IntMap<String>> trainWordClassifiers(
			final WordClassificationData unsmoothedTrainingData) {
		final WordClassificationData smoothedTrainingData = new WordClassificationData(unsmoothedTrainingData);
		final DiscountedWordClasses discountedWordClasses = smoother.redistributeMass(smoothedTrainingData);
		final Object2IntMap<String> discountedWordClassObservationCounts = createSmoothedWordClassObservationCountMap(
				discountedWordClasses);
		LOGGER.debug("{} instance(s) for out-of-vocabulary class.", discountedWordClasses.getOovInstances().size());
		return Pair.of(createWordClassifierMap(smoothedTrainingData.getClassInstances().entrySet()),
				discountedWordClassObservationCounts);
	}

	private Stream<CompletableFuture<Void>> updateWordClassifierMap(
			final ConcurrentMap<String, Logistic> currentWordClassifiers, final EventDialogue diagToClassify,
			final GameContext ctx) {
		// Object2IntMap<String> oldObservationCounts = new
		// Object2IntOpenHashMap<>(totalTrainingData.getClassObservationCounts());

		final Object2IntMap<String> dialogueWordObservationCounts = instExtractor.addTrainingData(diagToClassify,
				ctx.getHistory(), totalTrainingData, positiveExampleWeightFactor, negativeExampleWeightFactor);
		final WordClassificationData smoothedTrainingData = new WordClassificationData(totalTrainingData);
		final DiscountedWordClasses smoothingResults = smoother.redistributeMass(smoothedTrainingData);
		LOGGER.debug("{} instance(s) for out-of-vocabulary class.", smoothingResults.getOovInstances().size());
		final Set<String> discountedWordClasses = createDiscountedWordClassSet(smoothingResults);
		// The new counts can never be less than the old ones or have fewer word
		// classes because word classes are added as they are observed
		final Object2IntMap<String> newObservationCounts = smoothedTrainingData.getClassObservationCounts();

		final Map<String, Instances> smoothedClassInstances = smoothedTrainingData.getClassInstances();
		final Stream.Builder<CompletableFuture<Void>> trainingJobs = Stream.builder();
		for (final Object2IntMap.Entry<String> dialogueWordObservationCount : dialogueWordObservationCounts
				.object2IntEntrySet()) {
			final String wordClass = dialogueWordObservationCount.getKey();
			final boolean wasDiscountedDuringSmoothing = discountedWordClasses.contains(wordClass);
			if (!wasDiscountedDuringSmoothing) {
				LOGGER.debug("Training classifier for class \"{}\".", wordClass);
				final Instances trainingInsts = smoothedClassInstances.get(wordClass);
				LOGGER.debug("{} instance(s) for class \"{}\".", trainingInsts.size(), wordClass);
				final CompletableFuture<Void> trainingJob = CompletableFuture.runAsync(() -> {
					final Logistic classifier = new Logistic();
					try {
						classifier.buildClassifier(trainingInsts);
					} catch (final Exception e) {
						throw new WordClassifierTrainingException(wordClass, e);
					}
					final Logistic oldClassifier = currentWordClassifiers.put(wordClass, classifier);
					assert oldClassifier != null;
					// if (oldClassifier != null) {
					// throw new IllegalArgumentException(
					// String.format("More than one file for word class
					// \"%s\".",
					// className));
					// }

				}, backgroundJobExecutor);
				trainingJobs.add(trainingJob);
			}
		}

		return trainingJobs.build();
	}

}
