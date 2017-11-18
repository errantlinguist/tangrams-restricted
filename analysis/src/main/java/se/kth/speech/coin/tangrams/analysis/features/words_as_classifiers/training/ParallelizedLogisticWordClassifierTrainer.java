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
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.Executor;
import java.util.function.Function;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import it.unimi.dsi.fastutil.objects.Object2ObjectMap;
import it.unimi.dsi.fastutil.objects.ObjectSet;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.WordClassDiscountingSmoother;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.WordClassDiscountingSmoother.DiscountedWordClasses;
import weka.classifiers.functions.Logistic;
import weka.core.Instances;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 20 Oct 2017
 *
 */
public final class ParallelizedLogisticWordClassifierTrainer
		implements Function<WordClassificationData, TrainingResults<Logistic>> {

	private static final Logger LOGGER = LoggerFactory.getLogger(ParallelizedLogisticWordClassifierTrainer.class);

	private static ConcurrentMap<String, Logistic> createInitialMap(
			final ObjectSet<Object2ObjectMap.Entry<String, WordClassificationData.Datum>> classData) {
		// TODO: optimize
		return new ConcurrentHashMap<>(classData.size() + 1, 1.0f);
	}

	private final Executor backgroundJobExecutor;

	private final WordClassDiscountingSmoother smoother;

	public ParallelizedLogisticWordClassifierTrainer(final Executor backgroundJobExecutor,
			final WordClassDiscountingSmoother smoother) {
		this.backgroundJobExecutor = backgroundJobExecutor;
		this.smoother = smoother;
	}

	@Override
	public TrainingResults<Logistic> apply(final WordClassificationData trainingData) {
		final WordClassificationData smoothedTrainingData = new WordClassificationData(trainingData);
		final DiscountedWordClasses discountedWordClasses = smoother
				.redistributeMass(smoothedTrainingData.getClassData());
		LOGGER.info("{} instance(s) for out-of-vocabulary class.",
				discountedWordClasses.getOovClassDatum().getTrainingInstCount());
		final CompletableFuture<ConcurrentMap<String, Logistic>> futureWordClassifiers = createWordClassifierMap(
				smoothedTrainingData.getClassData().object2ObjectEntrySet());
		return new TrainingResults<>(futureWordClassifiers, discountedWordClasses);
	}

	private CompletableFuture<ConcurrentMap<String, Logistic>> createWordClassifierMap(
			final ObjectSet<Object2ObjectMap.Entry<String, WordClassificationData.Datum>> classData) {
		final ConcurrentMap<String, Logistic> wordClassifiers = createInitialMap(classData);
		final Stream.Builder<CompletableFuture<Void>> trainingJobs = Stream.builder();
		for (final Object2ObjectMap.Entry<String, WordClassificationData.Datum> classDatum : classData) {
			final String className = classDatum.getKey();
			LOGGER.debug("Training classifier for class \"{}\".", className);
			final WordClassificationData.Datum datum = classDatum.getValue();
			final Instances trainingInsts = datum.getTrainingInsts();
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
			}, backgroundJobExecutor);
			trainingJobs.add(trainingJob);
		}
		return CompletableFuture.allOf(trainingJobs.build().toArray(CompletableFuture[]::new))
				.thenApply(voidArg -> wordClassifiers);
	}

}
