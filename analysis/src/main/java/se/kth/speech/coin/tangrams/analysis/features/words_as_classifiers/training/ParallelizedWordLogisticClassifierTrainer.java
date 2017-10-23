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

import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.Executor;
import java.util.function.Function;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.WordClassDiscountingSmoother;
import weka.classifiers.functions.Logistic;
import weka.core.Instances;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 20 Oct 2017
 *
 */
public final class ParallelizedWordLogisticClassifierTrainer
		implements Function<WordClassificationData, ConcurrentMap<String, Logistic>> {

	private static final Logger LOGGER = LoggerFactory.getLogger(ParallelizedWordLogisticClassifierTrainer.class);

	private final Executor backgroundJobExecutor;

	private final WordClassDiscountingSmoother smoother;

	public ParallelizedWordLogisticClassifierTrainer(final Executor backgroundJobExecutor,
			final WordClassDiscountingSmoother smoother) {
		this.backgroundJobExecutor = backgroundJobExecutor;
		this.smoother = smoother;
	}

	@Override
	public ConcurrentMap<String, Logistic> apply(final WordClassificationData trainingData) {
		final WordClassificationData smoothedTrainingData = new WordClassificationData(trainingData);
		final Instances oovInstances = smoother.redistributeMass(smoothedTrainingData);
		LOGGER.info("{} instance(s) for out-of-vocabulary class.", oovInstances.size());
		return apply(trainingData.getClassInstances().entrySet());
	}

	private ConcurrentMap<String, Logistic> apply(final Set<Entry<String, Instances>> classInstances) {
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
					throw new TrainingException(className, e);
				}
				final Logistic oldClassifier = result.put(className, classifier);
				if (oldClassifier != null) {
					throw new IllegalArgumentException(
							String.format("More than one file for word class \"%s\".", className));
				}

			}, backgroundJobExecutor);
			trainingJobs.add(trainingJob);
		}
		CompletableFuture.allOf(trainingJobs.build().toArray(CompletableFuture[]::new)).join();
		return result;
	}

}
