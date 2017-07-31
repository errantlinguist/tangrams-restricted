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
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.Function;
import java.util.stream.Stream;

import javax.inject.Inject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import it.unimi.dsi.fastutil.objects.Object2DoubleMap;
import it.unimi.dsi.fastutil.objects.Object2IntMap;
import it.unimi.dsi.fastutil.objects.ObjectIterator;
import se.kth.speech.MutablePair;
import se.kth.speech.coin.tangrams.analysis.features.weka.WordClassInstancesFactory;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.WordClassificationData;
import weka.classifiers.Classifier;
import weka.core.Instances;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 16 May 2017
 * @see <a href="http://www.aclweb.org/anthology/P15-1029">Casey Kennington,
 *      &amp; David Schlangen. &ldquo;Simple Learning and Compositional
 *      Application of Perceptually Grounded Word Meanings for Incremental
 *      Reference Resolution&rdquo;. In <em>Proceedings of the 53<sup>rd</sup>
 *      Annual Meeting of the Association for Computational Linguistics and the
 *      7<sup>th</sup> International Joint Conference on Natural Language
 *      Processing</em><a>.
 *
 */
public final class WordClassDiscountingSmoother {

	private static final Logger LOGGER = LoggerFactory.getLogger(WordClassDiscountingSmoother.class);

	@Inject
	private WordClassInstancesFactory classInstsFactory;

	private final int minCount;

	private final String oovClassName;

	public WordClassDiscountingSmoother(final int minCount) { // NO_UCD (unused code)
		this(minCount, null);
	}

	public WordClassDiscountingSmoother(final int minCount, final String oovClassName) { // NO_UCD (use private)
		this.oovClassName = oovClassName;
		this.minCount = minCount;
	}

	Stream<WeightedClassifier> createClassifierWeighting(
			final Stream<Object2DoubleMap.Entry<String>> wordClasses,
			final Function<? super String, ? extends Classifier> wordClassifiers) {
		return wordClasses.map(wordClass -> {
			LOGGER.debug("Getting classifier for class \"{}\".", wordClass);
			Classifier classifier = wordClassifiers.apply(wordClass.getKey());
			if (classifier == null) {
				LOGGER.debug("Getting distribution for OOV classes (\"{}\").", oovClassName);
				classifier = wordClassifiers.apply(oovClassName);
			}
			return new WeightedClassifier(classifier, wordClass.getDoubleValue());
		});
	}

	public Instances redistributeMass(final WordClassificationData trainingData) {
		final List<Entry<String, Instances>> addendClassInsts = createdAddendClassInstList(trainingData, minCount,
				oovClassName);
		if (addendClassInsts.isEmpty()) {
			throw new IllegalArgumentException(
					String.format("Could not find any word classes with fewer than %s instance(s).", minCount));
		}
		// int freqToDiscount = minCount - 1;
		// do {
		// freqToDiscount++;
		// addendClassInsts = createdAddendClassInstList(trainingData,
		// freqToDiscount, oovClassName);
		// } while (addendClassInsts.isEmpty());
		// if (freqToDiscount != minCount) {
		// LOGGER.warn(
		// "Could not find word classes with fewer than {} instance(s); Using
		// those with fewer than {} instad.",
		// minCount, freqToDiscount);
		// }
		// assert !addendClassInsts.isEmpty();
		return redistributeMass(trainingData, oovClassName, addendClassInsts);
	}

	private List<Entry<String, Instances>> createdAddendClassInstList(final WordClassificationData trainingData,
			final int minCount, final String augendClassName) {
		final List<Entry<String, Instances>> result = new ArrayList<>();

		final Map<String, Instances> classInsts = trainingData.getClassInstances();
		for (final ObjectIterator<Object2IntMap.Entry<String>> observationCountIter = trainingData
				.getClassObservationCounts().object2IntEntrySet().iterator(); observationCountIter.hasNext();) {
			final Object2IntMap.Entry<String> observationCount = observationCountIter.next();
			final int count = observationCount.getIntValue();
			if (count < minCount) {
				final String className = observationCount.getKey();
				LOGGER.debug("Class \"{}\" has fewer than {} instances; Will redistribute to \"{}\".", className,
						minCount, augendClassName);
				result.add(new MutablePair<>(className, classInsts.remove(className)));
				observationCountIter.remove();
			}

		}
		return result;
	}

	private Instances redistributeMass(final WordClassificationData trainingData, final String augendClassName,
			final List<Entry<String, Instances>> addendClassInsts) {
		final Instances augendInsts = trainingData.getClassInstances().computeIfAbsent(augendClassName, k -> {
			final int totalInstCount = addendClassInsts.stream().map(Entry::getValue).mapToInt(Instances::numAttributes)
					.sum();
			return classInstsFactory.apply(WordClasses.createRelationName(augendClassName), totalInstCount);
		});
		addendClassInsts.stream().forEach(addendClassInst -> augendInsts.addAll(addendClassInst.getValue()));
		return augendInsts;
	}

}
