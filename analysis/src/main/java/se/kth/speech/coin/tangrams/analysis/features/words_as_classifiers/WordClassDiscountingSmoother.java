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
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.inject.Inject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Maps;

import it.unimi.dsi.fastutil.objects.Object2DoubleMap;
import se.kth.speech.coin.tangrams.analysis.features.weka.WordClassInstancesFactory;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.WordClassificationData;
import weka.classifiers.Classifier;
import weka.core.Instances;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 16 May 2017
 * @see <a href="http://www.aclweb.org/anthology/P15-1029">Casey Kennington
 *      &amp; David Schlangen. &ldquo;Simple Learning and Compositional
 *      Application of Perceptually Grounded Word Meanings for Incremental
 *      Reference Resolution&rdquo;. In <em>Proceedings of the 53<sup>rd</sup>
 *      Annual Meeting of the Association for Computational Linguistics and the
 *      7<sup>th</sup> International Joint Conference on Natural Language
 *      Processing</em>.</a>
 */
public final class WordClassDiscountingSmoother {

	public static final class DiscountedWordClasses {

		private final Map<String, WordClassificationData.Datum> discountedClassData;

		private final WordClassificationData.Datum oovClassDatum;

		private DiscountedWordClasses(final Map<String, WordClassificationData.Datum> discountedClassData,
				final WordClassificationData.Datum oovClassDatum) {
			this.discountedClassData = discountedClassData;
			this.oovClassDatum = oovClassDatum;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.lang.Object#equals(java.lang.Object)
		 */
		@Override
		public boolean equals(final Object obj) {
			if (this == obj) {
				return true;
			}
			if (obj == null) {
				return false;
			}
			if (!(obj instanceof DiscountedWordClasses)) {
				return false;
			}
			final DiscountedWordClasses other = (DiscountedWordClasses) obj;
			if (oovClassDatum == null) {
				if (other.oovClassDatum != null) {
					return false;
				}
			} else if (!oovClassDatum.equals(other.oovClassDatum)) {
				return false;
			}
			if (discountedClassData == null) {
				if (other.discountedClassData != null) {
					return false;
				}
			} else if (!discountedClassData.equals(other.discountedClassData)) {
				return false;
			}
			return true;
		}

		/**
		 * @return the discountedClassData
		 */
		public Map<String, WordClassificationData.Datum> getDiscountedClassData() {
			return discountedClassData;
		}

		/**
		 * @return the oovClassDatum
		 */
		public WordClassificationData.Datum getOovClassDatum() {
			return oovClassDatum;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.lang.Object#hashCode()
		 */
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + (oovClassDatum == null ? 0 : oovClassDatum.hashCode());
			result = prime * result + (discountedClassData == null ? 0 : discountedClassData.hashCode());
			return result;
		}

	}

	private static final Logger LOGGER = LoggerFactory.getLogger(WordClassDiscountingSmoother.class);

	@Inject
	private WordClassInstancesFactory classInstsFactory;

	private final int minCount;

	private final String oovClassName;

	public WordClassDiscountingSmoother(final int minCount, final String oovClassName) { // NO_UCD
																							// (use
																							// private)
		this.oovClassName = oovClassName;
		this.minCount = minCount;
	}

	/**
	 * Calculates which {@link Instances} objects for each unique word class
	 * (i.e.&nbsp;token type) should be discounted, removes them from the given
	 * {@link WordClassificationData} object and puts it into the
	 * {@link #oovClassName out-of-vocabulary label} {@code Instances} object.
	 *
	 * @param trainingData
	 *            The unsmoothed training data.
	 * @return A new {@link DiscountedWordClasses} instance representing
	 *         information about the word classes used for discounting and the
	 *         {@link Instances} objects for each.
	 */
	public DiscountedWordClasses redistributeMass(final WordClassificationData trainingData) {
		final Map<String, WordClassificationData.Datum> wordClassesToDiscount = createdAddendClassInstsMap(
				trainingData);
		if (wordClassesToDiscount.isEmpty()) {
			throw new IllegalArgumentException(
					String.format("Could not find any word classes with fewer than %s instance(s).", minCount));
		}

		// The new set of word classes is different from the previous one;
		// Re-redistribute instances to the OOV class
		final WordClassificationData.Datum oovClassDatum = redistributeMassToOovClass(trainingData,
				wordClassesToDiscount.entrySet());
		return new DiscountedWordClasses(wordClassesToDiscount, oovClassDatum);
	}

	/**
	 * Calculates which {@link Instances} objects for each unique word class
	 * (i.e.&nbsp;token type) should be discounted, removes them from the given
	 * {@link WordClassificationData} object and puts it into the
	 * {@link #oovClassName out-of-vocabulary label} {@code Instances} object.
	 *
	 * @param trainingData
	 *            The unsmoothed training data.
	 * @return A new {@link DiscountedWordClasses} instance representing
	 *         information about the word classes used for discounting and the
	 *         {@link Instances} objects for each.
	 */
	public DiscountedWordClasses redistributeMass(final WordClassificationData trainingData,
			final Map<String, WordClassificationData.Datum> previouslyDiscountedWordClassInsts) {
		final Map<String, WordClassificationData.Datum> wordClassesToDiscount = createdAddendClassInstsMap(
				trainingData);
		if (wordClassesToDiscount.isEmpty()) {
			throw new IllegalArgumentException(
					String.format("Could not find any word classes with fewer than %s instance(s).", minCount));
		}

		final DiscountedWordClasses result;
		if (wordClassesToDiscount.equals(previouslyDiscountedWordClassInsts)) {
			LOGGER.debug(
					"Set of discounted classes is identical to those already used for smoothing; Skipping redistribution of {} class(es).",
					wordClassesToDiscount.size());
			final WordClassificationData.Datum oovClassDatum = getOovClassDatum(trainingData);
			assert oovClassDatum != null;
			// Re-use the old discounted class set to avoid causing havoc with
			// code which relies on object identity rather than equivalence
			result = new DiscountedWordClasses(previouslyDiscountedWordClassInsts, oovClassDatum);
		} else {
			// The new set of word classes is different from the previous one;
			// Re-redistribute instances to the OOV class
			final WordClassificationData.Datum oovClassDatum = redistributeMassToOovClass(trainingData,
					wordClassesToDiscount.entrySet());
			result = new DiscountedWordClasses(wordClassesToDiscount, oovClassDatum);
		}
		return result;
	}

	private Map<String, WordClassificationData.Datum> createdAddendClassInstsMap(
			final WordClassificationData trainingData) {
		final Map<String, WordClassificationData.Datum> classInsts = trainingData.getClassData();
		final List<Entry<String, WordClassificationData.Datum>> wordClassesToDiscount = findClassesToDiscount(
				classInsts.entrySet()).collect(Collectors.toCollection(() -> new ArrayList<>(classInsts.size())));
		final Map<String, WordClassificationData.Datum> result = Maps
				.newHashMapWithExpectedSize(wordClassesToDiscount.size());

		for (final Entry<String, WordClassificationData.Datum> wordClassToDiscount : wordClassesToDiscount) {
			final String className = wordClassToDiscount.getKey();
			LOGGER.debug("Class \"{}\" has fewer than {} instances; Will redistribute to \"{}\".", className, minCount,
					oovClassName);
			result.put(className, classInsts.remove(className));
		}
		return result;
	}

	private Stream<Entry<String, WordClassificationData.Datum>> findClassesToDiscount(
			final Collection<Entry<String, WordClassificationData.Datum>> wordClassData) {
		return wordClassData.stream()
				.filter(observationCount -> observationCount.getValue().getObservationCount() < minCount);
	}

	private WordClassificationData.Datum getOovClassDatum(final WordClassificationData trainingData) {
		return trainingData.getClassData().get(oovClassName);
	}

	private WordClassificationData.Datum redistributeMassToOovClass(final WordClassificationData trainingData,
			final Collection<Entry<String, WordClassificationData.Datum>> addendWordClassData) {
		final WordClassificationData.Datum result = trainingData.getClassData().computeIfAbsent(oovClassName, k -> {
			final int totalInstCount = addendWordClassData.stream().map(Entry::getValue)
					.map(WordClassificationData.Datum::getTrainingInsts).mapToInt(Instances::size).sum();
			final Instances oovClassDatum = classInstsFactory.apply(WordClasses.createRelationName(k), totalInstCount);
			return new WordClassificationData.Datum(oovClassDatum);
		});
		addendWordClassData.stream().map(Entry::getValue).forEach(result::add);
		return result;
	}

	Stream<WeightedClassifier> createClassifierWeighting(final Stream<Object2DoubleMap.Entry<String>> wordClasses,
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

}
