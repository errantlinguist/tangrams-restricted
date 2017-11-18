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
import java.util.Map.Entry;
import java.util.function.Function;

import javax.inject.Inject;

import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import it.unimi.dsi.fastutil.objects.Object2DoubleMap;
import it.unimi.dsi.fastutil.objects.Object2ObjectMap;
import it.unimi.dsi.fastutil.objects.Object2ObjectOpenHashMap;
import it.unimi.dsi.fastutil.objects.ObjectCollection;
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

		public static final class Datum {

			private final int observationCount;

			private final int trainingInstancesChangeCount;

			private final int trainingInstCount;

			private Datum(final Datum copyee) {
				this(copyee.getObservationCount(), copyee.getTrainingInstCount(),
						copyee.getTrainingInstancesChangeCount());
			}

			private Datum(final int observationCount, final int trainingInstCount,
					final int trainingInstancesChangeCount) {
				this.observationCount = observationCount;
				this.trainingInstCount = trainingInstCount;
				this.trainingInstancesChangeCount = trainingInstancesChangeCount;
			}

			private Datum(final WordClassificationData.Datum copyee) {
				this(copyee.getObservationCount(), copyee.getTrainingInsts().size(),
						copyee.getTrainingInstancesChangeCount());
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
				if (!(obj instanceof Datum)) {
					return false;
				}
				final Datum other = (Datum) obj;
				if (observationCount != other.observationCount) {
					return false;
				}
				if (trainingInstancesChangeCount != other.trainingInstancesChangeCount) {
					return false;
				}
				return true;
			}

			/**
			 * @return the observationCount
			 */
			public int getObservationCount() {
				return observationCount;
			}

			public int getTrainingInstancesChangeCount() {
				return trainingInstancesChangeCount;
			}

			/**
			 * @return the trainingInstCount
			 */
			public int getTrainingInstCount() {
				return trainingInstCount;
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
				result = prime * result + observationCount;
				result = prime * result + trainingInstancesChangeCount;
				return result;
			}

			/*
			 * (non-Javadoc)
			 *
			 * @see java.lang.Object#toString()
			 */
			@Override
			public String toString() {
				final StringBuilder builder = new StringBuilder(128);
				builder.append("Datum [observationCount=");
				builder.append(observationCount);
				builder.append(", trainingInstancesChangeCount=");
				builder.append(trainingInstancesChangeCount);
				builder.append(", trainingInstCount=");
				builder.append(trainingInstCount);
				builder.append("]");
				return builder.toString();
			}

		}

		private final Object2ObjectMap<String, DiscountedWordClasses.Datum> discountedClassData;

		private final DiscountedWordClasses.Datum oovClassDatum;

		private DiscountedWordClasses(final Object2ObjectMap<String, DiscountedWordClasses.Datum> discountedClassData,
				final DiscountedWordClasses.Datum oovClassDatum) {
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
		public Object2ObjectMap<String, DiscountedWordClasses.Datum> getDiscountedClassData() {
			return discountedClassData;
		}

		/**
		 * @return the oovClassDatum
		 */
		public DiscountedWordClasses.Datum getOovClassDatum() {
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

		/*
		 * (non-Javadoc)
		 *
		 * @see java.lang.Object#toString()
		 */
		@Override
		public String toString() {
			final StringBuilder builder = new StringBuilder(256);
			builder.append("DiscountedWordClasses [discountedClassData=");
			builder.append(discountedClassData);
			builder.append(", oovClassDatum=");
			builder.append(oovClassDatum);
			builder.append("]");
			return builder.toString();
		}

	}

	private static final String DEFAULT_OOV_CLASS = "__OUT_OF_VOCAB__";

	private static final Logger LOGGER = LoggerFactory.getLogger(WordClassDiscountingSmoother.class);

	private static boolean containsNullKey(final Iterable<? extends Entry<String, ?>> entries) {
		boolean result = false;
		for (final Entry<String, ?> entry : entries) {
			final String key = entry.getKey();
			if (key == null) {
				result = true;
				break;
			}
		}
		return result;
	}

	private static <T> Object2ObjectMap<T, DiscountedWordClasses.Datum> createDiscountedWordClassDataMap(
			final Object2ObjectMap<? extends T, WordClassificationData.Datum> discountedWordClasses) {
		final Object2ObjectMap<T, DiscountedWordClasses.Datum> result = new Object2ObjectOpenHashMap<>(
				discountedWordClasses.size() + 1, 1.0f);
		for (final Object2ObjectMap.Entry<? extends T, WordClassificationData.Datum> discountedWordClass : discountedWordClasses
				.object2ObjectEntrySet()) {
			result.put(discountedWordClass.getKey(), new DiscountedWordClasses.Datum(discountedWordClass.getValue()));
		}
		assert result.size() == discountedWordClasses.size();
		return result;
	}

	private static <T> Collection<Entry<T, WordClassificationData.Datum>> findClassesToDiscount(
			final Object2ObjectMap<? extends T, WordClassificationData.Datum> wordClassData) {
		final List<Entry<T, WordClassificationData.Datum>> result = new ArrayList<>();
		for (final Object2ObjectMap.Entry<? extends T, WordClassificationData.Datum> wordClassDatum : wordClassData
				.object2ObjectEntrySet()) {
			final T className = wordClassDatum.getKey();
			assert className != null;
			final WordClassificationData.Datum datum = wordClassDatum.getValue();
			assert datum != null;
			// Create a new Pair instance because it's possible that the original
			// Object2ObjectMap.Entry instances no longer point to valid data after the map
			// which contains them is modified
			result.add(Pair.of(className, datum));
		}
		return result;
	}

	private final int minCount;

	private final String oovClassName;

	@Inject
	private WordClassInstancesFactory wordClassDataFactory;

	public WordClassDiscountingSmoother(final int minCount) {
		this(minCount, DEFAULT_OOV_CLASS);
	}

	public WordClassDiscountingSmoother(final int minCount, final String oovClassName) {
		this.minCount = minCount;
		this.oovClassName = oovClassName;
	}

	/**
	 * @return the minCount
	 */
	public int getMinCount() {
		return minCount;
	}

	/**
	 * @return the oovClassName
	 */
	public String getOovClassName() {
		return oovClassName;
	}

	/**
	 * Calculates which {@link Instances} objects for each unique word class
	 * (i.e.&nbsp;token type) should be discounted, removes them from the given
	 * {@link WordClassificationData} object and puts it into the
	 * {@link #oovClassName out-of-vocabulary label} {@code Instances} object.
	 *
	 * @param trainingData
	 *            The unsmoothed training data.
	 * @return A new {@link DiscountedWordClasses} instance representing information
	 *         about the word classes used for discounting and the {@link Instances}
	 *         objects for each.
	 */
	public DiscountedWordClasses redistributeMass(
			final Object2ObjectMap<String, WordClassificationData.Datum> wordClassData) {
		final Object2ObjectMap<String, WordClassificationData.Datum> wordClassesToDiscount = createdAddendClassInstsMap(
				wordClassData);
		if (wordClassesToDiscount.isEmpty()) {
			throw new IllegalArgumentException(
					String.format("Could not find any word classes with fewer than %s instance(s).", minCount));
		}

		// The new set of word classes is different from the previous one;
		// Re-redistribute instances to the OOV class
		final WordClassificationData.Datum oovClassDatum = redistributeMassToOovClass(wordClassData,
				wordClassesToDiscount.object2ObjectEntrySet());
		final Object2ObjectMap<String, DiscountedWordClasses.Datum> discountedWordClassData = createDiscountedWordClassDataMap(
				wordClassesToDiscount);
		return new DiscountedWordClasses(discountedWordClassData, new DiscountedWordClasses.Datum(oovClassDatum));
	}

	private void addSmoothedClassifierWeighting(final String wordClass, final double addendWeight,
			final Object2ObjectMap<String, WeightedClassifier> alreadyObservedWordClassifiers,
			final Function<? super String, ? extends Classifier> wordClassifiers) {
		final WeightedClassifier alreadyObservedClassifier = alreadyObservedWordClassifiers.get(wordClass);
		if (alreadyObservedClassifier == null) {
			// The given word class hasn't yet been used
			final Classifier wordClassifier = wordClassifiers.apply(wordClass);
			if (wordClassifier == null) {
				// This word class was not seen in the training data; Use the
				// OOV class instead
				LOGGER.debug("Getting distribution for OOV class (\"{}\").", oovClassName);
				final Classifier oovClassifier = wordClassifiers.apply(oovClassName);
				final WeightedClassifier newWeightedClassifier = new WeightedClassifier(oovClassifier, addendWeight);
				alreadyObservedWordClassifiers.put(oovClassName, newWeightedClassifier);
			} else {
				// There is a classifier trained for the word class even though
				// it hasn't yet been used in this sequence
				final WeightedClassifier newWeightedClassifier = new WeightedClassifier(wordClassifier, addendWeight);
				alreadyObservedWordClassifiers.put(wordClass, newWeightedClassifier);
			}
		} else {
			alreadyObservedClassifier.setWeight(alreadyObservedClassifier.getWeight() + addendWeight);
		}
	}

	private Object2ObjectMap<String, WordClassificationData.Datum> createdAddendClassInstsMap(
			final Object2ObjectMap<String, WordClassificationData.Datum> wordClassData) {
		final Collection<Entry<String, WordClassificationData.Datum>> wordClassesToDiscount = findClassesToDiscount(
				wordClassData);
		// Assert that there are no duplicate keys
		assert wordClassesToDiscount.stream().map(Entry::getKey).distinct().count() == wordClassesToDiscount.size();
		assert !containsNullKey(wordClassesToDiscount);
		final Object2ObjectMap<String, WordClassificationData.Datum> result = new Object2ObjectOpenHashMap<>(
				wordClassesToDiscount.size() + 1, 1.0f);
		for (final Entry<String, WordClassificationData.Datum> wordClassToDiscount : wordClassesToDiscount) {
			final String className = wordClassToDiscount.getKey();
			assert className != null;
			LOGGER.debug("Class \"{}\" has fewer than {} instances; Will redistribute to \"{}\".", className, minCount,
					oovClassName);
			result.put(className, wordClassData.remove(className));
		}
		return result;
	}

	private WordClassificationData.Datum redistributeMassToOovClass(
			final Object2ObjectMap<String, WordClassificationData.Datum> wordClassData,
			final ObjectCollection<Object2ObjectMap.Entry<String, WordClassificationData.Datum>> addendWordClassData) {
		final WordClassificationData.Datum result = wordClassData.computeIfAbsent(oovClassName, k -> {
			final int totalInstCount = addendWordClassData.stream().map(Object2ObjectMap.Entry::getValue)
					.map(WordClassificationData.Datum::getTrainingInsts).mapToInt(Instances::size).sum();
			final Instances oovClassDatum = wordClassDataFactory.apply(WordClasses.createRelationName(k),
					totalInstCount);
			return new WordClassificationData.Datum(oovClassDatum);
		});
		addendWordClassData.stream().map(Object2ObjectMap.Entry::getValue).forEach(result::add);

		assert !result.getTrainingInsts().isEmpty();
		assert addendWordClassData.stream().map(Object2ObjectMap.Entry::getValue)
				.mapToInt(WordClassificationData.Datum::getObservationCount).sum() == result.getObservationCount();
		assert addendWordClassData.stream().map(Object2ObjectMap.Entry::getValue)
				.map(WordClassificationData.Datum::getTrainingInsts).mapToInt(Instances::size)
				.sum() == result.getTrainingInsts().size();
		return result;
	}

	Object2ObjectMap<String, WeightedClassifier> createWeightedClassifierMap(
			final ObjectCollection<Object2DoubleMap.Entry<String>> wordClassWeights,
			final Function<? super String, ? extends Classifier> wordClassifiers) {
		// There are likely fewer result classifiers than the input size because
		// some classes will likely be OOV
		final Object2ObjectMap<String, WeightedClassifier> result = new Object2ObjectOpenHashMap<>(
				wordClassWeights.size() + 1, 1.0f);
		for (final Object2DoubleMap.Entry<String> wordClassWeight : wordClassWeights) {
			final String wordClass = wordClassWeight.getKey();
			LOGGER.debug("Getting classifier for class \"{}\".", wordClass);
			final double weight = wordClassWeight.getDoubleValue();
			addSmoothedClassifierWeighting(wordClass, weight, result, wordClassifiers);
		}
		return result;
	}

}
