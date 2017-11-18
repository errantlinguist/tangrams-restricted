package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training;

import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.Future;

import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.WordClassDiscountingSmoother.DiscountedWordClasses;
import weka.classifiers.Classifier;

/**
 *
 * * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 *
 * @since 18 Nov 2017
 *
 * @param <C>
 *            The type of {@link Classifier} trained.
 */
public final class TrainingResults<C extends Classifier> {

	/**
	 * A {@link DiscountedWordClasses} instance representing each word class used
	 * for discounting during smoothing, mapped to its corresponding
	 * {@link DiscountedWordClasses.Datum}.
	 */
	private final DiscountedWordClasses discountedWordClasses;

	/**
	 * A {@link Future future} of the {@link ConcurrentMap} of word classifiers to
	 * use for the next dialogue being classified.
	 */
	private final Future<ConcurrentMap<String, C>> futureWordClassifiers;

	/**
	 *
	 * @param futureWordClassifiers
	 *            A {@link Future future} of the {@link ConcurrentMap} of word
	 *            classifiers to use for the next dialogue being classified.
	 * @param discountedWordClasses
	 *            A {@link DiscountedWordClasses} of each word class used for
	 *            discounting during smoothing mapped to its corresponding
	 *            {@link DiscountedWordClasses.Datum}.
	 */
	TrainingResults(final Future<ConcurrentMap<String, C>> futureWordClassifiers,
			final DiscountedWordClasses discountedWordClasses) {
		this.futureWordClassifiers = futureWordClassifiers;
		this.discountedWordClasses = discountedWordClasses;
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
		if (!(obj instanceof TrainingResults)) {
			return false;
		}
		final TrainingResults<?> other = (TrainingResults<?>) obj;
		if (futureWordClassifiers == null) {
			if (other.futureWordClassifiers != null) {
				return false;
			}
		} else if (!futureWordClassifiers.equals(other.futureWordClassifiers)) {
			return false;
		}
		if (discountedWordClasses == null) {
			if (other.discountedWordClasses != null) {
				return false;
			}
		} else if (!discountedWordClasses.equals(other.discountedWordClasses)) {
			return false;
		}
		return true;
	}

	/**
	 * @return A {@link DiscountedWordClasses} instance representing each word class
	 *         used for discounting during smoothing, mapped to its corresponding
	 *         {@link DiscountedWordClasses.Datum}.
	 */
	public DiscountedWordClasses getDiscountedWordClasses() {
		return discountedWordClasses;
	}

	/**
	 * @return A {@link Future future} of the {@link ConcurrentMap} of word
	 *         classifiers to use for the next dialogue being classified.
	 */
	public Future<ConcurrentMap<String, C>> getFutureWordClassifiers() {
		return futureWordClassifiers;
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
		result = prime * result + (futureWordClassifiers == null ? 0 : futureWordClassifiers.hashCode());
		result = prime * result + (discountedWordClasses == null ? 0 : discountedWordClasses.hashCode());
		return result;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder(512);
		builder.append("TrainingResults [lastDiscountedWordClasses=");
		builder.append(discountedWordClasses);
		builder.append(", futureWordClassifiers=");
		builder.append(futureWordClassifiers);
		builder.append("]");
		return builder.toString();
	}
}