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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Stream;

import javax.annotation.Nonnull;

import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.coin.tangrams.analysis.dialogues.WeightedUtterance;
import se.kth.speech.math.NumberTypeConversions;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 14 Nov 2017
 *
 */
final class InstructorUtteranceWeighter implements Function<EventDialogue, Stream<WeightedUtterance>> {

	private static class ConstantWeightedUtteranceFactory implements Function<Utterance, WeightedUtterance> {

		private final double weight;

		private ConstantWeightedUtteranceFactory(final double weight) {
			this.weight = weight;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.util.function.Function#apply(java.lang.Object)
		 */
		@Override
		@Nonnull
		public WeightedUtterance apply(final Utterance utt) {
			return new WeightedUtterance(utt, weight);
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
			if (!(obj instanceof ConstantWeightedUtteranceFactory)) {
				return false;
			}
			final ConstantWeightedUtteranceFactory other = (ConstantWeightedUtteranceFactory) obj;
			if (Double.doubleToLongBits(weight) != Double.doubleToLongBits(other.weight)) {
				return false;
			}
			return true;
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
			long temp;
			temp = Double.doubleToLongBits(weight);
			result = prime * result + (int) (temp ^ temp >>> 32);
			return result;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.lang.Object#toString()
		 */
		@Override
		public String toString() {
			final StringBuilder builder = new StringBuilder(64);
			builder.append("ConstantWeightedUtteranceFactory [weight=");
			builder.append(weight);
			builder.append("]");
			return builder.toString();
		}

	}

	/**
	 * Always returns {@code null}.
	 */
	private static final Function<Utterance, WeightedUtterance> NULL_WEIGHTED_UTT_FACTORY = utt -> null;

	private static Function<Utterance, WeightedUtterance> createWeightedUtteranceFactory(final BigDecimal weight) {
		final Function<Utterance, WeightedUtterance> result;
		final double doubleWeight = NumberTypeConversions.nonInfiniteDoubleValue(weight);
		final int cmp = weight.compareTo(BigDecimal.ZERO);
		if (cmp > 0) {
			result = new ConstantWeightedUtteranceFactory(doubleWeight);
		} else if (cmp == 0) {
			result = NULL_WEIGHTED_UTT_FACTORY;
		} else {
			throw new IllegalArgumentException(String.format("Weight was %s but must be non-negative.", weight));
		}
		return result;
	}

	private final Function<? super Utterance, WeightedUtterance> weightedInstrUttFactory;

	private final Function<? super Utterance, WeightedUtterance> weightedOtherUttFactory;

	InstructorUtteranceWeighter(final BigDecimal instrUttObservationWeight, final BigDecimal otherUttObsevationWeight) {
		weightedInstrUttFactory = createWeightedUtteranceFactory(instrUttObservationWeight);
		weightedOtherUttFactory = createWeightedUtteranceFactory(otherUttObsevationWeight);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Function#apply(java.lang.Object)
	 */
	@Override
	public Stream<WeightedUtterance> apply(final EventDialogue diag) {
		return diag.getFirstEvent().map(event -> {
			final Predicate<Utterance> instrUttMatcher = UtteranceMatchers.createEventSubmitterUtteranceMatcher(event);
			final List<Utterance> utts = diag.getUtterances();
			final List<WeightedUtterance> weightedUtts = new ArrayList<>(utts.size());
			for (final Utterance utt : utts) {
				final Function<? super Utterance, WeightedUtterance> weightedUttFactory = instrUttMatcher.test(utt)
						? weightedInstrUttFactory : weightedOtherUttFactory;
				final WeightedUtterance weightedUtt = weightedUttFactory.apply(utt);
				if (weightedUtt != null) {
					weightedUtts.add(weightedUtt);
				}
			}
			return weightedUtts.stream();
		}).orElseGet(Stream::empty);
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
		if (!(obj instanceof InstructorUtteranceWeighter)) {
			return false;
		}
		final InstructorUtteranceWeighter other = (InstructorUtteranceWeighter) obj;
		if (weightedInstrUttFactory == null) {
			if (other.weightedInstrUttFactory != null) {
				return false;
			}
		} else if (!weightedInstrUttFactory.equals(other.weightedInstrUttFactory)) {
			return false;
		}
		if (weightedOtherUttFactory == null) {
			if (other.weightedOtherUttFactory != null) {
				return false;
			}
		} else if (!weightedOtherUttFactory.equals(other.weightedOtherUttFactory)) {
			return false;
		}
		return true;
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
		result = prime * result + (weightedInstrUttFactory == null ? 0 : weightedInstrUttFactory.hashCode());
		result = prime * result + (weightedOtherUttFactory == null ? 0 : weightedOtherUttFactory.hashCode());
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
		builder.append("InstructorUtteranceWeighter [weightedInstrUttFactory=");
		builder.append(weightedInstrUttFactory);
		builder.append(", weightedOtherUttFactory=");
		builder.append(weightedOtherUttFactory);
		builder.append("]");
		return builder.toString();
	}

}
