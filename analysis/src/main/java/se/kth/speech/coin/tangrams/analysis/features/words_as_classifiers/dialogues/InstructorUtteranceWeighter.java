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

import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Stream;

import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.coin.tangrams.analysis.dialogues.WeightedUtterance;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 14 Nov 2017
 *
 */
public final class InstructorUtteranceWeighter implements Function<EventDialogue, Stream<WeightedUtterance>> {

	private final double instrUttObservationWeight;

	private final double otherUttObsevationWeight;

	public InstructorUtteranceWeighter(final double instrUttObservationWeight, final double otherUttObsevationWeight) {
		assert instrUttObservationWeight >= 0;
		this.instrUttObservationWeight = instrUttObservationWeight;
		assert otherUttObsevationWeight >= 0;
		this.otherUttObsevationWeight = otherUttObsevationWeight;
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
			final List<Utterance> uttsToClassify = diag.getUtterances();
			return uttsToClassify.stream().map(utt -> {
				final double weight = instrUttMatcher.test(utt) ? instrUttObservationWeight : otherUttObsevationWeight;
				return new WeightedUtterance(utt, weight);
			});
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
		if (Double.doubleToLongBits(instrUttObservationWeight) != Double
				.doubleToLongBits(other.instrUttObservationWeight)) {
			return false;
		}
		if (Double.doubleToLongBits(otherUttObsevationWeight) != Double
				.doubleToLongBits(other.otherUttObsevationWeight)) {
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
		temp = Double.doubleToLongBits(instrUttObservationWeight);
		result = prime * result + (int) (temp ^ temp >>> 32);
		temp = Double.doubleToLongBits(otherUttObsevationWeight);
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
		final StringBuilder builder = new StringBuilder(128);
		builder.append("InstructorUtteranceWeighter [instrUttObservationWeight=");
		builder.append(instrUttObservationWeight);
		builder.append(", otherUttObsevationWeight=");
		builder.append(otherUttObsevationWeight);
		builder.append("]");
		return builder.toString();
	}

}
