/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.analysis.
 *
 *  tangrams is free software: you can redistribute it and/or modify
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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags;

import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map.Entry;
import java.util.function.BiFunction;
import java.util.function.Predicate;
import java.util.function.ToDoubleFunction;
import java.util.stream.Stream;

import iristk.system.Event;
import it.unimi.dsi.fastutil.objects.Object2DoubleMap;
import it.unimi.dsi.fastutil.objects.Object2DoubleOpenHashMap;
import se.kth.speech.Iterators;
import se.kth.speech.coin.tangrams.analysis.Utterance;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Jul 9, 2017
 *
 */
public final class SentimentAnalyzingEventDialogueUtteranceSorter
		implements BiFunction<List<Utterance>, Event, SentimentAnalyzingEventDialogueUtteranceSorter.Result> {

	public static final class Result {

		private final List<Utterance> otherEntityNegativeExamples;

		private final List<Utterance> refNegExamples;

		private final List<Utterance> refPosExamples;

		private final Object2DoubleMap<Utterance> uttSentimentRanks;

		private Result(final List<Utterance> refPosExamples, final List<Utterance> refNegExamples,
				final List<Utterance> otherEntityNegativeExamples,
				final Object2DoubleMap<Utterance> uttSentimentRanks) {
			this.refPosExamples = refPosExamples;
			this.refNegExamples = refNegExamples;
			this.otherEntityNegativeExamples = otherEntityNegativeExamples;
			this.uttSentimentRanks = uttSentimentRanks;
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
			if (!(obj instanceof Result)) {
				return false;
			}
			final Result other = (Result) obj;
			if (otherEntityNegativeExamples == null) {
				if (other.otherEntityNegativeExamples != null) {
					return false;
				}
			} else if (!otherEntityNegativeExamples.equals(other.otherEntityNegativeExamples)) {
				return false;
			}
			if (refNegExamples == null) {
				if (other.refNegExamples != null) {
					return false;
				}
			} else if (!refNegExamples.equals(other.refNegExamples)) {
				return false;
			}
			if (refPosExamples == null) {
				if (other.refPosExamples != null) {
					return false;
				}
			} else if (!refPosExamples.equals(other.refPosExamples)) {
				return false;
			}
			if (uttSentimentRanks == null) {
				if (other.uttSentimentRanks != null) {
					return false;
				}
			} else if (!uttSentimentRanks.equals(other.uttSentimentRanks)) {
				return false;
			}
			return true;
		}

		/**
		 * @return the otherEntityNegativeExamples
		 */
		public List<Utterance> getOtherEntityNegativeExamples() {
			return otherEntityNegativeExamples;
		}

		/**
		 * @return the refNegExamples
		 */
		public List<Utterance> getRefNegExamples() {
			return refNegExamples;
		}

		/**
		 * @return the refPosExamples
		 */
		public List<Utterance> getRefPosExamples() {
			return refPosExamples;
		}

		/**
		 * @return the uttSentimentRanks
		 */
		public Object2DoubleMap<Utterance> getUttSentimentRanks() {
			return uttSentimentRanks;
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
			result = prime * result
					+ (otherEntityNegativeExamples == null ? 0 : otherEntityNegativeExamples.hashCode());
			result = prime * result + (refNegExamples == null ? 0 : refNegExamples.hashCode());
			result = prime * result + (refPosExamples == null ? 0 : refPosExamples.hashCode());
			result = prime * result + (uttSentimentRanks == null ? 0 : uttSentimentRanks.hashCode());
			return result;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.lang.Object#toString()
		 */
		@Override
		public String toString() {
			final StringBuilder builder = new StringBuilder();
			builder.append("Result [refPosExamples=");
			builder.append(refPosExamples);
			builder.append(", refNegExamples=");
			builder.append(refNegExamples);
			builder.append(", otherEntityNegativeExamples=");
			builder.append(otherEntityNegativeExamples);
			builder.append(", uttSentimentRanks=");
			builder.append(uttSentimentRanks);
			builder.append("]");
			return builder.toString();
		}

	}

	private final ToDoubleFunction<? super Utterance> uttSentimentRanker;

	public SentimentAnalyzingEventDialogueUtteranceSorter(
			final ToDoubleFunction<? super Utterance> uttSentimentRanker) {
		this.uttSentimentRanker = uttSentimentRanker;
	}

	@Override
	public Result apply(final List<Utterance> utts, final Event event) {
		return apply(utts, UtteranceMatchers.createEventSubmitterUtteranceMatcher(event));
	}

	private Result apply(final List<Utterance> utts, final Predicate<Utterance> instructorUttMatcher) {
		final int listLen = utts.isEmpty() ? 0 : utts.size() / 3 + 1;
		final List<Utterance> refPosExamples = new ArrayList<>(listLen);
		final List<Utterance> refNegExamples = new ArrayList<>(listLen);
		final List<Utterance> otherEntityNegativeExamples = new ArrayList<>(listLen);
		final Object2DoubleMap<Utterance> uttSentimentRanks = new Object2DoubleOpenHashMap<>(listLen);

		final ListIterator<Utterance> uttIter = utts.listIterator();
		while (uttIter.hasNext()) {
			final Entry<Stream<Utterance>, Utterance> preInstructorUtts = Iterators.findElementsBeforeDelimiter(uttIter,
					instructorUttMatcher);
			final Utterance firstInstructorUtt = preInstructorUtts.getValue();
			refPosExamples.add(firstInstructorUtt);
			otherEntityNegativeExamples.add(firstInstructorUtt);

			{
				// Handle preceding non-instructor utterances
				final double firstInstructorUttSentimentRank = uttSentimentRanker.applyAsDouble(firstInstructorUtt);
				uttSentimentRanks.put(firstInstructorUtt, firstInstructorUttSentimentRank);
				if (firstInstructorUttSentimentRank < 0) {
					// Use the other player's utterances which came
					// before this instructor utterance as negative
					// examples
					preInstructorUtts.getKey().forEach(refNegExamples::add);
				} else if (firstInstructorUttSentimentRank > 0) {
					// Use the other player's utterances which came
					// before this instructor utterance as positive
					// examples
					preInstructorUtts.getKey().forEach(utt -> {
						refPosExamples.add(utt);
						// For each entity which is selected, process a
						// positive example for this observation: The
						// utterance being processed DOES correspond to
						// the selected entity
						otherEntityNegativeExamples.add(utt);
					});
				}
			}
		}

		return new Result(refPosExamples, refNegExamples, otherEntityNegativeExamples, uttSentimentRanks);
	}

}
