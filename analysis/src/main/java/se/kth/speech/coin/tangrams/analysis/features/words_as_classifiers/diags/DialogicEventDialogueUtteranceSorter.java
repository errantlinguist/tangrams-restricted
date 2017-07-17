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
import java.util.Arrays;
import java.util.List;
import java.util.ListIterator;
import java.util.Map.Entry;
import java.util.function.BiFunction;
import java.util.function.Predicate;
import java.util.function.ToDoubleFunction;
import java.util.stream.Stream;

import iristk.system.Event;
import se.kth.speech.Iterators;
import se.kth.speech.coin.tangrams.analysis.Utterance;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Jul 9, 2017
 *
 */
public final class DialogicEventDialogueUtteranceSorter
		implements BiFunction<List<Utterance>, Event, List<UtteranceRelation>> {

	private final ToDoubleFunction<? super Utterance> uttAcceptanceRanker;

	public DialogicEventDialogueUtteranceSorter(
			final ToDoubleFunction<? super Utterance> uttAcceptanceRanker) {
		this.uttAcceptanceRanker = uttAcceptanceRanker;
	}

	@Override
	public List<UtteranceRelation> apply(final List<Utterance> utts, final Event event) {
		return apply(utts, UtteranceMatchers.createEventSubmitterUtteranceMatcher(event));
	}

	private List<UtteranceRelation> apply(final List<Utterance> utts, final Predicate<Utterance> instructorUttMatcher) {
		final List<UtteranceRelation> result = new ArrayList<>(utts.size() / 2 + 1);
		final ListIterator<Utterance> uttIter = utts.listIterator();
		while (uttIter.hasNext()) {
			final Entry<Stream<Utterance>, Utterance> preInstructorUtts = Iterators.findElementsBeforeDelimiter(uttIter,
					instructorUttMatcher);
			final Utterance firstInstructorUtt = preInstructorUtts.getValue();

			final double firstInstructorUttAcceptanceRank = uttAcceptanceRanker.applyAsDouble(firstInstructorUtt);
			result.add(new UtteranceRelation(firstInstructorUtt, firstInstructorUttAcceptanceRank,
					Arrays.asList(preInstructorUtts.getKey().toArray(Utterance[]::new))));
		}

		return result;
	}

}
