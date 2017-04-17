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
package se.kth.speech.coin.tangrams.analysis.vocab;

import java.util.Collection;
import java.util.List;
import java.util.Locale;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.stream.Stream;

import se.kth.speech.coin.tangrams.analysis.Utterance;
import se.kth.speech.hat.xsd.Annotation;
import se.kth.speech.hat.xsd.Annotation.Segments.Segment;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Apr 17, 2017
 *
 */
final class WordLists {

	static final class WordAccumulator<C extends Collection<? super String>> implements BiConsumer<C, Annotation> {

		private final Function<Segment, ? extends Collection<Utterance>> segUttFactory;

		WordAccumulator(final Function<Segment, ? extends Collection<Utterance>> segUttFactory) {
			this.segUttFactory = segUttFactory;
		}

		@Override
		public void accept(final C wordList, final Annotation uttAnnots) {
			final List<Segment> segments = uttAnnots.getSegments().getSegment();
			final Stream<Utterance> utts = segments.stream().map(segUttFactory).map(Collection::stream)
					.flatMap(Function.identity());
			final Stream<String> words = utts.map(Utterance::getTokens).map(List::stream).flatMap(Function.identity());
			words.forEach(wordList::add);
		}
	}

	public static final Locale DEFAULT_LOCALE = Locale.US;

	private WordLists() {
	}

}
