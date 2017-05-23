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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags;

import java.util.Arrays;
import java.util.List;
import java.util.function.Predicate;

import se.kth.speech.coin.tangrams.analysis.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.Utterance;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 23 May 2017
 *
 */
public final class UtteranceFilteringEventDialogueFactory implements EventDialogueTransformer {

	private final Predicate<? super Utterance> uttFilter;

	public UtteranceFilteringEventDialogueFactory(final Predicate<? super Utterance> uttFilter) {
		this.uttFilter = uttFilter;
	}

	@Override
	public EventDialogue apply(final EventDialogue diag) {
		final List<Utterance> filteredUtts = Arrays
				.asList(diag.getUtts().stream().filter(uttFilter).toArray(Utterance[]::new));
		return new EventDialogue(diag.getLastEvent(), filteredUtts);
	}

}
