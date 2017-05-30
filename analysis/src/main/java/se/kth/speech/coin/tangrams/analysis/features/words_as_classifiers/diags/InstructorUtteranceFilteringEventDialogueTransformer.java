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
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.function.Predicate;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.coin.tangrams.analysis.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.Utterance;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 23 May 2017
 *
 */
public final class InstructorUtteranceFilteringEventDialogueTransformer implements EventDialogueTransformer {

	private static final List<Utterance> EMPTY_UTT_LIST = Collections.emptyList();

	private static final Logger LOGGER = LoggerFactory
			.getLogger(InstructorUtteranceFilteringEventDialogueTransformer.class);

	private static Optional<List<Utterance>> createInstructorUttList(final EventDialogue uttDiag) {
		return uttDiag.getFirstEvent().map(event -> {
			LOGGER.debug("Classifying entity referred to by instructor for {}.", event);
			final Predicate<Utterance> instructorUttMatcher = UtteranceMatchers
					.createEventSubmitterUtteranceMatcher(event);
			final List<Utterance> allUtts = uttDiag.getUtts();
			return Arrays.asList(allUtts.stream().filter(instructorUttMatcher).toArray(Utterance[]::new));
		});
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Function#apply(java.lang.Object)
	 */
	@Override
	public EventDialogue apply(final EventDialogue diag) {
		final List<Utterance> filteredUtts = createInstructorUttList(diag).orElse(EMPTY_UTT_LIST);
		return new EventDialogue(diag.getDialogueEvents(), filteredUtts);
	}

}
