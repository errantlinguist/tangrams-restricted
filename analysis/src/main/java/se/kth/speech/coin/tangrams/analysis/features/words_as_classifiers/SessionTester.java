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

import java.util.List;
import java.util.Optional;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.MutablePair;
import se.kth.speech.coin.tangrams.analysis.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.SessionEventDialogueManager;
import se.kth.speech.coin.tangrams.analysis.features.ClassificationException;

public final class SessionTester {

	private static final Logger LOGGER = LoggerFactory.getLogger(SessionTester.class);

	private final EventDialogueTester diagTester;

	/**
	 * @param wordClassifiers
	 * @param testInsts
	 */
	public SessionTester(final EventDialogueTester diagTester) {
		this.diagTester = diagTester;
	}

	public SessionTestResults apply(final SessionEventDialogueManager sessionEventDiagMgr)
			throws ClassificationException {
		final List<EventDialogue> uttDiags = sessionEventDiagMgr.getUttDialogues();
		final SessionTestResults result = new SessionTestResults(uttDiags.size());

		LOGGER.info("Testing {} individual dialogue(s).", uttDiags.size());
		for (final EventDialogue uttDiag : uttDiags) {
			final Optional<EventDialogueTestResults> optTestResults = diagTester.apply(uttDiag,
					sessionEventDiagMgr.getGameHistory());
			if (optTestResults.isPresent()) {
				final EventDialogueTestResults results = optTestResults.get();
				result.add(new MutablePair<>(uttDiag, results));
			} else {
				LOGGER.debug("No utterances tested for {}.", uttDiag);
			}
		}
		return result;
	}

}