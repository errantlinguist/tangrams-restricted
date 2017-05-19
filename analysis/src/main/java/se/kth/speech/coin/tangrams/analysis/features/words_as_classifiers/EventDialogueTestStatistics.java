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

import se.kth.speech.coin.tangrams.analysis.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.Utterance;

public interface EventDialogueTestStatistics {

	List<Utterance> getUtterancesTested();

	default double meanTokensPerTestedUtterance() {
		final int totalTokens = totalTokensTested();
		return totalTokens / (double) totalUtterancesTested();
	}

	int totalTokensTested();

	/**
	 *
	 * @return The number of {@link Utterance utterances} for the given
	 *         {@link EventDialogue dialogue} under test &mdash; including the
	 *         individual {@code Utterance} instances which were <em>not</em>
	 *         tested.
	 */
	int totalUtteranceCount();

	/**
	 *
	 * @return The number of {@link Utterance utterances} for the given
	 *         {@link EventDialogue dialogue} under test which were tested.
	 */
	int totalUtterancesTested();
}