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

import it.unimi.dsi.fastutil.ints.IntSet;
import se.kth.speech.coin.tangrams.analysis.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.Utterance;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 19 May 2017
 *
 */
public interface SessionTestStatistics extends EventDialogueTestStatistics {

	double meanRank();

	double meanReciprocalRank();

	/**
	 *
	 * @return The mean number of {@link Utterance utterances} for all
	 *         {@link EventDialogue dialogues} which were tested for the given
	 *         session under test.
	 */
	default double meanUtterancesTestedPerDialogue() {
		final int totalUtts = totalUtterancesTested();
		return totalUtts / (double) totalDialoguesTested();
	}

	/**
	 *
	 * @return The mean number of {@link Utterance utterances} for all
	 *         {@link EventDialogue dialogues} which were tested for the given
	 *         session under test &mdash; including the individual
	 *         {@code Utterance} instances which were <em>not</em> tested.
	 */
	default double meanUtteranceTotalPerDialogue() {
		final int totalUtts = totalUtteranceCount();
		return totalUtts / (double) totalDialoguesTested();
	}

	/**
	 *
	 * @return The referent ID(s) which is/are observed most frequently.
	 */
	IntSet modeReferentIds();

	int totalDialoguesTested();

	int uniqueGoldStandardReferentIdCount();

}