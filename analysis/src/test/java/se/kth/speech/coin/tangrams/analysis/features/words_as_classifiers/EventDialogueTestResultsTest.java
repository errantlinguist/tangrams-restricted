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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers;

import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.Collections;
import java.util.stream.IntStream;

import org.junit.Assert;
import org.junit.experimental.theories.DataPoints;
import org.junit.experimental.theories.Theories;
import org.junit.experimental.theories.Theory;
import org.junit.runner.RunWith;

import it.unimi.dsi.fastutil.objects.Object2DoubleMaps;
import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.coin.tangrams.iristk.GameEvent;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since May 31, 2017
 *
 */
@RunWith(Theories.class)
public final class EventDialogueTestResultsTest {

	@DataPoints
	public static final int[] TESTED_RANKS;
	
	private static final int[] EXPECTED_ENTITY_ID_RANKING;

	private static final double[] REF_CONF_VALS;

	static {
		REF_CONF_VALS = new double[] {0.04352434, 0.5343, 0.211234, 0.23245};
		EXPECTED_ENTITY_ID_RANKING = new int[] {1, 3, 2, 0};
		TESTED_RANKS = IntStream.rangeClosed(1, EXPECTED_ENTITY_ID_RANKING.length).toArray();
	}

	private static EventDialogueTestResults createMockDiagTestResult(final int rank) {
		final int goldStandardReferentId = EXPECTED_ENTITY_ID_RANKING[rank - 1];
		final Utterance testUtt = new Utterance("segment1", "testSpeaker", Arrays.asList("test", "utterance"), 2.3f,
				3.3f);
		final EventDialogue transformedDiag = new EventDialogue(
				Arrays.asList(new GameEvent("", "", "", "", LocalDateTime.now(), Collections.emptyMap())),
				Arrays.asList(testUtt));
		final int totalDiagUttCount = 1;
		return new EventDialogueTestResults(
				new ReferentConfidenceData(REF_CONF_VALS, Object2DoubleMaps.emptyMap(), "__OUT_OF_VOCABULARY__"),
				goldStandardReferentId, transformedDiag, totalDiagUttCount);
	}

	@Theory
	public void testRank(final int testedRank) {
		final EventDialogueTestResults testInst = createMockDiagTestResult(testedRank);
		Assert.assertEquals(testedRank, testInst.rank(), 0.00001);
	}

}
