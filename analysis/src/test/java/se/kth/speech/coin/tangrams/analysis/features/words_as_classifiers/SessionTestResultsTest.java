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

import org.apache.commons.lang3.tuple.Pair;
import org.junit.Assert;
import org.junit.experimental.theories.DataPoint;
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
 * @since Jun 1, 2017
 *
 */
@RunWith(Theories.class)
public final class SessionTestResultsTest {
	
	@DataPoints
	@DataPoint
	public static final int[] TESTED_RANKS;

	private static final GameEvent DUMMY_EVENT = new GameEvent("", "", "", "", LocalDateTime.now(),
			Collections.emptyMap());

	private static final int[] EXPECTED_ENTITY_ID_RANKING;

	private static final double[] REF_CONF_VALS;

	static {
		REF_CONF_VALS = new double[]{0.04352434, 0.5343,  0.211234,0.23245};
		EXPECTED_ENTITY_ID_RANKING = new int[] {1, 3, 2, 0};
		TESTED_RANKS = IntStream.rangeClosed(1, EXPECTED_ENTITY_ID_RANKING.length).toArray();
	}

	private static EventDialogueTestResults createMockDiagTestResult(final int rank) {
		final int goldStandardReferentId = EXPECTED_ENTITY_ID_RANKING[rank - 1];
		final Utterance testUtt = new Utterance("segment1", "testSpeaker", Arrays.asList("test", "utterance"), 2.3f,
				3.3f);
		final EventDialogue transformedDiag = new EventDialogue(Arrays.asList(DUMMY_EVENT), Arrays.asList(testUtt));
		final int totalDiagUttCount = 1;
		return new EventDialogueTestResults(
				new ReferentConfidenceData(REF_CONF_VALS, Object2DoubleMaps.emptyMap(), "__OUT_OF_VOCABULARY__"),
				goldStandardReferentId, transformedDiag, totalDiagUttCount);
	}

	private static SessionTestResults createTestInst(final int[] ranks) {
		final SessionTestResults result = new SessionTestResults(ranks.length);
		for (final int rank : ranks) {
			final EventDialogueTestResults diagTestResult = createMockDiagTestResult(rank);
			result.add(Pair.of(diagTestResult.getTransformedDiag(), diagTestResult));
		}
		return result;
	}

	@Theory
	public void testMeanRank(final int firstRank, final int secondRank, final int thirdRank) {
		testMeanRank(new int[] { firstRank, secondRank, thirdRank });
	}

	@Theory
	public void testMeanRank(final int[] ranks) {
		final SessionTestResults testInst = createTestInst(ranks);
		final double expected = Arrays.stream(ranks).average().getAsDouble();
		Assert.assertEquals(expected, testInst.meanRank(), 0.00001);
	}

}
