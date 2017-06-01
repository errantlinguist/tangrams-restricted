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

import java.util.Arrays;

import org.junit.Assert;
import org.junit.Test;

import iristk.system.Event;
import it.unimi.dsi.fastutil.ints.Int2DoubleMap;
import it.unimi.dsi.fastutil.ints.Int2DoubleOpenHashMap;
import se.kth.speech.coin.tangrams.analysis.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.Utterance;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since May 31, 2017
 *
 */
public final class EventDialogueTestResultsTest {

	// {0.08052273635368928=>[5], 0.07303962011080395=>[7],
	// 0.06956941639116672=>[13], 0.06450970173096632=>[19],
	// 0.06225250000646863=>[3], 0.05668477140677224=>[2],
	// 0.053169717542337715=>[12], 0.05107776309599048=>[17],
	// 0.0492581391364514=>[0], 0.048225258646517645=>[9],
	// 0.04804251162810409=>[18], 0.04756822650462815=>[15],
	// 0.04590847090319988=>[14], 0.04159146983372071=>[8],
	// 0.040567229529046636=>[10], 0.039745500506388455=>[6],
	// 0.03960019278814138=>[4], 0.03928188187638165=>[11],
	// 0.035364437818475716=>[1], 0.026732497546490454=>[16]}

	@Test
	public void testRank1() {
		final Int2DoubleMap referentConfidenceVals = new Int2DoubleOpenHashMap();
		referentConfidenceVals.put(0, 0.23245);
		referentConfidenceVals.put(1, 0.5343);
		referentConfidenceVals.put(2, 0.211234);
		referentConfidenceVals.put(3, 0.04352434);
		final int goldStandardReferentId = 2;
		final Utterance testUtt = new Utterance("segment1", "testSpeaker", Arrays.asList("test", "utterance"), 2.3f,
				3.3f);
		final EventDialogue transformedDiag = new EventDialogue(Arrays.asList(new Event()), Arrays.asList(testUtt));
		final int totalDiagUttCount = 1;
		final EventDialogueTestResults testInst = new EventDialogueTestResults(referentConfidenceVals,
				goldStandardReferentId, transformedDiag, totalDiagUttCount);
		Assert.assertEquals(3, testInst.rank(), 0.00001);
	}

	@Test
	public void testRank2() {
		final Int2DoubleMap referentConfidenceVals = new Int2DoubleOpenHashMap();
		referentConfidenceVals.put(0, 0.23245);
		referentConfidenceVals.put(1, 0.5343);
		referentConfidenceVals.put(2, 0.211234);
		referentConfidenceVals.put(3, 0.04352434);
		final int goldStandardReferentId = 1;
		final Utterance testUtt = new Utterance("segment1", "testSpeaker", Arrays.asList("test", "utterance"), 2.3f,
				3.3f);
		final EventDialogue transformedDiag = new EventDialogue(Arrays.asList(new Event()), Arrays.asList(testUtt));
		final int totalDiagUttCount = 1;
		final EventDialogueTestResults testInst = new EventDialogueTestResults(referentConfidenceVals,
				goldStandardReferentId, transformedDiag, totalDiagUttCount);
		Assert.assertEquals(1, testInst.rank(), 0.00001);
	}

	@Test
	public void testReciprocalRank1() {
		final Int2DoubleMap referentConfidenceVals = new Int2DoubleOpenHashMap();
		referentConfidenceVals.put(0, 0.23245);
		referentConfidenceVals.put(1, 0.5343);
		referentConfidenceVals.put(2, 0.211234);
		referentConfidenceVals.put(3, 0.04352434);
		final int goldStandardReferentId = 2;
		final Utterance testUtt = new Utterance("segment1", "testSpeaker", Arrays.asList("test", "utterance"), 2.3f,
				3.3f);
		final EventDialogue transformedDiag = new EventDialogue(Arrays.asList(new Event()), Arrays.asList(testUtt));
		final int totalDiagUttCount = 1;
		final EventDialogueTestResults testInst = new EventDialogueTestResults(referentConfidenceVals,
				goldStandardReferentId, transformedDiag, totalDiagUttCount);
		Assert.assertEquals(0.33333, testInst.reciprocalRank(), 0.00001);
	}

	@Test
	public void testReciprocalRank2() {
		final Int2DoubleMap referentConfidenceVals = new Int2DoubleOpenHashMap();
		referentConfidenceVals.put(0, 0.23245);
		referentConfidenceVals.put(1, 0.5343);
		referentConfidenceVals.put(2, 0.211234);
		referentConfidenceVals.put(3, 0.04352434);
		final int goldStandardReferentId = 1;
		final Utterance testUtt = new Utterance("segment1", "testSpeaker", Arrays.asList("test", "utterance"), 2.3f,
				3.3f);
		final EventDialogue transformedDiag = new EventDialogue(Arrays.asList(new Event()), Arrays.asList(testUtt));
		final int totalDiagUttCount = 1;
		final EventDialogueTestResults testInst = new EventDialogueTestResults(referentConfidenceVals,
				goldStandardReferentId, transformedDiag, totalDiagUttCount);
		Assert.assertEquals(1, testInst.reciprocalRank(), 0.00001);
	}

}
