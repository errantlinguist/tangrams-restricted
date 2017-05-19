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
package se.kth.speech.coin.tangrams.analysis;

import java.util.Arrays;
import java.util.stream.Stream;

import org.junit.Assert;
import org.junit.Test;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 19 May 2017
 *
 */
public final class UtteranceDialogueRepresentationStringFactoryTest {

	private static final UtteranceDialogueRepresentationStringFactory TEST_INST = new UtteranceDialogueRepresentationStringFactory();

	@Test
	public void testApply() {
		final Utterance utt1 = new Utterance("segment3", "Tobias", Arrays.asList("come", "out", "of", "there", "again"),
				2.0914, 2.8518);
		final Utterance utt2 = new Utterance("segment4", "nacho",
				Arrays.asList("yeah", "I'm", "connecting", "to", "game", "thirteen"), 3.9165, 4.9432);
		final Utterance utt3 = new Utterance("segment5", "nacho", Arrays.asList("okay"), 5.9318, 8.4415);

		final String actual = TEST_INST.apply(Stream.of(utt1, utt2, utt3).iterator());
		Assert.assertTrue(actual.indexOf(utt1.getSpeakerId()) < actual.indexOf(utt2.getSpeakerId()));
	}

}
