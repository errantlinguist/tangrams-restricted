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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags;

import java.io.IOException;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.function.ToDoubleFunction;
import java.util.stream.Collectors;

import javax.xml.bind.JAXBException;

import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import iristk.system.Event;
import se.kth.speech.coin.tangrams.analysis.Utterance;
import se.kth.speech.coin.tangrams.iristk.EventTimes;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;
import se.kth.speech.nlp.PatternMatchingUtteranceSentimentRanker;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Jul 13, 2017
 *
 */
public final class SentimentAnalyzingEventDialogueUtteranceSorterTest {

	private static final Logger LOGGER = LoggerFactory
			.getLogger(SentimentAnalyzingEventDialogueUtteranceSorterTest.class);

	private static SentimentAnalyzingEventDialogueUtteranceSorter createTestInst() {
		final ToDoubleFunction<Utterance> uttSentimentRanker = new PatternMatchingUtteranceSentimentRanker();
		return new SentimentAnalyzingEventDialogueUtteranceSorter(uttSentimentRanker);
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.SentimentAnalyzingEventDialogueUtteranceSorter#apply(java.util.List, java.util.function.Predicate)}.
	 *
	 * @throws JAXBException
	 * @throws IOException
	 */
	@Test
	public void testApply() throws JAXBException, IOException {
		final String instructorPlayerId = "jack";
		final String selectorPlayerId = "jill";
		final Event event = new Event();
		event.setTime(EventTimes.FORMATTER.format(LocalDateTime.now()));
		event.put(GameManagementEvent.Attribute.PLAYER_ID.toString(), instructorPlayerId);
		final List<Utterance> utts = Arrays.asList(
				new Utterance("1", instructorPlayerId, Arrays.asList("it's", "the", "red", "one"), 0.02f, 1.0f),
				new Utterance("2", selectorPlayerId, Arrays.asList("the", "big", "one", "right"), 1.2f, 2.0f),
				new Utterance("3", instructorPlayerId, Arrays.asList("yeah", "right"), 2.2f, 3.0f));

		final SentimentAnalyzingEventDialogueUtteranceSorter testInst = createTestInst();
		final List<UtteranceRelation> result = testInst.apply(utts, event);
		LOGGER.debug("{}", result);
		final long nonZeroSentimentRankCount = result.stream().mapToDouble(UtteranceRelation::getSentimentValue)
				.filter(val -> val != 0.0).count();
		Assert.assertEquals(nonZeroSentimentRankCount, 1);
		final Set<String> sentUttSpeakerIds = result.stream().map(UtteranceRelation::getSentimentUtt)
				.map(Utterance::getSpeakerId).collect(Collectors.toSet());
		Assert.assertEquals(Collections.singleton(instructorPlayerId), sentUttSpeakerIds);
		final Set<String> otherUttSpeakerIds = result.stream().map(UtteranceRelation::getPrevUtts).flatMap(List::stream)
				.map(Utterance::getSpeakerId).collect(Collectors.toSet());
		Assert.assertEquals(Collections.singleton(selectorPlayerId), otherUttSpeakerIds);
	}

}
