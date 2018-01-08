/*
 *  This file is part of Tangrams-restricted.
 *
 *  Tangrams-restricted is free software: you can redistribute it and/or modify
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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Stream;

import javax.xml.bind.JAXBException;

import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.coin.tangrams.TestDataResources;
import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.coin.tangrams.iristk.GameEvent;
import se.kth.speech.coin.tangrams.iristk.io.LoggedEventReader;
import se.kth.speech.higgins._2005.annotation.Annotation;
import se.kth.speech.higgins.io.HatIO;

/**
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since 17 May 2017
 *
 */
public final class EventDialogueFactoryTest {

	private static final Logger LOGGER = LoggerFactory.getLogger(EventDialogueFactoryTest.class);

	private static final SegmentUtteranceFactory SEG_UTT_FACTORY = new SegmentUtteranceFactory();

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.analysis.EventDialogueFactory#apply(java.util.ListIterator, se.kth.speech.coin.tangrams.analysis.GameHistory)}.
	 *
	 * @throws IOException
	 * @throws JAXBException
	 */
	@Test
	public void testApply() throws IOException, JAXBException {
		final String singleMoveSessionDataResLocStr = TestDataResources.SESSION_DATA_DIR
				+ "/karey-tangram_Jutta-ONEMOVE";

		// Game history
		final URL eventLogUrl = TestDataResources.class
				.getResource(singleMoveSessionDataResLocStr + "/events-karey.txt");
		LOGGER.info("Reading event history from \"{}\".", eventLogUrl);
		Map<String, GameHistory> gameHistories = Collections.emptyMap();
		try (BufferedReader eventLogReader = new BufferedReader(new InputStreamReader(eventLogUrl.openStream()))) {
			final Stream<String> eventLines = eventLogReader.lines();
			gameHistories = new LoggedEventReader(1, 20).parseGameHistories(eventLines, event -> true);
		}
		Assert.assertEquals(gameHistories.size(), 1);
		final GameHistory history = gameHistories.values().iterator().next();

		// Utts
		final URL hatInfileUrl = TestDataResources.class.getResource(singleMoveSessionDataResLocStr + "/utts.xml");
		LOGGER.info("Reading annotations from \"{}\".", hatInfileUrl);
		final Annotation uttAnnots = (Annotation) HatIO.fetchUnmarshaller().unmarshal(hatInfileUrl);
		final List<Utterance> utts = Arrays.asList(SEG_UTT_FACTORY.create(uttAnnots.getSegments().getSegment().stream())
				.flatMap(List::stream).toArray(Utterance[]::new));

		final EventDialogueFactory testInst = new EventDialogueFactory(event -> true);
		final List<EventDialogue> actualDiagList = Arrays
				.asList(testInst.apply(utts.listIterator(), history).toArray(EventDialogue[]::new));
		// NOTE: If there are utterances in the test data before the timestamp
		// of the first event, then this should be the history event count plus
		// one
		final List<GameEvent> events = Arrays.asList(history.getEventSequence().toArray(GameEvent[]::new));
		{
			final List<GameEvent> expectedEvents = events;
			// final List<HashableEvent> expectedEvents = events.subList(0,
			// events.size() - 1);
			Assert.assertEquals(expectedEvents.size(), actualDiagList.size());

			final Iterator<GameEvent> expectedEventIter = expectedEvents.iterator();
			final Iterator<EventDialogue> actualDiagIter = actualDiagList.iterator();
			while (expectedEventIter.hasNext()) {
				final GameEvent expectedEvent = expectedEventIter.next();
				Assert.assertTrue(actualDiagIter.hasNext());
				final EventDialogue actualDiag = actualDiagIter.next();
				final Optional<GameEvent> actualOptLastEvent = actualDiag.getFirstEvent();
				Assert.assertTrue(actualOptLastEvent.isPresent());
				Assert.assertEquals(expectedEvent, actualOptLastEvent.get());
			}
		}
	}

}
