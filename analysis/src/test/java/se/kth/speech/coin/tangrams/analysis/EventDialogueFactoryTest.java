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

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import javax.xml.bind.JAXBException;

import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import iristk.system.Event;
import iristk.util.HAT;
import se.kth.speech.coin.tangrams.iristk.io.LoggedEvents;
import se.kth.speech.hat.xsd.Annotation;
import se.kth.speech.hat.xsd.Annotation.Segments.Segment;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 17 May 2017
 *
 */
public final class EventDialogueFactoryTest {

	private static final Logger LOGGER = LoggerFactory.getLogger(EventDialogueFactoryTest.class);

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.analysis.EventDialogueFactory#apply(java.util.ListIterator, se.kth.speech.coin.tangrams.analysis.GameHistory)}.
	 *
	 * @throws URISyntaxException
	 * @throws IOException
	 * @throws JAXBException
	 */
	@Test
	public final void testApply() throws URISyntaxException, IOException, JAXBException {
		testApply(event -> true);
	}

	private final void testApply(final Predicate<? super Event> eventFilter)
			throws URISyntaxException, IOException, JAXBException {
		final String singleMoveSessionDataResLocStr = TestDataResources.SESSION_DATA_DIR
				+ "/karey-tangram_Jutta-ONEMOVE";
		final URL eventLogUrl = EventDialogueFactoryTest.class
				.getResource(singleMoveSessionDataResLocStr + "/events-karey.txt");
		final Path eventLogPath = Paths.get(eventLogUrl.toURI());
		LOGGER.info("Reading event history from \"{}\".", eventLogPath);
		final Map<String, GameHistory> gameHistories = LoggedEvents.parseGameHistories(Files.lines(eventLogPath),
				eventFilter);
		Assert.assertEquals(gameHistories.size(), 1);
		final GameHistory history = gameHistories.values().iterator().next();
		// TODO: Make a LoggedEventsTest class and put this assertion in that
		// class
		final List<Event> historyEvents = history.getEventSequence().collect(Collectors.toList());
		Assert.assertTrue(historyEvents.stream().allMatch(eventFilter));

		final URL hatInfileUrl = EventDialogueFactoryTest.class
				.getResource(singleMoveSessionDataResLocStr + "/utts.xml");
		LOGGER.info("Reading annotations from \"{}\".", hatInfileUrl);
		final Annotation uttAnnots = HAT.readAnnotation(new File(hatInfileUrl.toURI()));

		final SegmentUtteranceFactory segUttFactory = new SegmentUtteranceFactory(Segment::getSource);
		final List<Utterance> utts = segUttFactory.create(uttAnnots.getSegments().getSegment().stream())
				.flatMap(List::stream).collect(Collectors.toList());

		final EventDialogueFactory testInst = new EventDialogueFactory(eventFilter);
		final List<EventDialogue> actualDiagList = testInst.apply(utts.listIterator(), history)
				.collect(Collectors.toList());
		// NOTE: If there are utterances in the test data before the timestamp
		// of the first event, then this should be the history event count plus
		// one
		final int expectedDiagCount = Math.toIntExact(history.getEventSequence().count());
		Assert.assertEquals(expectedDiagCount, actualDiagList.size());

		final Iterator<Event> expectedEventIter = historyEvents.iterator();
		final Iterator<EventDialogue> actualDiagIter = actualDiagList.iterator();
		while (expectedEventIter.hasNext()) {
			final Event expectedEvent = expectedEventIter.next();
			Assert.assertTrue(actualDiagIter.hasNext());
			final EventDialogue actualDiag = actualDiagIter.next();
			final Optional<Event> actualOptLastEvent = actualDiag.getLastEvent();
			Assert.assertTrue(actualOptLastEvent.isPresent());
			Assert.assertEquals(expectedEvent, actualOptLastEvent.get());
		}
	}

}
