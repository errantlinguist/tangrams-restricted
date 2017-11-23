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

import java.net.URL;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.regex.Pattern;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.xml.bind.JAXBException;

import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.experimental.theories.DataPoints;
import org.junit.experimental.theories.Theories;
import org.junit.experimental.theories.Theory;
import org.junit.runner.RunWith;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.coin.tangrams.TestDataResources;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.higgins._2005.annotation.Annotation;
import se.kth.speech.higgins._2005.annotation.Annotation.Segments;
import se.kth.speech.higgins._2005.annotation.Annotation.Segments.Segment;
import se.kth.speech.higgins._2005.annotation.Transcription.T;
import se.kth.speech.higgins.io.HatIO;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 8 May 2017
 *
 */
@RunWith(Theories.class)
public final class SegmentUtteranceFactoryTest {

	private static Annotation annot;

	private static final double DOUBLE_DELTA = 0.01;

	private static final Logger LOGGER = LoggerFactory.getLogger(SegmentUtteranceFactoryTest.class);

	private static final Collector<CharSequence, ?, String> SEG_CONTENT_JOINER = Collectors.joining(" ");

	private static final SegmentUtteranceFactory TEST_INST = new SegmentUtteranceFactory();

	private static final Collector<CharSequence, ?, String> TOKEN_JOINING_COLLECTOR = Collectors.joining(" ");

	private static final Pattern WHITESPACE_PATTERN = Pattern.compile("\\s+");

	@BeforeClass
	public static void loadAnnotation() throws JAXBException {
		annot = readAnnotation();
	}

	@DataPoints("segments")
	public static List<Segment> segments() {
		return annot.getSegments().getSegment();
	}

	@DataPoints("segmentSets")
	public static Collection<Segments> segmentSets() {
		return Collections.singleton(annot.getSegments());
	}

	private static Annotation readAnnotation() throws JAXBException {
		final String resLoc = TestDataResources.createResourceLocator("test-hat.xml");
		LOGGER.debug("Reading test annotations from resource locator \"{}\".", resLoc);
		final URL testAnnotFileUrl = TestDataResources.class.getResource(resLoc);
		LOGGER.info("Reading test annotations from URL \"{}\".", testAnnotFileUrl);
		return (Annotation) HatIO.fetchUnmarshaller().unmarshal(testAnnotFileUrl);
	}

	private static boolean shouldHaveUtts(final Segment seg) {
		final Stream<String> segTokenContents = seg.getTranscription().getSegmentOrT().stream().map(T.class::cast)
				.map(T::getContent).map(String::trim);
		final Stream<String> tokenizedSegTokenContents = segTokenContents.flatMap(WHITESPACE_PATTERN::splitAsStream);
		return tokenizedSegTokenContents.findAny().isPresent();
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.analysis.SegmentUtteranceFactory#create(se.kth.speech.higgins._2005.annotation.Annotation.Segments.Segment)}.
	 */
	@Theory
	public void testCreateSegment(final Segment seg) {
		LOGGER.debug("Testing segment \"{}\".", SegmentUtteranceFactory.createSegmentTokenList(seg).stream()
				.map(T::getContent).collect(TOKEN_JOINING_COLLECTOR));
		final List<Utterance> utts = TEST_INST.create(seg);

		final boolean shouldHaveUtts = shouldHaveUtts(seg);
		final boolean hasUtts = !utts.isEmpty();
		Assert.assertEquals(shouldHaveUtts, hasUtts);
		if (hasUtts) {
			final Float actualStart = seg.getStart();
			final Float actualEnd = seg.getEnd();
			final double minStart = utts.stream().mapToDouble(Utterance::getStartTime).min().getAsDouble();
			Assert.assertEquals(actualStart.doubleValue(), minStart, DOUBLE_DELTA);
			final double maxEnd = utts.stream().mapToDouble(Utterance::getEndTime).max().getAsDouble();
			Assert.assertEquals(actualEnd.doubleValue(), maxEnd, DOUBLE_DELTA);
			for (final Utterance utt : utts) {
				Assert.assertTrue(actualStart <= utt.getStartTime());
				Assert.assertTrue(actualEnd >= utt.getEndTime());
			}
		} else {
			final String segContent = seg.getTranscription().getSegmentOrT().stream().map(T.class::cast)
					.map(T::getContent).collect(SEG_CONTENT_JOINER);
			LOGGER.debug("Segment content \"{}\" was converted into an empty utterance list.", segContent);
		}
	}

}
