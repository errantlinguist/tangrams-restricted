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
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collector;
import java.util.stream.Collectors;

import javax.xml.bind.JAXBException;

import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.experimental.theories.DataPoints;
import org.junit.experimental.theories.Theories;
import org.junit.experimental.theories.Theory;
import org.junit.runner.RunWith;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import iristk.util.HAT;
import se.kth.speech.hat.xsd.Annotation;
import se.kth.speech.hat.xsd.Annotation.Segments;
import se.kth.speech.hat.xsd.Annotation.Segments.Segment;
import se.kth.speech.hat.xsd.Transcription.T;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 8 May 2017
 *
 */
@RunWith(Theories.class)
public final class SegmentUtteranceFactoryTest {

	private static Annotation annots;

	private static final double DOUBLE_DELTA = 0.01;

	private static final Logger LOGGER = LoggerFactory.getLogger(SegmentUtteranceFactoryTest.class);

	private static final SegmentUtteranceFactory TEST_INST = new SegmentUtteranceFactory(Segment::getSource);

	private static final Collector<CharSequence, ?, String> TOKEN_JOINING_COLLECTOR = Collectors.joining(" ");

	@BeforeClass
	public static void loadAnnotations() throws URISyntaxException, JAXBException {
		annots = readAnnotations();
	}

	@DataPoints("segments")
	public static List<Segment> segments() {
		return annots.getSegments().getSegment();
	}

	@DataPoints("segmentSets")
	public static Collection<Segments> segmentSets() {
		return Collections.singleton(annots.getSegments());
	}

	private static Annotation readAnnotations() throws URISyntaxException, JAXBException {
		final URL testAnnotFileUrl = SegmentUtteranceFactoryTest.class.getResource(TestDataResources.ROOT_DIR + "/test-hat.xml");
		LOGGER.info("Reading test annotations from \"{}\".", testAnnotFileUrl);
		final File testAnnotFile = new File(testAnnotFileUrl.toURI());
		return HAT.readAnnotation(testAnnotFile);
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.analysis.SegmentUtteranceFactory#create(se.kth.speech.hat.xsd.Annotation.Segments.Segment)}.
	 */
	@Theory
	public void testCreateSegment(final Segment seg) {
		LOGGER.info("Testing segment \"{}\".", SegmentUtteranceFactory.createSegmentTokenList(seg).stream()
				.map(T::getContent).collect(TOKEN_JOINING_COLLECTOR));
		final List<Utterance> utts = TEST_INST.create(seg);
		Assert.assertFalse(utts.isEmpty());
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
	}

}
