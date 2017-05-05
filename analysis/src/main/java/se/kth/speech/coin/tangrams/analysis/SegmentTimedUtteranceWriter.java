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
import java.time.LocalDateTime;
import java.util.List;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.xml.bind.JAXBException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import iristk.util.HAT;
import se.kth.speech.MutablePair;
import se.kth.speech.TimestampArithmetic;
import se.kth.speech.coin.tangrams.iristk.EventTimes;
import se.kth.speech.hat.xsd.Annotation;
import se.kth.speech.hat.xsd.Annotation.Segments.Segment;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 5 May 2017
 *
 */
public final class SegmentTimedUtteranceWriter {

	private static final Logger LOGGER = LoggerFactory.getLogger(SegmentTimedUtteranceWriter.class);

	private static final SegmentUtteranceFactory SEG_UTT_FACTORY = new SegmentUtteranceFactory();

	private static final Collector<CharSequence, ?, String> WORD_JOINER = Collectors.joining(" ");

	public static void main(final String[] args) throws JAXBException {
		if (args.length != 2) {
			throw new IllegalArgumentException(
					String.format("Usage: %s INPATH INITIAL_TIMESTAMP", SegmentTimedUtteranceWriter.class));
		} else {
			final File inpath = new File(args[0]);
			LOGGER.info("Reading annotations from \"{}\".", inpath);
			final Annotation uttAnnots = HAT.readAnnotation(inpath);
			final List<Segment> segments = uttAnnots.getSegments().getSegment();
			final Stream<Utterance> utts = SEG_UTT_FACTORY.create(segments.stream()).flatMap(List::stream);
			// .sorted(Comparator.comparing(Utterance::getStartTime)
			// .thenComparing(Comparator.comparing(Utterance::getEndTime)))

			final LocalDateTime initialTime = EventTimes.parseEventTime(args[1]);
			LOGGER.info("Initial timestamp is {}.", initialTime);
			final Stream<MutablePair<String, String>> uttReprTimestamps = utts.map(utt -> {
				final float startTime = utt.getStartTime();
				final String uttRepr = utt.getTokens().stream().collect(WORD_JOINER);
				LOGGER.debug("Start time for \"{}\" is {}.", uttRepr, startTime);
				final LocalDateTime uttTime = TimestampArithmetic.createOffsetTimestamp(initialTime, startTime);
				final String uttTimestamp = uttTime.format(EventTimes.FORMATTER);
				return new MutablePair<>(uttRepr, uttTimestamp);
			});
			uttReprTimestamps.map(uttRepr -> String.format("%s\t%s", uttRepr.getKey(), uttRepr.getValue()))
					.forEachOrdered(System.out::println);
		}
	}

}
