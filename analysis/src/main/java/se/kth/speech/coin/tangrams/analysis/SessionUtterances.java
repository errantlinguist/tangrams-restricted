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

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.coin.tangrams.analysis.io.PlayerDataManager;
import se.kth.speech.coin.tangrams.analysis.io.SessionDataManager;
import se.kth.speech.coin.tangrams.iristk.io.HatIO;
import se.kth.speech.hat.xsd.Annotation;
import se.kth.speech.hat.xsd.Annotation.Segments.Segment;
import se.kth.speech.io.RuntimeJAXBException;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 9 Aug 2017
 *
 */
final class SessionUtterances {

	private static final ThreadLocal<Unmarshaller> HAT_UNMARSHALLER = new ThreadLocal<Unmarshaller>() {

		/*
		 * (non-Javadoc)
		 *
		 * @see java.lang.ThreadLocal#initialValue()
		 */
		@Override
		protected Unmarshaller initialValue() {
			try {
				return HatIO.fetchContext().createUnmarshaller();
			} catch (final JAXBException e) {
				throw new RuntimeJAXBException(e);
			}
		}

	};

	private static final Logger LOGGER = LoggerFactory.getLogger(SessionUtterances.class);

	static List<Utterance> createUtteranceList(final SessionDataManager sessionData)
			throws JAXBException, IOException {
		final PlayerDataManager playerData = sessionData.getPlayerData();
		final Map<String, String> sourcePlayerIds = playerData.getPlayerSourceIds().inverse();
		final SegmentUtteranceFactory segUttFactory = new SegmentUtteranceFactory(seg -> {
			final String sourceId = seg.getSource();
			return sourcePlayerIds.get(sourceId);
		});
		final Path hatInfilePath = sessionData.getHATFilePath();
		LOGGER.debug("Reading HAT annotations at \"{}\".", hatInfilePath);
		final Annotation uttAnnots = readAnnotations(hatInfilePath);
		final List<Segment> segs = uttAnnots.getSegments().getSegment();
		return Arrays.asList(segUttFactory.create(segs.stream()).flatMap(List::stream).toArray(Utterance[]::new));
	}

	private static Annotation readAnnotations(final Path hatInfilePath) throws JAXBException, IOException {
		try (InputStream instream = Files.newInputStream(hatInfilePath)) {
			return (Annotation) HAT_UNMARSHALLER.get().unmarshal(instream);
		}
	}

	private SessionUtterances() {
	}

}
