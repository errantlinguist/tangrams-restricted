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
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.Function;
import java.util.function.Predicate;

import javax.xml.bind.JAXBException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Maps;

import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.coin.tangrams.analysis.io.PlayerDataManager;
import se.kth.speech.coin.tangrams.analysis.io.SessionDataManager;
import se.kth.speech.coin.tangrams.iristk.io.HatIO;
import se.kth.speech.coin.tangrams.iristk.io.LoggedEventReader;
import se.kth.speech.hat.xsd.Annotation;
import se.kth.speech.hat.xsd.Annotation.Segments.Segment;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 12 May 2017
 *
 */
public final class SessionGameManager {

	public static final class Factory {

		private static final int ESTIMATED_UNIQUE_SESSION_COUNT = 50;

		private static final int ESTIMATED_UNIQUE_TOKEN_SEQ_COUNT = 6000;

		private final LoggedEventReader eventReader;

		private final Function<Function<Segment, String>, SegmentUtteranceFactory> segUttFactoryFactory;

		public Factory() {
			this(new LoggedEventReader(ESTIMATED_UNIQUE_SESSION_COUNT, ESTIMATED_UNIQUE_SESSION_COUNT * 10),
					ESTIMATED_UNIQUE_TOKEN_SEQ_COUNT);
		}

		public Factory(final LoggedEventReader eventReader, final int expectedUniqueTokenSeqCount) {
			this.eventReader = eventReader;
			final TokenListSingletonFactory tokenListFactory = new TokenListSingletonFactory(
					expectedUniqueTokenSeqCount);
			segUttFactoryFactory = uttSpeakerIdFactory -> new SegmentUtteranceFactory(uttSpeakerIdFactory,
					tokenListFactory);
		}

		public Factory(final LoggedEventReader eventReader, final Predicate<? super String> uttTokenFilter) {
			this(eventReader, uttTokenFilter, ESTIMATED_UNIQUE_TOKEN_SEQ_COUNT);
		}
		
		public Factory(final LoggedEventReader eventReader, final Predicate<? super String> uttTokenFilter,
				final int expectedUniqueTokenSeqCount) {
			this.eventReader = eventReader;
			final TokenListSingletonFactory tokenListFactory = new TokenListSingletonFactory(
					expectedUniqueTokenSeqCount);
			segUttFactoryFactory = uttSpeakerIdFactory -> new SegmentUtteranceFactory(uttSpeakerIdFactory,
					uttTokenFilter, tokenListFactory);
		}

		public SessionGameManager apply(final SessionDataManager sessionData) throws JAXBException, IOException {
			final List<Utterance> utts = createUtteranceList(sessionData);
			LOGGER.debug("Creating dialogues for {} annotated utterance(s).", utts.size());
			final SessionGame canonicalGame = SessionGame.create(sessionData.getCanonicalEventLogPath(), utts,
					eventReader);
			final Map<String, Path> playerEventLogs = sessionData.getPlayerData().getPlayerEventLogs();
			return new SessionGameManager(utts, canonicalGame, playerEventLogs);
		}

		private List<Utterance> createUtteranceList(final SessionDataManager sessionData)
				throws JAXBException, IOException {
			final PlayerDataManager playerData = sessionData.getPlayerData();
			final Map<String, String> sourcePlayerIds = playerData.getPlayerSourceIds().inverse();
			final Function<Segment, String> uttSpeakerIdFactory = seg -> {
				final String sourceId = seg.getSource();
				return sourcePlayerIds.get(sourceId);
			};
			final SegmentUtteranceFactory segUttFactory = segUttFactoryFactory.apply(uttSpeakerIdFactory);
			final Path hatInfilePath = sessionData.getHATFilePath();
			LOGGER.debug("Reading HAT annotations at \"{}\".", hatInfilePath);
			final Annotation uttAnnots = HatIO.readAnnotation(hatInfilePath);
			final List<Segment> segs = uttAnnots.getSegments().getSegment();
			return Arrays.asList(segUttFactory.create(segs.stream()).flatMap(List::stream).toArray(Utterance[]::new));
		}

		LoggedEventReader getEventReader() {
			return eventReader;
		}
	}

	private static final Logger LOGGER = LoggerFactory.getLogger(SessionGameManager.class);

	private static Map<String, SessionGame> createPlayerPerspectiveGameMap(
			final Collection<Entry<String, Path>> playerEventLogPaths, final List<Utterance> utts,
			final LoggedEventReader eventReader) throws IOException {
		final Map<String, SessionGame> result = Maps.newHashMapWithExpectedSize(playerEventLogPaths.size());
		for (final Entry<String, Path> playerEventLogPath : playerEventLogPaths) {
			final String playerId = playerEventLogPath.getKey();
			final Path eventLogPath = playerEventLogPath.getValue();
			final SessionGame sessionGame = SessionGame.create(eventLogPath, utts, eventReader);
			result.put(playerId, sessionGame);
		}
		return result;
	}

	private final SessionGame canonicalGame;

	private final Map<String, Path> playerEventLogs;

	private final List<Utterance> utts;

	private SessionGameManager(final List<Utterance> utts, final SessionGame canonicalGame,
			final Map<String, Path> playerEventLogs) throws IOException, JAXBException {
		this.utts = utts;
		this.canonicalGame = canonicalGame;
		this.playerEventLogs = playerEventLogs;
	}

	public Map<String, SessionGame> createPlayerPerspectiveGameMap(final LoggedEventReader eventReader)
			throws IOException {
		return createPlayerPerspectiveGameMap(playerEventLogs.entrySet(), utts, eventReader);

	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(final Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (!(obj instanceof SessionGameManager)) {
			return false;
		}
		final SessionGameManager other = (SessionGameManager) obj;
		if (canonicalGame == null) {
			if (other.canonicalGame != null) {
				return false;
			}
		} else if (!canonicalGame.equals(other.canonicalGame)) {
			return false;
		}
		if (playerEventLogs == null) {
			if (other.playerEventLogs != null) {
				return false;
			}
		} else if (!playerEventLogs.equals(other.playerEventLogs)) {
			return false;
		}
		if (utts == null) {
			if (other.utts != null) {
				return false;
			}
		} else if (!utts.equals(other.utts)) {
			return false;
		}
		return true;
	};

	public SessionGame getCanonicalGame() {
		return canonicalGame;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (canonicalGame == null ? 0 : canonicalGame.hashCode());
		result = prime * result + (playerEventLogs == null ? 0 : playerEventLogs.hashCode());
		result = prime * result + (utts == null ? 0 : utts.hashCode());
		return result;
	}

}
