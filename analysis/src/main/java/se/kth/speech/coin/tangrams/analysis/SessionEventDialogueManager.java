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
import java.util.Collections;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.BiFunction;
import java.util.function.Supplier;
import java.util.stream.Stream;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.coin.tangrams.iristk.io.HatIO;
import se.kth.speech.coin.tangrams.iristk.io.LoggedEvents;
import se.kth.speech.hat.xsd.Annotation;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 12 May 2017
 *
 */
public final class SessionEventDialogueManager {

	private static class EventDialogueCreatingClosure implements Supplier<Stream<EventDialogue>> {

		private final BiFunction<ListIterator<Utterance>, GameHistory, Stream<EventDialogue>> eventDiagFactory;

		private final GameHistory gameHistory;

		private final SegmentUtteranceFactory segUttFactory;

		private final Annotation uttAnnots;

		private EventDialogueCreatingClosure(final Annotation uttAnnots, final GameHistory gameHistory,
				final SegmentUtteranceFactory segUttFactory,
				final BiFunction<ListIterator<Utterance>, GameHistory, Stream<EventDialogue>> eventDiagFactory) {
			this.uttAnnots = uttAnnots;
			this.gameHistory = gameHistory;
			this.segUttFactory = segUttFactory;
			this.eventDiagFactory = eventDiagFactory;
		}

		@Override
		public Stream<EventDialogue> get() {
			final List<Utterance> utts = Arrays
					.asList(segUttFactory.create(uttAnnots.getSegments().getSegment().stream()).flatMap(List::stream)
							.toArray(Utterance[]::new));
			LOGGER.debug("Creating dialogues for {} annotated utterance(s).", utts.size());
			return eventDiagFactory.apply(utts.listIterator(), gameHistory);
		}
	}

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
				throw new RuntimeException(e);
			}
		}

	};

	private static final Logger LOGGER = LoggerFactory.getLogger(SessionEventDialogueManager.class);

	private static Entry<String, GameHistory> loadGameHistory(final Path eventLogPath) throws IOException {
		Entry<String, GameHistory> result;
		LOGGER.info("Reading game histories from \"{}\".", eventLogPath);
		final Map<String, GameHistory> gameHistories = LoggedEvents.readGameHistories(eventLogPath,
				LoggedEvents.VALID_MODEL_MIN_REQUIRED_EVENT_MATCHER);
		final int gameCount = gameHistories.size();
		switch (gameCount) {
		case 0: {
			throw new IllegalArgumentException(String.format("Event log \"%s\" contains no games.", eventLogPath));
		}
		case 1: {
			result = gameHistories.entrySet().iterator().next();
			LOGGER.debug("Parsed history for game \"{}\".", result.getKey());
			break;
		}
		default: {
			throw new IllegalArgumentException(String
					.format("Event log \"%s\" contains multiple games; Not (currently) supported.", eventLogPath));
		}
		}
		return result;
	}

	private final Entry<String, GameHistory> idGameHistory;

	private final List<EventDialogue> uttDialogues;

	SessionEventDialogueManager(final SessionDataManager sessionData,
			final BiFunction<ListIterator<Utterance>, GameHistory, Stream<EventDialogue>> eventDiagFactory)
			throws IOException, JAXBException {
		idGameHistory = loadGameHistory(sessionData.getCanonicalEventLogPath());

		{
			final Map<String, String> sourcePlayerIds = sessionData.getPlayerData().getPlayerSourceIds().inverse();
			final SegmentUtteranceFactory segUttFactory = new SegmentUtteranceFactory(seg -> {
				final String sourceId = seg.getSource();
				return sourcePlayerIds.get(sourceId);
			});
			final Path hatInfilePath = sessionData.getHATFilePath();
			LOGGER.info("Reading annotations from \"{}\".", hatInfilePath);
			final Annotation uttAnnots = (Annotation) HAT_UNMARSHALLER.get().unmarshal(hatInfilePath.toFile());
			uttDialogues = Collections.unmodifiableList(Arrays.asList(new EventDialogueCreatingClosure(uttAnnots,
					idGameHistory.getValue(), segUttFactory, eventDiagFactory).get().toArray(EventDialogue[]::new)));
		}
	}

	public GameHistory getGameHistory() {
		return idGameHistory.getValue();
	}

	/**
	 * @return the gameId
	 */
	public String getGameId() {
		return idGameHistory.getKey();
	}

	public List<EventDialogue> getUttDialogues() {
		return uttDialogues;
	}

}
