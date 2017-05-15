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
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.xml.bind.JAXBException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.BiMap;
import com.google.common.collect.Maps;

import iristk.util.HAT;
import se.kth.speech.coin.tangrams.iristk.io.LoggedEvents;
import se.kth.speech.hat.xsd.Annotation;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 12 May 2017
 *
 */
public final class SessionEventDialogueManager {

	private static final Logger LOGGER = LoggerFactory.getLogger(SessionEventDialogueManager.class);

	private final BiFunction<ListIterator<Utterance>, GameHistory, Stream<EventDialogue>> eventUttFactory;

	private final Map<String, GameHistory> gameHistories;

	private final BiMap<String, String> playerSourceIds;

	public int getUniqueGameModelDescriptionCount(){
		return gameHistories.values().size();
	}

	private final List<Utterance> utts;

	public SessionEventDialogueManager(
			final SessionDataManager sessionData,
			final BiFunction<ListIterator<Utterance>, GameHistory, Stream<EventDialogue>> eventUttFactory) throws JAXBException, IOException {
		this.eventUttFactory = eventUttFactory;
		final Path hatInfilePath = sessionData.getHATFilePath();
		LOGGER.info("Reading annotations from \"{}\".", hatInfilePath);
		final Annotation uttAnnots = HAT.readAnnotation(hatInfilePath.toFile());

		final Path eventLogPath = sessionData.getCanonicalEventLogPath();
		LOGGER.info("Reading game histories from \"{}\".", eventLogPath);
		gameHistories = LoggedEvents.parseGameHistories(Files.lines(eventLogPath),
				LoggedEvents.VALID_MODEL_MIN_REQUIRED_EVENT_MATCHER);

		playerSourceIds = sessionData.getPlayerData().getPlayerSourceIds();
		final Map<String, String> sourcePlayerIds = sessionData.getPlayerData().getPlayerSourceIds().inverse();
		final SegmentUtteranceFactory segUttFactory = new SegmentUtteranceFactory(seg -> {
			final String sourceId = seg.getSource();
			return sourcePlayerIds.get(sourceId);
		});
		utts = segUttFactory.create(uttAnnots.getSegments().getSegment().stream()).flatMap(List::stream).collect(Collectors.toList());
	}

	public List<EventDialogue> createUttDialogues(final GameHistory history, final String perspectivePlayerId) {
		final List<EventDialogue> eventUttLists = eventUttFactory.apply(utts.listIterator(), history)
				.collect(Collectors.toList());
		return eventUttLists;
	}

	/**
	 * @return the gameHistories
	 */
	public Map<String, GameHistory> getGameHistories() {
		return Collections.unmodifiableMap(gameHistories);
	}

	/**
	 * @return the playerSourceIds
	 */
	public BiMap<String, String> getPlayerSourceIds() {
		return Maps.unmodifiableBiMap(playerSourceIds);
	}

}
