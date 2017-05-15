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

import iristk.util.HAT;
import se.kth.speech.coin.tangrams.iristk.io.LoggedEvents;
import se.kth.speech.hat.xsd.Annotation;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 12 May 2017
 *
 */
public final class UtteranceEntityContextManager {

	private static final Logger LOGGER = LoggerFactory.getLogger(UtteranceEntityContextManager.class);

	private final BiFunction<ListIterator<Utterance>, GameHistory, Stream<EventDialogue>> eventUttFactory;

//	private final EntityFeatureExtractionContextFactory extractionContextFactory;

	private final Map<String, GameHistory> gameHistories;

	private final BiMap<String, String> playerSourceIds;

	public int getUniqueGameModelDescriptionCount(){
		return gameHistories.values().size();
	}

	private final List<Utterance> utts;

	public UtteranceEntityContextManager(
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
//		final int uniqueModelDescriptionCount = gameHistories.values().size();

//		extractionContextFactory = new EntityFeatureExtractionContextFactory(
//				new GameContextModelFactory(uniqueModelDescriptionCount), imgEdgeCounter);

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
//		final List<UtteranceDialogue> result = new ArrayList<>(eventUttLists.size());
//		for (final Entry<Event, List<Utterance>> eventUttList : eventUttLists) {
//			final Event event = eventUttList.getKey();
//			final List<Utterance> uttList = eventUttList.getValue();
//			final Map<Utterance, GameContext> uttCtxs = uttList.stream()
//					.collect(Collectors.toMap(Function.identity(), utt -> {
//						final List<GameContext> uttContexts = TemporalGameContexts
//								.create(history, utt.getStartTime(), utt.getEndTime(), perspectivePlayerId)
//								.collect(Collectors.toList());
//						if (uttContexts.size() > 1) {
//							LOGGER.warn("More than one context found for {}; Ignoring all but the first.", utt);
//						}
//						return uttContexts.get(0);
//					}));
//			final UtteranceDialogue uttDiag = new UtteranceDialogue(uttList, uttCtxs::get);
//			final Event lastEventBeforeFirstUtt = uttDiag.findLastEventBeforeFirstUtt().orElse(null);
//			if (!Objects.equals(event, lastEventBeforeFirstUtt)){
//				LOGGER.warn("Do something here");
//			}
//		}
//		return result;
	}

//	/**
//	 * @return the extractionContextFactory
//	 */
//	public EntityFeatureExtractionContextFactory getExtractionContextFactory() {
//		return extractionContextFactory;
//	}

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
		return playerSourceIds;
	}

}
