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
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.function.ToIntFunction;

import javax.xml.bind.JAXBException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.BiMap;

import iristk.util.HAT;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeatureExtractionContextFactory;
import se.kth.speech.coin.tangrams.iristk.io.LoggedEvents;
import se.kth.speech.hat.xsd.Annotation;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 12 May 2017
 *
 */
public final class UtteranceEntityContextManager {

	private static final Logger LOGGER = LoggerFactory.getLogger(UtteranceEntityContextManager.class);

	private final EntityFeatureExtractionContextFactory extractionContextFactory;

	private final Map<String, GameHistory> gameHistories;

	private final BiMap<String, String> playerSourceIds;

	private final Map<Utterance, String> uttPlayerIds;

	private final List<Utterance> utts;

	public UtteranceEntityContextManager(final SessionDataManager sessionData, final SegmentUtteranceFactory segUttFactory,
			final ToIntFunction<? super String> imgEdgeCounter) throws JAXBException, IOException {
		final Path hatInfilePath = sessionData.getHATFilePath();
		LOGGER.info("Reading annotations from \"{}\".", hatInfilePath);
		final Annotation uttAnnots = HAT.readAnnotation(hatInfilePath.toFile());

		final Path eventLogPath = sessionData.getCanonicalEventLogPath();
		LOGGER.info("Reading game histories from \"{}\".", eventLogPath);
		gameHistories = LoggedEvents.parseGameHistories(Files.lines(eventLogPath),
				LoggedEvents.VALID_MODEL_MIN_REQUIRED_EVENT_MATCHER);
		final int uniqueModelDescriptionCount = gameHistories.values().size();

		extractionContextFactory = new EntityFeatureExtractionContextFactory(
				new GameContextModelFactory(uniqueModelDescriptionCount), imgEdgeCounter);

		playerSourceIds = sessionData.getPlayerData().getPlayerSourceIds();
		final Function<String, String> sourcePlayerIdGetter = playerSourceIds.inverse()::get;
		uttPlayerIds = new UtterancePlayerIdMapFactory(segUttFactory::create, sourcePlayerIdGetter)
				.apply(uttAnnots.getSegments().getSegment());
		utts = Arrays.asList(uttPlayerIds.keySet().stream().sorted().toArray(Utterance[]::new));
	}

	/**
	 * @return the extractionContextFactory
	 */
	public EntityFeatureExtractionContextFactory getExtractionContextFactory() {
		return extractionContextFactory;
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
		return playerSourceIds;
	}

	/**
	 * @return the uttPlayerIds
	 */
	public Map<Utterance, String> getUttPlayerIds() {
		return Collections.unmodifiableMap(uttPlayerIds);
	}

	/**
	 * @return the utts
	 */
	public List<Utterance> getUtts() {
		return Collections.unmodifiableList(utts);
	}

}
