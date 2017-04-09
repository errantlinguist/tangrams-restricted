/*
 *  This file is part of analysis.
 *
 *  tangrams is free software: you can redistribute it and/or modify
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
package se.kth.speech.coin.tangrams;

import java.io.IOException;
import java.nio.file.FileVisitOption;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.HashBasedTable;
import com.google.common.collect.Table;

import se.kth.speech.MutablePair;
import se.kth.speech.SpatialMatrix;
import se.kth.speech.SpatialRegion;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;
import se.kth.speech.coin.tangrams.iristk.GameStateChangeData;
import se.kth.speech.coin.tangrams.iristk.LoggedGameStateChangeDataParser;
import se.kth.speech.coin.tangrams.iristk.io.LoggingFormats;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Apr 5, 2017
 *
 */
public final class ModelFeatureExtractor {

	private enum SessionFileType {
		EVENTS("events-(.+?)\\.txt"), UTTERANCES("(.+?)\\_rec.xml");

		private final Pattern pattern;

		private SessionFileType(final String regex) {
			// Case-insensitive because case doesn't matter on Windows systems,
			// and the recording can (currently) only be done on Windows systems
			pattern = Pattern.compile(regex, Pattern.CASE_INSENSITIVE);
		}
	}

	private static final Logger LOGGER = LoggerFactory.getLogger(ModelFeatureExtractor.class);

	public static void main(final String[] args) throws IOException {
		if (args.length < 1) {
			final String usageMsg = String.format("Usage: %s INDIR", ModelFeatureExtractor.class.getName());
			throw new IllegalArgumentException(usageMsg);
		} else {
			final Path indir = Paths.get(args[0]);
			final int expectedPlayerCount = 2;
			final Table<String, SessionFileType, Path> playerDataFiles = HashBasedTable.create(expectedPlayerCount,
					SessionFileType.values().length);
			try (Stream<Path> filePaths = Files.walk(indir, FileVisitOption.FOLLOW_LINKS)) {
				filePaths.forEach(filePath -> {
					final Optional<MutablePair<SessionFileType, String>> optFileTypeAndPlayerId = findFileTypeAndPlayerId(
							filePath.getFileName().toString());
					optFileTypeAndPlayerId.ifPresent(fileTypeAndPlayerId -> {
						playerDataFiles.put(fileTypeAndPlayerId.getValue(), fileTypeAndPlayerId.getKey(), filePath);
					});
				});
			}
			final int playerCount = playerDataFiles.rowKeySet().size();
			if (playerCount == expectedPlayerCount) {
				final Map<String, Map<SessionFileType, Path>> rows = playerDataFiles.rowMap();
				for (final Entry<String, Map<SessionFileType, Path>> row : rows.entrySet()) {
					final String playerId = row.getKey();
					final Map<SessionFileType, Path> dataFiles = row.getValue();
					extractFeatures(playerId, dataFiles);
				}
			} else {
				throw new IllegalArgumentException(
						String.format("Expected to find data files for %d unique player(s) but found %d instead.",
								expectedPlayerCount, playerCount));
			}

		}
	}

	private static void extractFeatures(final String playerId, final Map<SessionFileType, Path> dataFiles)
			throws IOException {
		LOGGER.info("Extracting features for player \"{}\".", playerId);
		final Path eventLogFile = dataFiles.get(SessionFileType.EVENTS);
		try (final Stream<String> lines = Files.lines(eventLogFile, LoggingFormats.ENCODING)) {
			final Map<String, GameStateChangeData> gameStateChangeData = new LoggedGameStateChangeDataParser()
					.apply(lines);
			final int gameCount = gameStateChangeData.size();
			switch (gameCount) {
			case 1: {
				final Entry<String, GameStateChangeData> toFeatureize = gameStateChangeData.entrySet().iterator()
						.next();
				LOGGER.info("Extracting features for game \"{}\".", toFeatureize.getKey());
				final GameStateChangeData gameData = toFeatureize.getValue();
				System.out.println(gameData);
				break;
			}
			default: {
				throw new UnsupportedOperationException(
						String.format("No logic for handling a game count of %d.", gameCount));
			}
			}
		}
	}

	private static Optional<MutablePair<SessionFileType, String>> findFileTypeAndPlayerId(final String filename) {
		Optional<MutablePair<SessionFileType, String>> result = Optional.empty();

		for (final SessionFileType ft : SessionFileType.values()) {
			final Matcher m = ft.pattern.matcher(filename);
			if (m.matches()) {
				result = Optional.of(new MutablePair<>(ft, m.group(1)));
				break;
			}
		}

		return result;
	}

	public void extract(final ImageVisualizationInfo imgVizInfo, final SpatialMatrix<Integer> model) {
		final Map<Integer, SpatialRegion> piecePlacements = model.getElementPlacements().getElementMinimalRegions();
		for (final Entry<Integer, SpatialRegion> piecePlacement : piecePlacements.entrySet()) {
			final ImageVisualizationInfo.Datum imgVizInfoDatum = imgVizInfo.getData().get(piecePlacement.getKey());
		}
	}

	public void readLogfile() {

	}

}
