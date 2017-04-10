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

import java.awt.Color;
import java.io.IOException;
import java.nio.file.FileVisitOption;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.xml.bind.JAXBException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.HashBasedTable;
import com.google.common.collect.Maps;
import com.google.common.collect.Table;

import iristk.util.HAT;
import it.unimi.dsi.fastutil.objects.Object2DoubleMap;
import it.unimi.dsi.fastutil.objects.Object2DoubleOpenHashMap;
import se.kth.speech.IntArrays;
import se.kth.speech.SpatialMap;
import se.kth.speech.SpatialMatrix;
import se.kth.speech.SpatialRegion;
import se.kth.speech.coin.tangrams.content.IconImages;
import se.kth.speech.coin.tangrams.iristk.GameStateChangeData;
import se.kth.speech.coin.tangrams.iristk.ImageVisualizationInfoDescription;
import se.kth.speech.coin.tangrams.iristk.LoggedGameStateChangeDataParser;
import se.kth.speech.coin.tangrams.iristk.events.GameStateDescription;
import se.kth.speech.coin.tangrams.iristk.events.GameStateUnmarshalling;
import se.kth.speech.coin.tangrams.iristk.events.ModelDescription;
import se.kth.speech.coin.tangrams.iristk.io.LoggingFormats;
import se.kth.speech.hat.xsd.Annotation;
import se.kth.speech.hat.xsd.Annotation.Segments.Segment;
import se.kth.speech.hat.xsd.Annotation.Tracks.Track;
import se.kth.speech.hat.xsd.Annotation.Tracks.Track.Sources;
import se.kth.speech.hat.xsd.Annotation.Tracks.Track.Sources.Source;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Apr 5, 2017
 *
 */
public final class ModelFeatureExtractor {

	private static class FeatureVectorFactory implements Function<Segment, Void> {

		private enum EntityFeature {
			COLOR, POSITION_X, POSITION_Y, SELECTED, SHAPE, SIZE;

			private static final List<EntityFeature> ORDERING;

			static {
				ORDERING = Arrays.asList(SHAPE, COLOR, SIZE, POSITION_X, POSITION_Y, SELECTED);
				assert ORDERING.size() == EntityFeature.values().length;
			}
		}

		private enum EnvironmentFeature {
			COL_COUNT, ENTITY_COUNT, ROW_COUNT;

			private static final List<EnvironmentFeature> ORDERING;

			static {
				ORDERING = Arrays.asList(ROW_COUNT, COL_COUNT, ENTITY_COUNT);
				assert ORDERING.size() == EnvironmentFeature.values().length;
			}
		}

		private static final Object2DoubleMap<String> SHAPE_FEATURE_VALS = createShapeFeatureValueMap();

		private static float createColorFeatureVal(final ImageVisualizationInfoDescription.Datum pieceImgVizInfoDatum) {
			final Color color = pieceImgVizInfoDatum.getColor();
			final float[] hsbVals = Color.RGBtoHSB(color.getRed(), color.getGreen(), color.getBlue(), null);
			return hsbVals[0];
		}

		private static Object2DoubleMap<String> createShapeFeatureValueMap() {
			final Set<String> possibleShapeStrValues = IconImages.getImageResources().keySet();
			final Object2DoubleMap<String> result = new Object2DoubleOpenHashMap<>();
			possibleShapeStrValues.forEach(strValue -> {
				final double featureVal = result.size();
				result.put(strValue, featureVal);
			});
			return result;
		}

		private static double[] createStaticFeatures(final GameStateDescription initialStateDesc) {
			final ModelDescription modelDesc = initialStateDesc.getModelDescription();
			final SpatialMatrix<Integer> model = GameStateUnmarshalling.createModel(modelDesc);
			final int[] modelDims = model.getDimensions();
			final double modelArea = IntArrays.product(modelDims);

			final SpatialMap<Integer> piecePlacements = model.getElementPlacements();
			final ImageVisualizationInfoDescription imgVizInfoDataDesc = initialStateDesc
					.getImageVisualizationInfoDescription();
			final List<ImageVisualizationInfoDescription.Datum> imgVizInfoData = imgVizInfoDataDesc.getData();
			final int pieceCount = imgVizInfoData.size();
			final double[] result = new double[EnvironmentFeature.values().length
					+ pieceCount * EntityFeature.values().length];
			int currentFeatureIdx = setEnvironmentFeatureVals(result, 0, modelDims, pieceCount);

			for (final ListIterator<ImageVisualizationInfoDescription.Datum> imgVizInfoDataIter = imgVizInfoData
					.listIterator(); imgVizInfoDataIter.hasNext();) {
				final int pieceId = imgVizInfoDataIter.nextIndex();
				final ImageVisualizationInfoDescription.Datum pieceImgVizInfoDatum = imgVizInfoDataIter.next();
				final SpatialRegion pieceRegion = piecePlacements.getElementMinimalRegions().get(pieceId);
				currentFeatureIdx = setStaticEntityFeatureVals(result, currentFeatureIdx, pieceImgVizInfoDatum,
						pieceRegion, modelDims, modelArea);
			}
			return result;
		}

		private static double getShapeFeatureVal(final ImageVisualizationInfoDescription.Datum pieceImgVizInfoDatum) {
			final String strVal = pieceImgVizInfoDatum.getResourceName();
			return SHAPE_FEATURE_VALS.getDouble(strVal);
		}

		private static int setEnvironmentFeatureVals(final double[] vals, int currentFeatureIdx, final int[] modelDims,
				final int pieceCount) {
			for (final EnvironmentFeature feature : EnvironmentFeature.ORDERING) {
				switch (feature) {
				case COL_COUNT:
					vals[currentFeatureIdx] = modelDims[1];
					break;
				case ENTITY_COUNT:
					vals[currentFeatureIdx] = pieceCount;
					break;
				case ROW_COUNT:
					vals[currentFeatureIdx] = modelDims[0];
					break;
				default: {
					throw new AssertionError("Logic error");
				}
				}
				currentFeatureIdx++;
			}
			return currentFeatureIdx;
		}

		private static int setStaticEntityFeatureVals(final double[] vals, int currentFeatureIdx,
				final ImageVisualizationInfoDescription.Datum pieceImgVizInfoDatum, final SpatialRegion pieceRegion,
				final int[] modelDims, final double modelArea) {
			for (final EntityFeature feature : EntityFeature.ORDERING) {
				switch (feature) {
				case COLOR:
					final float colorFeatureVal = createColorFeatureVal(pieceImgVizInfoDatum);
					vals[currentFeatureIdx] = colorFeatureVal;
					break;
				case POSITION_X: {
					final double centerX = pieceRegion.getXLowerBound() + pieceRegion.getLengthX() / 2.0;
					final double posX = centerX / modelDims[0];
					vals[currentFeatureIdx] = posX;
					break;
				}
				case POSITION_Y: {
					final double centerY = pieceRegion.getYLowerBound() + pieceRegion.getLengthY() / 2.0;
					final double posY = centerY / modelDims[1];
					vals[currentFeatureIdx] = posY;
					break;
				}
				case SHAPE:
					final double shapeFeatureVal = getShapeFeatureVal(pieceImgVizInfoDatum);
					vals[currentFeatureIdx] = shapeFeatureVal;
					break;
				case SIZE:
					final int pieceArea = IntArrays.product(pieceRegion.getDimensions());
					final double sizeFeatureVal = pieceArea / modelArea;
					vals[currentFeatureIdx] = sizeFeatureVal;
					break;
				case SELECTED: {
					// Set "not selected" as the initial value
					vals[currentFeatureIdx] = 0.0;
					break;
				}
				default: {
					throw new AssertionError("Logic error");
				}
				}
				currentFeatureIdx++;
			}
			return currentFeatureIdx;
		}

		private final Map<? super String, GameStateChangeData> playerStateChangeData;

		private final Map<? super String, String> sourceIdPlayerIds;

		private FeatureVectorFactory(final Map<? super String, String> sourceIdPlayerIds,
				final Map<? super String, GameStateChangeData> playerStateChangeData) {
			this.sourceIdPlayerIds = sourceIdPlayerIds;
			this.playerStateChangeData = playerStateChangeData;
		}

		@Override
		public Void apply(final Segment segment) {
			final String sourceId = segment.getSource();
			// Get the player ID associated with the given audio source
			final String playerId = sourceIdPlayerIds.get(sourceId);
			final GameStateChangeData gameStateChangeData = playerStateChangeData.get(playerId);
			// TODO: Finish
			return null;
		}
	}

	private static final int EXPECTED_UNIQUE_GAME_COUNT = 1;

	private static final Pattern LOGGED_EVENT_FILE_NAME_PATTERN = Pattern.compile("events-(.+?)\\.txt");

	private static final Logger LOGGER = LoggerFactory.getLogger(ModelFeatureExtractor.class);

	/**
	 * @see <a href=
	 *      "http://stackoverflow.com/a/4546093/1391325">StackOverflow</a>
	 */
	private static final Pattern MINIMAL_FILE_EXT_PATTERN = Pattern.compile("\\.(?=[^\\.]+$)");

	public static void main(final String[] args) throws IOException, JAXBException {
		if (args.length < 1) {
			final String usageMsg = String.format("Usage: %s UTTERANCES_FILE", ModelFeatureExtractor.class.getName());
			throw new IllegalArgumentException(usageMsg);
		} else {
			final Path uttFilePath = Paths.get(args[0]);
			LOGGER.info("Parsing utterances from \"{}\".", uttFilePath);
			final Annotation uttAnnots = HAT.readAnnotation(uttFilePath.toFile());
			final Map<String, String> sourceIdPlayerIds = createSourceIdPlayerIdMap(uttAnnots);
			final Set<String> playerIds = new HashSet<>(sourceIdPlayerIds.values());
			final int expectedEventLogFileCount = playerIds.size();
			final Map<String, Path> playerEventLogFilePaths = createPlayerEventLogFileMap(uttFilePath,
					expectedEventLogFileCount);
			final Table<String, String, GameStateChangeData> playerGameStateChangeData = createPlayerGameStateChangeData(
					playerEventLogFilePaths.entrySet());
			final Set<String> playerGameIdIntersection = new HashSet<>(playerGameStateChangeData.columnKeySet());
			playerGameStateChangeData.rowMap().values().stream().map(Map::keySet)
					.forEach(playerGameIdIntersection::retainAll);
			final int gameCount = playerGameIdIntersection.size();
			if (gameCount == 1) {
				final String gameId = playerGameIdIntersection.iterator().next();
				final Map<String, GameStateChangeData> playerStateChangeData = playerGameStateChangeData.columnMap()
						.get(gameId);
				final FeatureVectorFactory featureVectorFactory = new FeatureVectorFactory(sourceIdPlayerIds,
						playerStateChangeData);
				final List<Segment> segments = uttAnnots.getSegments().getSegment();
				segments.stream().map(featureVectorFactory);
				// TODO: Finish
			} else {
				throw new UnsupportedOperationException(
						String.format("No logic for handling a game count of %d.", gameCount));
			}
		}
	}

	private static Map<String, Path> createPlayerEventLogFileMap(final Path sessionLogDir,
			final int minEventLogFileCount) throws IOException {
		final Map<String, Path> result = Maps.newHashMapWithExpectedSize(minEventLogFileCount);
		LOGGER.info("Processing session log directory \"{}\".", sessionLogDir);
		try (Stream<Path> filePaths = Files.walk(sessionLogDir, FileVisitOption.FOLLOW_LINKS)) {
			filePaths.forEach(filePath -> {
				final Matcher m = LOGGED_EVENT_FILE_NAME_PATTERN.matcher(filePath.getFileName().toString());
				if (m.matches()) {
					final String playerId = m.group(1);
					result.put(playerId, filePath);
				}
			});
		}
		final int playerEventLogFileCount = result.size();
		if (playerEventLogFileCount < minEventLogFileCount) {
			throw new IllegalArgumentException(
					String.format("Expected to find data files for at least %d unique player(s) but found %d instead.",
							minEventLogFileCount, playerEventLogFileCount));
		}
		return result;
	}

	private static Table<String, String, GameStateChangeData> createPlayerGameStateChangeData(
			final Collection<Entry<String, Path>> playerEventLogFilePaths) throws IOException {
		final Table<String, String, GameStateChangeData> result = HashBasedTable.create(playerEventLogFilePaths.size(),
				EXPECTED_UNIQUE_GAME_COUNT);
		for (final Entry<String, Path> playerEventLogFilePath : playerEventLogFilePaths) {
			final String playerId = playerEventLogFilePath.getKey();
			LOGGER.info("Extracting features for player \"{}\".", playerId);
			final Path eventLogFile = playerEventLogFilePath.getValue();
			try (final Stream<String> lines = Files.lines(eventLogFile, LoggingFormats.ENCODING)) {
				final Map<String, GameStateChangeData> gameStateChangeData = new LoggedGameStateChangeDataParser()
						.apply(lines);
				gameStateChangeData.forEach((gameId, gameData) -> {
					result.put(playerId, gameId, gameData);
				});
			}
		}
		return result;
	}

	private static Map<String, String> createSourceIdPlayerIdMap(final Annotation uttAnnots) {
		final List<Track> tracks = uttAnnots.getTracks().getTrack();
		final Stream<Source> sources = tracks.stream().map(Track::getSources).map(Sources::getSource)
				.flatMap(List::stream);
		return sources.collect(Collectors.toMap(Source::getId, source -> {
			final String href = source.getHref();
			return MINIMAL_FILE_EXT_PATTERN.split(href)[0];
		}));
	}

	public void readLogfile() {

	}

}
