/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.analysis.
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
package se.kth.speech.coin.tangrams.analysis;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.FileVisitOption;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.DoubleStream;
import java.util.stream.Stream;

import javax.xml.bind.JAXBException;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.HashBasedTable;
import com.google.common.collect.Maps;
import com.google.common.collect.Table;

import iristk.util.HAT;
import se.kth.speech.coin.tangrams.iristk.events.GameStateDescription;
import se.kth.speech.coin.tangrams.iristk.io.LoggingFormats;
import se.kth.speech.hat.xsd.Annotation;
import se.kth.speech.hat.xsd.Annotation.Segments.Segment;
import se.kth.speech.hat.xsd.Annotation.Tracks.Track;
import se.kth.speech.hat.xsd.Annotation.Tracks.Track.Sources;
import se.kth.speech.hat.xsd.Annotation.Tracks.Track.Sources.Source;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Apr 17, 2017
 * @see <a href="http://anthology.aclweb.org/W/W15/W15-0124.pdf">Casey
 *      Kennington, Livia Dia, & David Schlangen. &ldquo;A Discriminative Model
 *      for Perceptually-Grounded Incremental Reference Resolution.&rdquo; In
 *      <em>Proceedings of IWCS 2015</em><a>.
 *
 */
public final class WordsAsClassifiersTrainingDataFactory
		implements Function<Segment, Stream<Entry<Stream<String>, DoubleStream>>> {

	private enum Parameter implements Supplier<Option> {
		HELP("?") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("help").desc("Prints this message.").build();
			}
		},
		INPATH("i") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("inpath").desc("The path to the HAT file corpus to process.")
						.hasArg().argName("path").type(File.class).required().build();
			}
		},
		OUTPATH("o") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("outpath").desc("The path to write the training data to.")
						.hasArg().argName("path").type(File.class).build();
			}
		};

		protected final String optName;

		private Parameter(final String optName) {
			this.optName = optName;
		}

	}

	private static final int EXPECTED_UNIQUE_GAME_COUNT = 1;

	private static final Pattern LOGGED_EVENT_FILE_NAME_PATTERN = Pattern.compile("events-(.+?)\\.txt");

	private static final Logger LOGGER = LoggerFactory.getLogger(WordsAsClassifiersTrainingDataFactory.class);

	/**
	 * @see <a href=
	 *      "http://stackoverflow.com/a/4546093/1391325">StackOverflow</a>
	 */
	private static final Pattern MINIMAL_FILE_EXT_PATTERN = Pattern.compile("\\.(?=[^\\.]+$)");

	private static final Options OPTIONS = createOptions();

	private static final Function<Segment, List<Utterance>> SEG_UTT_FACTORY = new SegmentUtteranceFactory();

	private static final Collector<CharSequence, ?, String> TABLE_ROW_CELL_JOINER;

	private static final String TABLE_STRING_REPR_COL_DELIMITER;

	private static final String TABLE_STRING_REPR_ROW_DELIMITER = System.lineSeparator();

	static {
		TABLE_STRING_REPR_COL_DELIMITER = "\t";
		TABLE_ROW_CELL_JOINER = Collectors.joining(TABLE_STRING_REPR_COL_DELIMITER);
	}

	public static void main(final String[] args) throws IOException, JAXBException {
		final CommandLineParser parser = new DefaultParser();
		try {
			final CommandLine cl = parser.parse(OPTIONS, args);
			if (cl.hasOption(Parameter.HELP.optName)) {
				printHelp();
			} else {
				final File inpath = (File) cl.getParsedOptionValue(Parameter.INPATH.optName);
				LOGGER.info("Reading annotations from \"{}\".", inpath);
				final Annotation uttAnnots = HAT.readAnnotation(inpath);
				final Map<String, String> sourceIdPlayerIds = createSourceIdPlayerIdMap(uttAnnots);
				final Set<String> playerIds = new HashSet<>(sourceIdPlayerIds.values());
				final int expectedEventLogFileCount = playerIds.size();
				final Path sessionLogDir = inpath.getParentFile().toPath();
				LOGGER.info("Processing session log directory \"{}\".", sessionLogDir);
				final Map<String, Path> playerEventLogFilePaths = createPlayerEventLogFileMap(sessionLogDir,
						expectedEventLogFileCount);
				final Table<String, String, GameHistory> playerGameStateChangeData = createPlayerGameStateChangeData(
						playerEventLogFilePaths.entrySet());
				final Set<String> playerGameIdIntersection = new HashSet<>(playerGameStateChangeData.columnKeySet());
				playerGameStateChangeData.rowMap().values().stream().map(Map::keySet)
						.forEach(playerGameIdIntersection::retainAll);
				final int gameCount = playerGameIdIntersection.size();
				if (gameCount == 1) {
					final Iterator<GameStateDescription> gameDescs = playerGameStateChangeData.values().stream()
							.map(GameHistory::getInitialState).iterator();
					final GameStateDescription firstGameDesc = gameDescs.next();
					while (gameDescs.hasNext()) {
						// Sanity check to make sure that all players have
						// started with the same game setup
						final GameStateDescription next = gameDescs.next();
						if (!firstGameDesc.isEquivalent(next)) {
							throw new IllegalArgumentException("Found non-equivalent initial states between players.");
						}
					}
					final String gameId = playerGameIdIntersection.iterator().next();
					final Map<String, GameHistory> playerStateChangeData = playerGameStateChangeData.columnMap()
							.get(gameId);

					final int uniqueModelDescriptionCount = playerGameStateChangeData.values().size();
					final List<GameContextFeatureExtractor> contextFeatureExtractors = Arrays
							.asList(new EntityFeatureExtractor(uniqueModelDescriptionCount));
					final Stream.Builder<String> featureDescBuilder = Stream.builder();
					featureDescBuilder.accept("WORD");
					contextFeatureExtractors.stream()
							.map(extractor -> extractor.createFeatureDescriptions(firstGameDesc))
							.flatMap(Function.identity()).forEachOrdered(featureDescBuilder);

					final Stream<String> featureDescs = featureDescBuilder.build();
					final String header = featureDescs.collect(TABLE_ROW_CELL_JOINER);

					final WordsAsClassifiersTrainingDataFactory trainingDataFactory = new WordsAsClassifiersTrainingDataFactory(
							sourceIdPlayerIds, playerStateChangeData, contextFeatureExtractors);
					final List<Segment> segments = uttAnnots.getSegments().getSegment();
					final Stream<Stream<Entry<Stream<String>, DoubleStream>>> segTrainingData = segments.stream()
							.map(trainingDataFactory);
					final Stream<Entry<Stream<String>, DoubleStream>> trainingData = segTrainingData
							.flatMap(Function.identity());
					// TODO: finish
					try (final PrintWriter out = parseOutpath(cl)) {
						out.print(header);
						trainingData.forEachOrdered(trainingDatum -> {
							final double[] featureVector = trainingDatum.getValue().toArray();
							final Stream<String> rows = trainingDatum.getKey().map(word -> {
								final Stream.Builder<String> rowBuilder = Stream.builder();
								rowBuilder.accept(word);
								Arrays.stream(featureVector).mapToObj(Double::toString).forEachOrdered(rowBuilder);
								return rowBuilder.build();
							}).map(stream -> stream.collect(TABLE_ROW_CELL_JOINER));
							rows.forEachOrdered(row -> {
								out.print(TABLE_STRING_REPR_ROW_DELIMITER);
								out.println(row);
							});
						});
					}

				} else {
					throw new UnsupportedOperationException(
							String.format("No logic for handling a game count of %d.", gameCount));
				}
			}
		} catch (final ParseException e) {
			System.out.println(String.format("An error occured while parsing the command-line arguments: %s", e));
			printHelp();
		}
	}

	private static Options createOptions() {
		final Options result = new Options();
		Arrays.stream(Parameter.values()).map(Parameter::get).forEach(result::addOption);
		return result;
	}

	private static Map<String, Path> createPlayerEventLogFileMap(final Path sessionLogDir,
			final int minEventLogFileCount) throws IOException {
		final Map<String, Path> result = Maps.newHashMapWithExpectedSize(minEventLogFileCount);
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

	private static Table<String, String, GameHistory> createPlayerGameStateChangeData(
			final Collection<Entry<String, Path>> playerEventLogFilePaths) throws IOException {
		final Table<String, String, GameHistory> result = HashBasedTable.create(playerEventLogFilePaths.size(),
				EXPECTED_UNIQUE_GAME_COUNT);
		for (final Entry<String, Path> playerEventLogFilePath : playerEventLogFilePaths) {
			final String playerId = playerEventLogFilePath.getKey();
			LOGGER.info("Reading session event log for player \"{}\".", playerId);
			final Path eventLogFile = playerEventLogFilePath.getValue();
			try (final Stream<String> lines = Files.lines(eventLogFile, LoggingFormats.ENCODING)) {
				final Map<String, GameHistory> gameHistories = new LoggedGameStateChangeDataParser().apply(lines);
				gameHistories.forEach((gameId, gameData) -> {
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

	private static PrintWriter parseOutpath(final CommandLine cl) throws ParseException, IOException {
		final PrintWriter result;
		final File outfile = (File) cl.getParsedOptionValue(Parameter.OUTPATH.optName);
		if (outfile == null) {
			LOGGER.info("No output file path specified; Writing to standard output.");
			result = new PrintWriter(System.out);
		} else {
			LOGGER.info("Output file path is \"{}\".", outfile);
			result = new PrintWriter(Files.newBufferedWriter(outfile.toPath(), StandardOpenOption.CREATE));
		}
		return result;
	}

	private static void printHelp() {
		final HelpFormatter formatter = new HelpFormatter();
		formatter.printHelp(FeatureVectorPrinter.class.getName(), OPTIONS);
	}

	private final List<GameContextFeatureExtractor> contextFeatureExtractors;

	private final Map<String, GameHistory> playerStateChangeData;

	private final Map<String, String> sourceIdPlayerIds;

	/**
	 * @param contextFeatureExtractors
	 * @param playerStateChangeData
	 * @param sourceIdPlayerIds
	 *
	 */
	public WordsAsClassifiersTrainingDataFactory(final Map<String, String> sourceIdPlayerIds,
			final Map<String, GameHistory> playerStateChangeData,
			final List<GameContextFeatureExtractor> contextFeatureExtractors) {
		this.sourceIdPlayerIds = sourceIdPlayerIds;
		this.playerStateChangeData = playerStateChangeData;
		this.contextFeatureExtractors = contextFeatureExtractors;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Function#apply(java.lang.Object)
	 */
	@Override
	public Stream<Entry<Stream<String>, DoubleStream>> apply(final Segment t) {
		// TODO Auto-generated method stub
		return null;
	}

}
