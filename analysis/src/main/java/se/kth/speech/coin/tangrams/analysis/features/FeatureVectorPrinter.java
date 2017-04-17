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
package se.kth.speech.coin.tangrams.analysis.features;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Supplier;
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

import com.google.common.collect.Table;

import iristk.util.HAT;
import se.kth.speech.coin.tangrams.analysis.GameHistory;
import se.kth.speech.coin.tangrams.iristk.events.GameStateDescription;
import se.kth.speech.coin.tangrams.iristk.io.LoggedEvents;
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
public final class FeatureVectorPrinter {

	private enum Parameter implements Supplier<Option> {
		HELP("?") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("help").desc("Prints this message.").build();
			}
		},
		INFILE("i") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("infile").desc("The path to the HAT file to process.").hasArg()
						.argName("path").type(File.class).required().build();
			}
		},
		OUTFILE("o") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("outfile").desc("The path of the feature file to write.")
						.hasArg().argName("path").type(File.class).build();
			}
		};

		protected final String optName;

		private Parameter(final String optName) {
			this.optName = optName;
		}

	}

	private static final int EXPECTED_UNIQUE_GAME_COUNT = 1;

	private static final Logger LOGGER = LoggerFactory.getLogger(FeatureVectorPrinter.class);

	/**
	 * @see <a href=
	 *      "http://stackoverflow.com/a/4546093/1391325">StackOverflow</a>
	 */
	private static final Pattern MINIMAL_FILE_EXT_PATTERN = Pattern.compile("\\.(?=[^\\.]+$)");

	private static final Options OPTIONS = createOptions();

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
				final File infile = (File) cl.getParsedOptionValue(Parameter.INFILE.optName);
				LOGGER.info("Parsing utterances from \"{}\".", infile);
				final Annotation uttAnnots = HAT.readAnnotation(infile);
				final Map<String, String> sourceIdPlayerIds = createSourceIdPlayerIdMap(uttAnnots);
				final Set<String> playerIds = new HashSet<>(sourceIdPlayerIds.values());
				final int expectedEventLogFileCount = playerIds.size();
				final Path sessionLogDir = infile.getParentFile().toPath();
				LOGGER.info("Processing session log directory \"{}\".", sessionLogDir);
				final Map<String, Path> playerEventLogFilePaths = LoggedEvents
						.createPlayerEventLogFileMap(sessionLogDir, expectedEventLogFileCount);
				final Table<String, String, GameHistory> playerGameStateChangeData = LoggedEvents
						.createPlayerGameHistories(playerEventLogFilePaths.entrySet(),
								EXPECTED_UNIQUE_GAME_COUNT);
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
					final List<GameContextFeatureExtractor> contextFeatureExtractors = Arrays.asList(
							new EnvironmentFeatureExtractor(uniqueModelDescriptionCount),
							new EntityFeatureExtractor(uniqueModelDescriptionCount), new GameEventFeatureExtractor());
					final Stream.Builder<String> featureDescBuilder = Stream.builder();
					contextFeatureExtractors.stream()
							.map(extractor -> extractor.createFeatureDescriptions(firstGameDesc))
							.flatMap(Function.identity()).forEachOrdered(featureDescBuilder);

					final List<UtteranceFeatureExtractor> uttFeatureExtrators = Arrays
							.asList(new StanfordNLPFeatureExtractor());
					uttFeatureExtrators.stream().map(UtteranceFeatureExtractor::createFeatureDescriptions)
							.flatMap(Function.identity()).forEachOrdered(featureDescBuilder);
					final Stream<String> featureDescs = featureDescBuilder.build();
					final String header = featureDescs.collect(TABLE_ROW_CELL_JOINER);

					final SegmentFeatureVectorFactory featureVectorFactory = new SegmentFeatureVectorFactory(
							sourceIdPlayerIds, playerStateChangeData, contextFeatureExtractors, uttFeatureExtrators);
					final List<Segment> segments = uttAnnots.getSegments().getSegment();
					final Stream<Stream<DoubleStream>> segmentFeatureVectors = segments.stream()
							.map(featureVectorFactory);
					final Stream<DoubleStream> featureVectors = segmentFeatureVectors.flatMap(Function.identity());
					try (final PrintWriter out = parseOutfile(cl)) {
						out.print(header);
						featureVectors.forEachOrdered(featureVector -> {
							out.print(TABLE_STRING_REPR_ROW_DELIMITER);
							final Stream<String> cellVals = featureVector.mapToObj(Double::toString);
							final String row = cellVals.collect(TABLE_ROW_CELL_JOINER);
							out.print(row);
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

	private static Map<String, String> createSourceIdPlayerIdMap(final Annotation uttAnnots) {
		final List<Track> tracks = uttAnnots.getTracks().getTrack();
		final Stream<Source> sources = tracks.stream().map(Track::getSources).map(Sources::getSource)
				.flatMap(List::stream);
		return sources.collect(Collectors.toMap(Source::getId, source -> {
			final String href = source.getHref();
			return MINIMAL_FILE_EXT_PATTERN.split(href)[0];
		}));
	}

	private static PrintWriter parseOutfile(final CommandLine cl) throws ParseException, IOException {
		final PrintWriter result;
		final File outfile = (File) cl.getParsedOptionValue(Parameter.OUTFILE.optName);
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

}
