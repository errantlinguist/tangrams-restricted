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
import java.util.Map.Entry;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.function.ToDoubleFunction;
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
import se.kth.speech.MutablePair;
import se.kth.speech.coin.tangrams.analysis.Annotations;
import se.kth.speech.coin.tangrams.analysis.GameContext;
import se.kth.speech.coin.tangrams.analysis.GameContextModelFactory;
import se.kth.speech.coin.tangrams.analysis.GameHistory;
import se.kth.speech.coin.tangrams.analysis.SegmentUtteranceFactory;
import se.kth.speech.coin.tangrams.analysis.Utterance;
import se.kth.speech.coin.tangrams.analysis.UtteranceGameContextFactory;
import se.kth.speech.coin.tangrams.iristk.events.GameStateDescription;
import se.kth.speech.coin.tangrams.iristk.io.LoggedEvents;
import se.kth.speech.hat.xsd.Annotation;
import se.kth.speech.hat.xsd.Annotation.Segments.Segment;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Apr 17, 2017
 * @see <a href="http://anthology.aclweb.org/W/W15/W15-0124.pdf">Casey
 *      Kennington, Livia Dia, &amp; David Schlangen. &ldquo;A Discriminative
 *      Model for Perceptually-Grounded Incremental Reference Resolution.&rdquo;
 *      In <em>Proceedings of IWCS 2015</em><a>.
 *
 */
public final class WordsAsClassifiersTrainingDataFactory
		implements Function<Segment, Stream<Entry<List<String>, DoubleStream>>> {

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

	private static final Logger LOGGER = LoggerFactory.getLogger(WordsAsClassifiersTrainingDataFactory.class);

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
				final Map<String, String> sourceIdPlayerIds = Annotations.createSourceIdPlayerIdMap(uttAnnots);
				final Set<String> playerIds = new HashSet<>(sourceIdPlayerIds.values());
				final int expectedEventLogFileCount = playerIds.size();
				final Path sessionLogDir = inpath.getParentFile().toPath();
				LOGGER.info("Processing session log directory \"{}\".", sessionLogDir);
				final Map<String, Path> playerEventLogFilePaths = LoggedEvents
						.createPlayerEventLogFileMap(sessionLogDir, expectedEventLogFileCount);
				final Table<String, String, GameHistory> playerGameHistoryTable = LoggedEvents
						.createPlayerGameHistoryTable(playerEventLogFilePaths.entrySet(), EXPECTED_UNIQUE_GAME_COUNT);
				final Set<String> playerGameIdIntersection = new HashSet<>(playerGameHistoryTable.columnKeySet());
				playerGameHistoryTable.rowMap().values().stream().map(Map::keySet)
						.forEach(playerGameIdIntersection::retainAll);
				final int gameCount = playerGameIdIntersection.size();
				if (gameCount == 1) {
					final Iterator<GameStateDescription> gameDescs = playerGameHistoryTable.values().stream()
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

					final int uniqueModelDescriptionCount = playerGameHistoryTable.values().size();
					final ToDoubleFunction<String> namedResourceEdgeCounter = new ImageEdgeCounter();
					final List<GameContextFeatureExtractor> contextFeatureExtractors = Arrays.asList(
							new SelectedEntityFeatureExtractor(new GameContextModelFactory(uniqueModelDescriptionCount),
									namedResourceEdgeCounter));
					final Stream.Builder<String> featureDescBuilder = Stream.builder();
					featureDescBuilder.accept("WORD");
					contextFeatureExtractors.stream()
							.map(extractor -> extractor.createFeatureDescriptions(firstGameDesc))
							.flatMap(Function.identity()).forEachOrdered(featureDescBuilder);

					final Stream<String> featureDescs = featureDescBuilder.build();
					final String header = featureDescs.collect(TABLE_ROW_CELL_JOINER);

					final String gameId = playerGameIdIntersection.iterator().next();
					final Map<String, GameHistory> playerGameHistories = playerGameHistoryTable.columnMap().get(gameId);
					final UtteranceGameContextFactory uttContextFactory = new UtteranceGameContextFactory(
							playerGameHistories::get);
					final WordsAsClassifiersTrainingDataFactory trainingDataFactory = new WordsAsClassifiersTrainingDataFactory(
							sourceIdPlayerIds, uttContextFactory, contextFeatureExtractors);
					final List<Segment> segments = uttAnnots.getSegments().getSegment();
					final Stream<Stream<Entry<List<String>, DoubleStream>>> segTrainingData = segments.stream()
							.map(trainingDataFactory);
					final Stream<Entry<List<String>, DoubleStream>> trainingData = segTrainingData
							.flatMap(Function.identity());
					try (final PrintWriter out = parseOutpath(cl)) {
						out.print(header);
						final Stream<String> rows = trainingData.flatMap(trainingDatum -> {
							final double[] featureVector = trainingDatum.getValue().toArray();
							return trainingDatum.getKey().stream().map(word -> {
								final Stream.Builder<String> rowBuilder = Stream.builder();
								rowBuilder.accept(word);
								Arrays.stream(featureVector).mapToObj(Double::toString).forEachOrdered(rowBuilder);
								return rowBuilder.build();
							}).map(stream -> stream.collect(TABLE_ROW_CELL_JOINER));
						});
						rows.forEachOrdered(row -> {
							out.print(TABLE_STRING_REPR_ROW_DELIMITER);
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
		formatter.printHelp(WordsAsClassifiersTrainingDataFactory.class.getName(), OPTIONS);
	}

	private final List<GameContextFeatureExtractor> contextFeatureExtractors;

	private final Map<String, String> sourceIdPlayerIds;

	private final BiFunction<? super Utterance, ? super String, Stream<Entry<Utterance, GameContext>>> uttContextFactory;

	/**
	 * @param contextFeatureExtractors
	 * @param playerGameHistories
	 * @param sourceIdPlayerIds
	 *
	 */
	public WordsAsClassifiersTrainingDataFactory(final Map<String, String> sourceIdPlayerIds,
			final BiFunction<? super Utterance, ? super String, Stream<Entry<Utterance, GameContext>>> uttContextFactory,
			final List<GameContextFeatureExtractor> contextFeatureExtractors) {
		this.sourceIdPlayerIds = sourceIdPlayerIds;
		this.uttContextFactory = uttContextFactory;
		this.contextFeatureExtractors = contextFeatureExtractors;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Function#apply(java.lang.Object)
	 */
	@Override
	public Stream<Entry<List<String>, DoubleStream>> apply(final Segment segment) {
		final List<Utterance> utts = SEG_UTT_FACTORY.apply(segment);
		final String sourceId = segment.getSource();
		// Get the player ID associated with the given audio source
		final String playerId = sourceIdPlayerIds.get(sourceId);
		final Stream<Entry<Utterance, GameContext>> uttContexts = utts.stream()
				.flatMap(utt -> uttContextFactory.apply(utt, playerId));
		return uttContexts.map(uttContext -> {
			final DoubleStream.Builder featureVectorBuilder = DoubleStream.builder();
			contextFeatureExtractors
					.forEach(extractor -> extractor.accept(uttContext.getValue(), featureVectorBuilder));
			final DoubleStream vals = featureVectorBuilder.build();
			final List<String> uttTokens = uttContext.getKey().getTokens();
			return new MutablePair<>(uttTokens, vals);
		});
	}

}
