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

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.StringWriter;
import java.io.UncheckedIOException;
import java.io.Writer;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.DoubleStream;
import java.util.stream.Stream;

import javax.swing.JFileChooser;
import javax.swing.filechooser.FileNameExtensionFilter;
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

import iristk.system.Event;
import iristk.util.HAT;
import se.kth.speech.FilenameBaseSplitter;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeature;
import se.kth.speech.coin.tangrams.analysis.features.ImageEdgeCounter;
import se.kth.speech.coin.tangrams.analysis.features.SelectedEntityFeatureExtractor;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfoTableRowWriter;
import se.kth.speech.coin.tangrams.iristk.EventTypeMatcher;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;
import se.kth.speech.coin.tangrams.iristk.ImageVisualizationInfoUnmarshaller;
import se.kth.speech.coin.tangrams.iristk.events.GameStateDescription;
import se.kth.speech.coin.tangrams.iristk.events.Move;
import se.kth.speech.coin.tangrams.iristk.io.LoggedEvents;
import se.kth.speech.coin.tangrams.view.UserPrompts;
import se.kth.speech.hat.xsd.Annotation;
import se.kth.speech.hat.xsd.Annotation.Segments.Segment;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 4 May 2017
 *
 */
public final class UtteranceSelectedEntityDescriptionWriter {

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
		OUTFILE_PREFIX("p") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("outfile-prefix").desc("A prefix to add to the output files.")
						.hasArg().argName("prefix").build();
			}
		},
		OUTPATH("o") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("outpath").desc("The path to write the training data to.")
						.hasArg().argName("path").type(File.class).required().build();
			}
		};

		protected final String optName;

		private Parameter(final String optName) {
			this.optName = optName;
		}

	}

	private static final String DEFAULT_OUTFILE_PREFIX = "uttImgDescs_";

	private static final int EXPECTED_UNIQUE_GAME_COUNT = 1;

	private static final List<EntityFeature> FEATURES_TO_DESCRIBE = Arrays.asList(EntityFeature.POSITION_X,
			EntityFeature.POSITION_Y, EntityFeature.EDGE_COUNT);

	private static final Logger LOGGER = LoggerFactory.getLogger(UtteranceSelectedEntityDescriptionWriter.class);

	private static final EventTypeMatcher NEW_SALIENT_PIECE_EVENT_MATCHER = new EventTypeMatcher(
			EnumSet.of(GameManagementEvent.NEXT_TURN_REQUEST));

	private static final Options OPTIONS = createOptions();

	private static final EventTypeMatcher REQUIRED_EVENT_MATCHER = new EventTypeMatcher(
			EnumSet.of(GameManagementEvent.NEXT_TURN_REQUEST, GameManagementEvent.GAME_READY_RESPONSE));

	private static final Function<Segment, List<Utterance>> SEG_UTT_FACTORY = new SegmentUtteranceFactory();

	private static final Collector<CharSequence, ?, String> SENTENCE_JOINER = Collectors.joining(". ");

	private static final Collector<CharSequence, ?, String> TABLE_ROW_CELL_JOINER;

	private static final Collector<CharSequence, ?, String> TABLE_ROW_JOINER;

	private static final String TABLE_STRING_REPR_COL_DELIMITER;

	private static final String TABLE_STRING_REPR_ROW_DELIMITER;

	private static final Collector<CharSequence, ?, String> WORD_JOINER = Collectors.joining(" ");

	static {
		TABLE_STRING_REPR_ROW_DELIMITER = System.lineSeparator();
		TABLE_ROW_JOINER = Collectors.joining(TABLE_STRING_REPR_ROW_DELIMITER);
	}

	static {
		TABLE_STRING_REPR_COL_DELIMITER = "\t";
		TABLE_ROW_CELL_JOINER = Collectors.joining(TABLE_STRING_REPR_COL_DELIMITER);
	}

	public static void main(final String[] args) throws IOException, JAXBException, InterruptedException {
		if (args.length < 1) {
			final JFileChooser fileChooser = new JFileChooser(System.getProperty("user.dir"));
			final FileNameExtensionFilter filter = new FileNameExtensionFilter("XML files (*.xml)", "xml");
			fileChooser.setFileFilter(filter);
			fileChooser.setDialogTitle("Input file");
			fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
			UserPrompts.promptFile(fileChooser).ifPresent(inpath -> {
				LOGGER.info("Will read annotations from \"{}\".", inpath);
				fileChooser.setDialogTitle("Output dir");
				fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
				fileChooser.removeChoosableFileFilter(filter);
				UserPrompts.promptFile(fileChooser).map(File::toPath).ifPresent(outpath -> {
					LOGGER.info("Will write data to \"{}\".", outpath);
					UserPrompts.promptNonBlankString("Enter output filename prefix.", DEFAULT_OUTFILE_PREFIX)
							.ifPresent(outfileNamePrefix -> {
								LOGGER.info("Will prefix each output file with \"{}\".", outfileNamePrefix);
								try {
									run(inpath, outpath, outfileNamePrefix);
								} catch (final JAXBException e) {
									throw new RuntimeException(e);
								} catch (final IOException e) {
									throw new UncheckedIOException(e);
								}
							});
				});
			});
		} else {
			final CommandLineParser parser = new DefaultParser();
			try {
				final CommandLine cl = parser.parse(OPTIONS, args);
				if (cl.hasOption(Parameter.HELP.optName)) {
					printHelp();
				} else {
					final File inpath = (File) cl.getParsedOptionValue(Parameter.INPATH.optName);
					LOGGER.info("Will read annotations from \"{}\".", inpath);
					final Path outpath = ((File) cl.getParsedOptionValue(Parameter.OUTPATH.optName)).toPath();
					LOGGER.info("Will write data to \"{}\".", outpath);
					final String outfileNamePrefix = parseOutfilePrefix(cl, inpath);
					LOGGER.info("Will prefix each output file with \"{}\".", outfileNamePrefix);
					run(inpath, outpath, outfileNamePrefix);
				}
			} catch (final ParseException e) {
				System.out.println(String.format("An error occured while parsing the command-line arguments: %s", e));
				printHelp();
			}
		}

	}

	private static List<List<String>> createColHeaders() {
		final List<List<String>> imgViewDescColHeaders = ImageVisualizationInfoTableRowWriter.createColumnHeaders();
		final int resultColCount = imgViewDescColHeaders.stream().mapToInt(List::size).max().getAsInt()
				+ FEATURES_TO_DESCRIBE.size();

		final Iterator<List<String>> imgDescHeaderIter = imgViewDescColHeaders.iterator();
		List<List<String>> result;
		if (imgDescHeaderIter.hasNext()) {
			result = new ArrayList<>(imgViewDescColHeaders.size());
			final List<String> firstHeader = new ArrayList<>(resultColCount);
			result.add(firstHeader);

			FEATURES_TO_DESCRIBE.stream().map(Object::toString).forEachOrdered(firstHeader::add);
			final List<String> firstImgDescHeader = imgDescHeaderIter.next();
			firstHeader.addAll(firstImgDescHeader);
			System.out.println(firstHeader);
			final String padding = "";
			while (firstHeader.size() < resultColCount) {
				firstHeader.add(padding);
			}

			while (imgDescHeaderIter.hasNext()) {
				final List<String> nextImgDescHeader = imgDescHeaderIter.next();
				final List<String> nextHeader = new ArrayList<>(resultColCount);
				result.add(nextHeader);

				// Add padding for feature-derived descriptions
				FEATURES_TO_DESCRIBE.stream().map(feature -> padding).forEach(nextHeader::add);
				nextHeader.addAll(nextImgDescHeader);
			}

		} else {
			result = Collections.emptyList();
		}
		return result;
	}

	private static Options createOptions() {
		final Options result = new Options();
		Arrays.stream(Parameter.values()).map(Parameter::get).forEach(result::addOption);
		return result;
	}

	private static String createUtteranceDialogString(final Stream<Utterance> utts,
			final Function<? super Utterance, String> uttPlayerIdGetter) {
		final Stream<String> uttStrs = utts.map(utt -> {
			final String playerId = uttPlayerIdGetter.apply(utt);
			return playerId + ": \"" + utt.getTokens().stream().collect(WORD_JOINER) + "\"";
		});
		return uttStrs.collect(SENTENCE_JOINER);
	}

	private static String parseOutfilePrefix(final CommandLine cl, final File inpath) {
		final String infix = new FilenameBaseSplitter().apply(inpath.getName())[0] + "_LOG-";
		final String prefix = cl.getOptionValue(Parameter.OUTFILE_PREFIX.optName, DEFAULT_OUTFILE_PREFIX);
		return prefix + infix;
	}

	private static void printHelp() {
		final HelpFormatter formatter = new HelpFormatter();
		formatter.printHelp(EventUtterancePrinter.class.getName(), OPTIONS);
	}

	private static void run(final File inpath, final Path outpath, final String outfileNamePrefix)
			throws JAXBException, IOException {
		LOGGER.info("Reading annotations from \"{}\".", inpath);
		final Annotation uttAnnots = HAT.readAnnotation(inpath);

		final Map<String, String> sourceIdPlayerIds = Annotations.createSourceIdPlayerIdMap(uttAnnots);
		final Set<String> playerIds = new HashSet<>(sourceIdPlayerIds.values());
		final int expectedEventLogFileCount = playerIds.size();
		final Path sessionLogDir = inpath.getParentFile().toPath();
		LOGGER.info("Processing session log directory \"{}\".", sessionLogDir);
		final Map<String, Path> playerEventLogFilePaths = LoggedEvents.createPlayerEventLogFileMap(sessionLogDir,
				expectedEventLogFileCount);
		final Table<String, String, GameHistory> playerGameHistoryTable = LoggedEvents.createPlayerGameHistoryTable(
				playerEventLogFilePaths.entrySet(), EXPECTED_UNIQUE_GAME_COUNT, REQUIRED_EVENT_MATCHER);
		final Set<String> playerGameIdIntersection = new HashSet<>(playerGameHistoryTable.columnKeySet());
		playerGameHistoryTable.rowMap().values().stream().map(Map::keySet).forEach(playerGameIdIntersection::retainAll);
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

			final String gameId = playerGameIdIntersection.iterator().next();

			final List<Segment> segments = uttAnnots.getSegments().getSegment();
			final Map<Utterance, String> uttPlayerIds = new UtterancePlayerIdMapFactory(SEG_UTT_FACTORY,
					sourceIdPlayerIds::get).apply(segments);
			final List<Utterance> utts = Arrays
					.asList(uttPlayerIds.keySet().stream()
							.sorted(Comparator.comparing(Utterance::getStartTime)
									.thenComparing(Comparator.comparing(Utterance::getEndTime)))
							.toArray(Utterance[]::new));
			final Map<String, GameHistory> playerGameHistories = playerGameHistoryTable.columnMap().get(gameId);
			final UtteranceGameContextFactory uttContextFactory = new UtteranceGameContextFactory(
					playerGameHistories::get);
			final int uniqueModelDescriptionCount = playerGameHistoryTable.values().size();
			final GameContextModelFactory gameModelFactory = new GameContextModelFactory(uniqueModelDescriptionCount);
			final ImageVisualizationInfoUnmarshaller imgVizInfoUnmarshaller = new ImageVisualizationInfoUnmarshaller();
			final SelectedEntityFeatureExtractor entityFeatureExtractor = new SelectedEntityFeatureExtractor(
					new GameContextModelFactory(uniqueModelDescriptionCount), new ImageEdgeCounter());

			for (final Entry<String, GameHistory> playerGameHistory : playerGameHistories.entrySet()) {
				final String playerId = playerGameHistory.getKey();
				final GameHistory history = playerGameHistory.getValue();
				// The visualization info for the given game
				final ImageVisualizationInfo imgVizInfo = imgVizInfoUnmarshaller
						.apply(history.getInitialState().getImageVisualizationInfoDescription());

				final Stream<Entry<Event, List<Utterance>>> eventUttLists = EventUtterances
						.createEventUtteranceMappings(utts, history, NEW_SALIENT_PIECE_EVENT_MATCHER);

				final List<List<String>> colHeaders = createColHeaders();
				final String headerStr = colHeaders.stream()
						.map(header -> header.stream().collect(TABLE_ROW_CELL_JOINER)).collect(TABLE_ROW_JOINER);

				final Path outfilePath = outpath.resolve(outfileNamePrefix + playerId + ".txt");
				LOGGER.info("Writing utterances from perspective of \"{}\" to \"{}\".", playerId, outfilePath);
				try (BufferedWriter writer = Files.newBufferedWriter(outfilePath, StandardOpenOption.CREATE,
						StandardOpenOption.TRUNCATE_EXISTING)) {
					writer.write(headerStr);

					for (final Iterator<Entry<Event, List<Utterance>>> eventUttListIter = eventUttLists
							.iterator(); eventUttListIter.hasNext();) {
						writer.write(TABLE_STRING_REPR_ROW_DELIMITER);
						final Entry<Event, List<Utterance>> eventUttList = eventUttListIter.next();
						final Event event = eventUttList.getKey();
						final List<Utterance> eventUtts = eventUttList.getValue();

						final String imgVizInfoDesc;
						if (event == null) {
							final int colCount = colHeaders.stream().mapToInt(List::size).max().getAsInt();
							final String[] blankCells = new String[colCount];
							Arrays.fill(blankCells, "-");
							imgVizInfoDesc = Arrays.stream(blankCells).collect(TABLE_ROW_CELL_JOINER);
						} else {
							final StringWriter strWriter = new StringWriter(256);
							final double[] featureVector = eventUtts.stream().map(eventUtt -> {
								// Just use the first event context
								final GameContext context = uttContextFactory.apply(eventUtt, playerId).findFirst()
										.get();
								final DoubleStream.Builder vals = DoubleStream.builder();
								entityFeatureExtractor.accept(context, vals);
								return vals.build();
							}).map(DoubleStream::toArray).findFirst().get();
							writer.write(FEATURES_TO_DESCRIBE.stream()
									.map(feature -> Double.toString(feature.getVal(featureVector)))
									.collect(TABLE_ROW_CELL_JOINER));
							writer.write(TABLE_STRING_REPR_COL_DELIMITER);
							final ImageVisualizationInfoTableRowWriter imgInfoDescWriter = new ImageVisualizationInfoTableRowWriter(
									strWriter);
							final Move move = (Move) event.get(GameManagementEvent.Attribute.MOVE.toString());
							final Integer selectedPieceId = move.getPieceId();
							final ImageVisualizationInfo.Datum selectedPieceImgVizInfo = imgVizInfo.getData()
									.get(selectedPieceId);
							imgInfoDescWriter.write(selectedPieceId, selectedPieceImgVizInfo);

							imgVizInfoDesc = strWriter.toString();
						}
						writer.write(imgVizInfoDesc);
						writer.write(TABLE_STRING_REPR_COL_DELIMITER);

						final String eventDialogStr = createUtteranceDialogString(eventUtts.stream(),
								uttPlayerIds::get);
						writer.write(eventDialogStr);
					}

				}
			}

		} else {
			throw new UnsupportedOperationException(
					String.format("No logic for handling a game count of %d.", gameCount));
		}

	}

	private static void writeFeatureDescs(final Writer writer, final double[] featureVector) throws IOException {
		for (final EntityFeature feature : FEATURES_TO_DESCRIBE) {
			writer.write(Double.toString(feature.getVal(featureVector)));
		}
	}

}
