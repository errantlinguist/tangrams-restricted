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

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
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

import com.google.common.collect.Maps;
import com.google.common.collect.Table;

import iristk.system.Event;
import iristk.util.HAT;
import se.kth.speech.FilenameBaseSplitter;
import se.kth.speech.MutablePair;
import se.kth.speech.TimestampArithmetic;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;
import se.kth.speech.coin.tangrams.iristk.events.GameStateDescription;
import se.kth.speech.coin.tangrams.iristk.io.LoggedEvents;
import se.kth.speech.coin.tangrams.view.UserPrompts;
import se.kth.speech.hat.xsd.Annotation;
import se.kth.speech.hat.xsd.Annotation.Segments.Segment;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Apr 17, 2017
 */
public final class EventUtterancePrinter implements Function<GameHistory, Stream<Entry<Event, List<Utterance>>>> {

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

	private static final int EXPECTED_UNIQUE_GAME_COUNT = 1;

	private static final Logger LOGGER = LoggerFactory.getLogger(EventUtterancePrinter.class);

	private static final Options OPTIONS = createOptions();

	private static final Function<Segment, List<Utterance>> SEG_UTT_FACTORY = new SegmentUtteranceFactory();

	private static final String DEFAULT_OUTFILE_PREFIX = "eventUtts";

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

	private static Options createOptions() {
		final Options result = new Options();
		Arrays.stream(Parameter.values()).map(Parameter::get).forEach(result::addOption);
		return result;
	}

	private static Map<Utterance, String> createUtterancePlayerIdMap(final Collection<Segment> segments,
			final Function<? super String, String> sourcePlayerIdGetter) {
		final Map<Utterance, String> result = Maps.newHashMapWithExpectedSize(segments.size());
		for (final Segment segment : segments) {
			final String sourceId = segment.getSource();
			final List<Utterance> segUtts = SEG_UTT_FACTORY.apply(segment);
			for (final Utterance segUtt : segUtts) {
				final String playerId = sourcePlayerIdGetter.apply(sourceId);
				result.put(segUtt, playerId);
			}
		}
		return result;
	}

	private static String parseOutfilePrefix(final CommandLine cl, final File inpath) {
		final String infix = "_" + new FilenameBaseSplitter().apply(inpath.getName())[0] + "_LOG-";
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
		final Table<String, String, GameHistory> playerGameHistoryTable = LoggedEvents
				.createPlayerGameHistoryTable(playerEventLogFilePaths.entrySet(), EXPECTED_UNIQUE_GAME_COUNT);
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
			final Map<String, GameHistory> playerGameHistories = playerGameHistoryTable.columnMap().get(gameId);
			final List<Segment> segments = uttAnnots.getSegments().getSegment();
			final Map<Utterance, String> uttPlayerIds = createUtterancePlayerIdMap(segments, sourceIdPlayerIds::get);
			final List<Utterance> utts = uttPlayerIds.keySet().stream().sorted(Comparator
					.comparing(Utterance::getStartTime).thenComparing(Comparator.comparing(Utterance::getEndTime)))
					.collect(Collectors.toList());
			final EventUtterancePrinter replayer = new EventUtterancePrinter(utts);
			final Collector<CharSequence, ?, String> joiner = Collectors.joining(" ");
			playerGameHistories.forEach((playerId, history) -> {
				final Path outfilePath = outpath.resolve(outfileNamePrefix + playerId + ".txt");
				final Stream<Entry<Event, List<Utterance>>> eventUttLists = replayer.apply(history);
				LOGGER.info("Writing utterances from perspective of \"{}\" to \"{}\".", playerId, outfilePath);
				try (BufferedWriter writer = Files.newBufferedWriter(outfilePath, StandardOpenOption.CREATE,
						StandardOpenOption.TRUNCATE_EXISTING)) {
					eventUttLists.forEachOrdered(eventUttList -> {
						final Event event = eventUttList.getKey();
						try {
							if (event == null) {
								writer.write("EVENT - null");
								writer.newLine();
							} else {
								writer.write(String.format("EVENT - time %s - submitter %s", event.getTime(),
										event.get(GameManagementEvent.Attribute.PLAYER_ID.toString())));
								writer.newLine();
								writer.write(event.toString());
								writer.newLine();
							}

							final List<Utterance> eventUtts = eventUttList.getValue();
							if (eventUtts.isEmpty()) {
								writer.write("\tNo utterances for event!");
								writer.newLine();
							} else {
								final Stream<String> sents = eventUtts.stream().map(utt -> {
									final String speakingPlayerId = uttPlayerIds.get(utt);
									return speakingPlayerId + " - " + utt.getTokens().stream().collect(joiner);
								});
								sents.forEachOrdered(sent -> {
									try {
										writer.write("\t" + sent);
										writer.newLine();
									} catch (final IOException e) {
										throw new UncheckedIOException(e);
									}
								});
							}
						} catch (final IOException e) {
							throw new UncheckedIOException(e);
						}

					});
				} catch (final IOException e) {
					throw new UncheckedIOException(e);
				}
			});
		} else {
			throw new UnsupportedOperationException(
					String.format("No logic for handling a game count of %d.", gameCount));
		}
	}

	private final List<Utterance> utts;

	public EventUtterancePrinter(final List<Utterance> utts) {
		this.utts = utts;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Function#apply(java.lang.Object)
	 */
	@Override
	public Stream<Entry<Event, List<Utterance>>> apply(final GameHistory history) {
		final Stream<Event> events = history.getEvents().values().stream().flatMap(List::stream);
		final Timestamp gameStartTime = history.getStartTime();

		final Stream<Entry<Event, List<Utterance>>> result;

		final Iterator<Event> eventIter = events.iterator();
		if (eventIter.hasNext()) {
			final Iterator<Utterance> uttIter = utts.iterator();
			if (uttIter.hasNext()) {
				final Stream.Builder<Entry<Event, List<Utterance>>> resultBuilder = Stream.builder();
				Event currentEvent = null;
				List<Utterance> nextUttList = new ArrayList<>();
				{
					// Find all utterances up to the first event
					final Event firstEvent = eventIter.next();
					final Timestamp firstEventTimestamp = Timestamp.valueOf(firstEvent.getTime());
					do {
						final Utterance nextUtt = uttIter.next();
						final float uttStartMills = nextUtt.getStartTime() * SegmentTimes.TIME_TO_MILLS_FACTOR;
						final Timestamp uttStartTimestamp = TimestampArithmetic.createOffsetTimestamp(gameStartTime,
								uttStartMills);
						// If the utterance was before the first event, add it
						// to the
						// list of before-event utterances
						if (uttStartTimestamp.compareTo(firstEventTimestamp) < 0) {
							nextUttList.add(nextUtt);
						} else {
							break;
						}
					} while (uttIter.hasNext());
					// Add the first list only if any utterances preceding the
					// first event were found
					if (!nextUttList.isEmpty()) {
						resultBuilder.accept(new MutablePair<>(null, nextUttList));
						nextUttList = new ArrayList<>();
					}

					currentEvent = firstEvent;
				}
				{
					// Find the next set of utterances following each event
					while (eventIter.hasNext()) {
						final Event nextEvent = eventIter.next();
						final Timestamp nextEventTimestamp = Timestamp.valueOf(nextEvent.getTime());
						eventUtts: while (uttIter.hasNext()) {
							final Utterance nextUtt = uttIter.next();
							final float uttStartMills = nextUtt.getStartTime() * SegmentTimes.TIME_TO_MILLS_FACTOR;
							final Timestamp uttStartTimestamp = TimestampArithmetic.createOffsetTimestamp(gameStartTime,
									uttStartMills);
							// If the utterance was before the next event, add
							// it to the
							// list of utterances for the current event
							if (uttStartTimestamp.compareTo(nextEventTimestamp) < 0) {
								nextUttList.add(nextUtt);
							} else {
								resultBuilder.accept(new MutablePair<>(currentEvent, nextUttList));
								nextUttList = new ArrayList<>();
								nextUttList.add(nextUtt);
								currentEvent = nextEvent;
								break eventUtts;
							}
						}
					}
				}

				result = resultBuilder.build();

			} else {
				// No utterances were found; Return an empty list of utterances
				// for each event
				final List<Utterance> nullList = Collections.emptyList();
				result = events.map(event -> new MutablePair<>(event, nullList));
			}

		} else {
			// No events were found; Return all the utterances
			result = Stream.of(new MutablePair<>(null, Collections.unmodifiableList(utts)));
		}
		return result;
	}

}
