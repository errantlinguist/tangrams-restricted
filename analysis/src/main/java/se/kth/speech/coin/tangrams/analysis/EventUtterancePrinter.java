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
import java.io.PrintStream;
import java.nio.file.Path;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
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
import java.util.regex.Pattern;
import java.util.stream.Collector;
import java.util.stream.Collectors;
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

import iristk.system.Event;
import iristk.util.HAT;
import se.kth.speech.MutablePair;
import se.kth.speech.TimestampArithmetic;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;
import se.kth.speech.coin.tangrams.iristk.events.GameStateDescription;
import se.kth.speech.coin.tangrams.iristk.io.LoggedEvents;
import se.kth.speech.hat.xsd.Annotation;
import se.kth.speech.hat.xsd.Annotation.Segments.Segment;
import se.kth.speech.hat.xsd.Annotation.Tracks.Track;
import se.kth.speech.hat.xsd.Annotation.Tracks.Track.Sources;
import se.kth.speech.hat.xsd.Annotation.Tracks.Track.Sources.Source;

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
		};

		protected final String optName;

		private Parameter(final String optName) {
			this.optName = optName;
		}

	}

	private static final int EXPECTED_UNIQUE_GAME_COUNT = 1;

	private static final Logger LOGGER = LoggerFactory.getLogger(EventUtterancePrinter.class);

	/**
	 * @see <a href=
	 *      "http://stackoverflow.com/a/4546093/1391325">StackOverflow</a>
	 */
	private static final Pattern MINIMAL_FILE_EXT_PATTERN = Pattern.compile("\\.(?=[^\\.]+$)");

	private static final Options OPTIONS = createOptions();

	private static final Function<Segment, List<Utterance>> SEG_UTT_FACTORY = new SegmentUtteranceFactory();

	public static void main(final String[] args) throws IOException, JAXBException, InterruptedException {
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

					final String gameId = playerGameIdIntersection.iterator().next();
					final Map<String, GameHistory> playerGameHistories = playerGameHistoryTable.columnMap().get(gameId);
					final List<Segment> segments = uttAnnots.getSegments().getSegment();
					final List<Utterance> utts = segments.stream().map(SEG_UTT_FACTORY).flatMap(List::stream)
							.sorted(Comparator.comparing(Utterance::getStartTime)
									.thenComparing(Comparator.comparing(Utterance::getEndTime)))
							.collect(Collectors.toList());
					final EventUtterancePrinter replayer = new EventUtterancePrinter(utts);

					final String playerPerspectiveToUse = "sinc";
					final GameHistory history = playerGameHistories.get(playerPerspectiveToUse);
					final Stream<Entry<Event, List<Utterance>>> eventUttLists = replayer.apply(history);
					final PrintStream out = System.out;
					final Collector<CharSequence, ?, String> joiner = Collectors.joining(" ");
					eventUttLists.forEachOrdered(eventUttList -> {
						final Event event = eventUttList.getKey();
						if (event == null) {
							out.println("EVENT - null");
						} else {
							out.println(String.format("EVENT - time %s - submitter %s", event.getTime(),
									event.get(GameManagementEvent.Attribute.PLAYER_ID.toString())));
							out.println(event);
						}

						final List<Utterance> eventUtts = eventUttList.getValue();
						if (eventUtts.isEmpty()) {
							out.println("\tNo utterances for event!");
						} else {
							final Stream<String> sents = eventUtts.stream().map(Utterance::getTokens).map(List::stream)
									.map(str -> str.collect(joiner));
							sents.forEachOrdered(sent -> out.println("\t" + sent));
						}

					});

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

	private static void printHelp() {
		final HelpFormatter formatter = new HelpFormatter();
		formatter.printHelp(EventUtterancePrinter.class.getName(), OPTIONS);
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
				{
					// Find all utterances up to the first event
					final Event firstEvent = eventIter.next();
					final Timestamp firstEventTimestamp = Timestamp.valueOf(firstEvent.getTime());
					final List<Utterance> firstUttList = new ArrayList<>();
					do {
						final Utterance nextUtt = uttIter.next();
						final float uttStartMills = nextUtt.getStartTime() * SegmentTimes.TIME_TO_MILLS_FACTOR;
						final Timestamp uttStartTimestamp = TimestampArithmetic.createOffsetTimestamp(gameStartTime,
								uttStartMills);
						// If the utterance was before the first event, add it
						// to the
						// list of before-event utterances
						if (uttStartTimestamp.compareTo(firstEventTimestamp) < 0) {
							firstUttList.add(nextUtt);
						} else {
							break;
						}
					} while (uttIter.hasNext());
					// Add the first list only if any utterances preceding the
					// first event were found
					if (!firstUttList.isEmpty()) {
						resultBuilder.accept(new MutablePair<>(null, firstUttList));
					}

					currentEvent = firstEvent;
				}
				{
					// Find the next set of utterances following each event
					while (eventIter.hasNext()) {
						final Event nextEvent = eventIter.next();
						final Timestamp nextEventTimestamp = Timestamp.valueOf(nextEvent.getTime());
						final List<Utterance> uttList = new ArrayList<>();
						while (uttIter.hasNext()) {
							final Utterance nextUtt = uttIter.next();
							final float uttStartMills = nextUtt.getStartTime() * SegmentTimes.TIME_TO_MILLS_FACTOR;
							final Timestamp uttStartTimestamp = TimestampArithmetic.createOffsetTimestamp(gameStartTime,
									uttStartMills);
							// If the utterance was before the next event, add
							// it to the
							// list of utterances for the current event
							if (uttStartTimestamp.compareTo(nextEventTimestamp) < 0) {
								uttList.add(nextUtt);
							} else {
								resultBuilder.accept(new MutablePair<>(currentEvent, uttList));
								currentEvent = nextEvent;
								break;
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
