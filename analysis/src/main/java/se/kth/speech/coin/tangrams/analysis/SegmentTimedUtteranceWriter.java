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

import java.io.File;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.time.LocalDateTime;
import java.util.EnumSet;
import java.util.List;
import java.util.Map.Entry;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Stream;

import javax.xml.bind.JAXBException;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.MissingOptionException;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.OptionGroup;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import iristk.system.Event;
import se.kth.speech.TimestampArithmetic;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.coin.tangrams.iristk.EventTimes;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;
import se.kth.speech.coin.tangrams.iristk.io.LoggedEventReader;
import se.kth.speech.higgins._2005.annotation.Annotation;
import se.kth.speech.higgins._2005.annotation.Annotation.Segments.Segment;
import se.kth.speech.higgins.io.HatIO;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 5 May 2017
 *
 */
final class SegmentTimedUtteranceWriter { // NO_UCD (use default)

	private enum Parameter implements Supplier<Option> {
		EVENT_LOG(EVENT_LOG_OPT_NAME) {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("log")
						.desc("The event log to use for calculating utterance times.").hasArg().argName("path")
						.type(File.class).build();
			}
		},
		HELP("?") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("help").desc("Prints this message.").build();
			}
		},
		INITIAL_TIMESTAMP(INITIAL_TIMESTAMP_OPT_NAME) {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("timestamp")
						.desc("The initial timestamp to use for calculating utterance times.").hasArg()
						// See
						// se.kth.speech.coin.tangrams.iristk.EventTimes.FORMATTER
						.argName("'yyyy-[m]m-[d]d hh:mm:ss[.f...]'").build();
			}
		};

		private static final Options OPTIONS;

		private static final OptionGroup TIMESTAMP_OPTS;

		private static final EnumSet<Parameter> TIMESTAMP_PARAMS = EnumSet.of(INITIAL_TIMESTAMP, EVENT_LOG);

		static {
			OPTIONS = new Options();
			EnumSet.complementOf(Parameter.TIMESTAMP_PARAMS).stream().map(Parameter::get).forEach(OPTIONS::addOption);
			TIMESTAMP_OPTS = new OptionGroup();
			Parameter.TIMESTAMP_PARAMS.stream().map(Parameter::get).forEach(TIMESTAMP_OPTS::addOption);
			TIMESTAMP_OPTS.setRequired(true);
			OPTIONS.addOptionGroup(TIMESTAMP_OPTS);
		}

		private static void printHelp() {
			final HelpFormatter formatter = new HelpFormatter();
			formatter.printHelp(SegmentTimedUtteranceWriter.class.getSimpleName() + " INPATHS...", OPTIONS);
		}

		protected final String optName;

		private Parameter(final String optName) {
			this.optName = optName;
		}

	}

	private static final String EVENT_LOG_OPT_NAME = "l";

	private static final Predicate<Event> INITIAL_EVENT_PREDICATE = event -> GameManagementEvent.GAME_READY_RESPONSE
			.equals(GameManagementEvent.getEventType(event.getName()));

	private static final String INITIAL_TIMESTAMP_OPT_NAME = "t";

	private static final Logger LOGGER = LoggerFactory.getLogger(SegmentTimedUtteranceWriter.class);

	private static final Charset OUTPUT_ENCODING = StandardCharsets.UTF_8;

	/**
	 * Just use the source ID as the speaker ID
	 */
	private static final SegmentUtteranceFactory SEG_UTT_FACTORY = new SegmentUtteranceFactory();

	public static void main(final String[] args) throws JAXBException, IOException {
		final CommandLineParser parser = new DefaultParser();
		try {
			final CommandLine cl = parser.parse(Parameter.OPTIONS, args);
			if (cl.hasOption(Parameter.HELP.optName)) {
				Parameter.printHelp();
			} else {
				final File[] infiles = cl.getArgList().stream()
						.map(File::new).toArray(File[]::new);
				if (infiles.length < 1) {
					throw new MissingOptionException("No input file(s) specified.");

				} else {
					final String selectedTimestampOpt = Parameter.TIMESTAMP_OPTS.getSelected();
					final Object timestampOptVal = cl.getParsedOptionValue(selectedTimestampOpt);
					final LocalDateTime initialTime;
					switch (selectedTimestampOpt) {
					case EVENT_LOG_OPT_NAME: {
						final Path eventLogFilePath = ((File) timestampOptVal).toPath();
						initialTime = parseInitialTime(eventLogFilePath);
						break;
					}
					case INITIAL_TIMESTAMP_OPT_NAME: {
						initialTime = EventTimes.parseEventTime(timestampOptVal.toString());
						break;
					}
					default: {
						throw new AssertionError("No logic for case statement val \"" + selectedTimestampOpt + "\".");
					}
					}
					LOGGER.info("Initial timestamp is \"{}\".", EventTimes.FORMATTER.format(initialTime));

					try (PrintWriter outputWriter = new PrintWriter(
							new OutputStreamWriter(System.out, OUTPUT_ENCODING))) {
						for (final File infile : infiles) {
							LOGGER.info("Reading annotations from \"{}\".", infile);
							final Annotation uttAnnots = (Annotation) HatIO.fetchUnmarshaller().unmarshal(infile);
							final List<Segment> segments = uttAnnots.getSegments().getSegment();
							final Stream<Utterance> utts = SEG_UTT_FACTORY.create(segments.stream())
									.flatMap(List::stream);
							final Stream<Entry<String, String>> uttReprTimestamps = utts.map(utt -> {
								final float startTime = utt.getStartTime();
								final String uttRepr = utt.createTokenString();
								LOGGER.debug("Start time for \"{}\" is{}.", uttRepr, startTime);
								final LocalDateTime uttTime = TimestampArithmetic.createOffsetTimestamp(initialTime,
										startTime);
								final String uttTimestamp = uttTime.format(EventTimes.FORMATTER);
								return Pair.of(uttRepr, uttTimestamp);
							});
							outputWriter.println("UTTERANCE\tTIME");
							uttReprTimestamps
									.map(uttRepr -> String.format("%s\t%s", uttRepr.getKey(), uttRepr.getValue()))
									.forEachOrdered(outputWriter::println);
						}
					}
				}
			}
		} catch (final ParseException e) {
			System.out.println(String.format("An error occured while parsing the command-line arguments: %s", e));
			Parameter.printHelp();
		}

	}

	private static LocalDateTime parseInitialTime(final Path eventLogFilePath) throws IOException {
		LOGGER.info("Reading log at \"{}\" to find timestamp.", eventLogFilePath);
		try (Stream<Event> initialEvents = LoggedEventReader.readLoggedEvents(eventLogFilePath)
				.filter(INITIAL_EVENT_PREDICATE)) {
			return EventTimes.parseEventTime(initialEvents.findFirst().get().getTime());
		}
	}

	private SegmentTimedUtteranceWriter() {

	}

}
