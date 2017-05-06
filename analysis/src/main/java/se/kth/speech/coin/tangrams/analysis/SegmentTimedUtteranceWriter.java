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
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDateTime;
import java.util.EnumSet;
import java.util.List;
import java.util.function.Supplier;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.xml.bind.JAXBException;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.OptionGroup;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import iristk.util.HAT;
import se.kth.speech.MutablePair;
import se.kth.speech.TimestampArithmetic;
import se.kth.speech.coin.tangrams.iristk.EventTimes;
import se.kth.speech.coin.tangrams.iristk.EventTypeMatcher;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;
import se.kth.speech.coin.tangrams.iristk.io.LoggedEvents;
import se.kth.speech.hat.xsd.Annotation;
import se.kth.speech.hat.xsd.Annotation.Segments.Segment;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 5 May 2017
 *
 */
public final class SegmentTimedUtteranceWriter {

	private enum Parameter implements Supplier<Option> {
		HELP("?") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("help").desc("Prints this message.").build();
			}
		},
		HAT_FILE("h") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("hat").desc("The path to the HAT file to process.").hasArg()
						.argName("path").type(File.class).required().build();
			}
		},
		INITIAL_TIMESTAMP(INITIAL_TIMESTAMP_OPT_NAME) {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("timestamp")
						.desc("The initial timestamp to use for calculating utterance times.").hasArg()
						// See
						// se.kth.speech.coin.tangrams.iristk.EventTimes.FORMATTER
						.argName("'yyyy-MM-dd HH:mm:ss.SSS'").build();
			}
		},
		EVENT_LOG(EVENT_LOG_OPT_NAME) {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("log")
						.desc("The event log to use for calculating utterance times.").hasArg().argName("path")
						.type(File.class).build();
			}
		};

		private static final EnumSet<Parameter> TIMESTAMP_PARAMS = EnumSet.of(INITIAL_TIMESTAMP, EVENT_LOG);

		protected final String optName;

		private Parameter(final String optName) {
			this.optName = optName;
		}

	}

	private static final Options OPTIONS;

	private static final OptionGroup TIMESTAMP_OPTS;

	static {
		OPTIONS = new Options();
		EnumSet.complementOf(Parameter.TIMESTAMP_PARAMS).stream().map(Parameter::get).forEach(OPTIONS::addOption);
		TIMESTAMP_OPTS = new OptionGroup();
		Parameter.TIMESTAMP_PARAMS.stream().map(Parameter::get).forEach(TIMESTAMP_OPTS::addOption);
		TIMESTAMP_OPTS.setRequired(true);
		OPTIONS.addOptionGroup(TIMESTAMP_OPTS);
	}

	private static final String INITIAL_TIMESTAMP_OPT_NAME = "t";

	private static final String EVENT_LOG_OPT_NAME = "l";

	private static final Logger LOGGER = LoggerFactory.getLogger(SegmentTimedUtteranceWriter.class);

	private static final SegmentUtteranceFactory SEG_UTT_FACTORY = new SegmentUtteranceFactory();

	private static final Collector<CharSequence, ?, String> WORD_JOINER = Collectors.joining(" ");

	public static void main(final String[] args) throws JAXBException, IOException {
		final CommandLineParser parser = new DefaultParser();
		try {
			final CommandLine cl = parser.parse(OPTIONS, args);
			if (cl.hasOption(Parameter.HELP.optName)) {
				printHelp();
			} else {
				final String selectedTimestampOpt = TIMESTAMP_OPTS.getSelected();
				final Object timestampOptVal = cl.getParsedOptionValue(selectedTimestampOpt);
				final LocalDateTime initialTime;
				switch (selectedTimestampOpt) {
				case EVENT_LOG_OPT_NAME: {
					final Path eventLogFilePath = ((File) timestampOptVal).toPath();
					LOGGER.info("Reading log at \"{}\" to find timestamp.", eventLogFilePath);
					initialTime = EventTimes
							.parseEventTime(LoggedEvents.parseLoggedEvents(Files.lines(eventLogFilePath))
									.filter(new EventTypeMatcher(EnumSet.of(GameManagementEvent.GAME_READY_RESPONSE)))
									.findFirst().get().getTime());
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

				final File hatFile = (File) cl.getParsedOptionValue(Parameter.HAT_FILE.optName);
				LOGGER.info("Reading annotations from \"{}\".", hatFile);
				final Annotation uttAnnots = HAT.readAnnotation(hatFile);
				final List<Segment> segments = uttAnnots.getSegments().getSegment();
				final Stream<Utterance> utts = SEG_UTT_FACTORY.create(segments.stream()).flatMap(List::stream);
				final Stream<MutablePair<String, String>> uttReprTimestamps = utts.map(utt -> {
					final float startTime = utt.getStartTime();
					final String uttRepr = utt.getTokens().stream().collect(WORD_JOINER);
					LOGGER.debug("Start time for \"{}\" is{}.", uttRepr, startTime);
					final LocalDateTime uttTime = TimestampArithmetic.createOffsetTimestamp(initialTime, startTime);
					final String uttTimestamp = uttTime.format(EventTimes.FORMATTER);
					return new MutablePair<>(uttRepr, uttTimestamp);
				});
				System.out.println("UTTERANCE\tTIME");
				uttReprTimestamps.map(uttRepr -> String.format("%s\t%s", uttRepr.getKey(), uttRepr.getValue()))
						.forEachOrdered(System.out::println);
			}
		} catch (final ParseException e) {
			System.out.println(String.format("An error occured while parsing the command-line arguments: %s", e));
			printHelp();
		}

	}

	private static void printHelp() {
		final HelpFormatter formatter = new HelpFormatter();
		formatter.printHelp(SegmentTimedUtteranceWriter.class.getName(), OPTIONS);
	}

}
