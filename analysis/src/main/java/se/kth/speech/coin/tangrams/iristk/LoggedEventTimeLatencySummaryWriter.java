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
package se.kth.speech.coin.tangrams.iristk;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.LongSummaryStatistics;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.regex.Pattern;
import java.util.stream.LongStream;
import java.util.stream.Stream;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.MissingOptionException;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import iristk.system.Event;
import se.kth.speech.Iterators;
import se.kth.speech.coin.tangrams.CLIParameters;
import se.kth.speech.coin.tangrams.iristk.io.LoggedEventReader;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Apr 29, 2017
 *
 */
final class LoggedEventTimeLatencySummaryWriter { // NO_UCD (use default)

	private enum Parameter implements Supplier<Option> {
		EVENT_SENDER_PATTERN("p") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("pattern")
						.desc("A regular expression matching the sender ID of the events to change.").hasArg()
						.argName("regex").required().build();
			}
		},
		OUTPATH("o") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("outpath").desc("The path to write the processed data to.")
						.hasArg().argName("path").type(File.class).build();
			}
		};

		private static final Options OPTIONS = createOptions();

		private static Options createOptions() {
			final Options result = new Options();
			Arrays.stream(Parameter.values()).map(Parameter::get).forEach(result::addOption);
			return result;
		}

		private static void printHelp() {
			final HelpFormatter formatter = new HelpFormatter();
			formatter.printHelp(LoggedEventTimeLatencySummaryWriter.class.getSimpleName() + " INPATH", OPTIONS);
		}

		protected final String optName;

		private Parameter(final String optName) {
			this.optName = optName;
		}
	}

	private static final Comparator<Event> EVENT_TIME_COMPARATOR = Comparator.comparing(Event::getTime,
			(timeStr1, timeStr2) -> {
				final LocalDateTime time1 = EventTimes.parseEventTime(timeStr1);
				final LocalDateTime time2 = EventTimes.parseEventTime(timeStr2);
				return time1.compareTo(time2);
			});

	private static final Logger LOGGER = LoggerFactory.getLogger(LoggedEventTimeLatencySummaryWriter.class);

	public static void main(final CommandLine cl) throws IOException, ParseException {
		final Path[] inpaths = cl.getArgList().stream()
				.map(Paths::get).toArray(Path[]::new);
		switch (inpaths.length) {
		case 0: {
			throw new MissingOptionException("No input path specified.");
		}
		case 1: {
			final Path inpath = inpaths[0];
			LOGGER.info("Will read event log data from \"{}\".", inpath);
			final Pattern evtSenderPattern = Pattern.compile(cl.getOptionValue(Parameter.EVENT_SENDER_PATTERN.optName));
			LOGGER.info("Using \"{}\" to match sender ID.", evtSenderPattern.pattern());
			try (final PrintWriter out = CLIParameters
					.parseOutpath((File) cl.getParsedOptionValue(Parameter.OUTPATH.optName))) {
				run(inpath, evt -> {
					final String playerId = evt.getString(GameManagementEvent.Attribute.PLAYER_ID.toString());
					return playerId == null ? false : evtSenderPattern.matcher(playerId).matches();
				}, out);
			}
			break;
		}
		default: {
			throw new IllegalArgumentException("No support for multiple inpaths (yet).");
		}
		}
	}

	public static void main(final String[] args) throws IOException {
		final CommandLineParser parser = new DefaultParser();
		try {
			final CommandLine cl = parser.parse(Parameter.OPTIONS, args);
			main(cl);
		} catch (final ParseException e) {
			System.out.println(String.format("An error occured while parsing the command-line arguments: %s", e));
			Parameter.printHelp();
		}
	}

	private static LongStream calculateEventTimeDifferences(final Iterator<Event> evtIter,
			final Predicate<? super Event> evtFilter) {
		final LongStream.Builder b = LongStream.builder();
		while (evtIter.hasNext()) {
			final Entry<Stream<Event>, Optional<Event>> preSenderEvts = Iterators.findElementsBeforeDelimiter(evtIter,
					evtFilter);
			final Optional<Event> optSenderEvt = preSenderEvts.getValue();
			optSenderEvt.ifPresent(senderEvt -> {
				LOGGER.debug("Sender event: {}", senderEvt);
				if (evtFilter.test(senderEvt)) {
					// https://stackoverflow.com/a/21441634/1391325
					final Optional<Event> optLastForeignEvent = preSenderEvts.getKey()
							.reduce((first, second) -> second);
					optLastForeignEvent.ifPresent(lastForeignEvent -> {
						LOGGER.debug("Last foreign event: {}", lastForeignEvent);

						final LocalDateTime senderEvtTime = EventTimes.parseEventTime(senderEvt.getTime());
						final LocalDateTime lastForeignEvtTime = EventTimes.parseEventTime(lastForeignEvent.getTime());
						final long diffMills = ChronoUnit.MILLIS.between(lastForeignEvtTime, senderEvtTime);
						LOGGER.debug("{} milliseconds' difference between sender event and last foreign event.",
								diffMills);
						b.accept(diffMills);
					});
				}
			});
		}
		return b.build();
	}

	private static void run(final Path inpath, final Predicate<? super Event> evtFilter, final PrintWriter out)
			throws IOException {
		LOGGER.info("Reading event log data from \"{}\".", inpath);

		List<Event> events = Collections.emptyList();
		try (Stream<Event> eventStream = LoggedEventReader.readLoggedEvents(inpath)) {
			events = Arrays.asList(eventStream.toArray(Event[]::new));
		}
		LOGGER.info("Read {} event(s) from file.", events.size());
		events.sort(EVENT_TIME_COMPARATOR);

		final LongStream evtTimeDiffs = calculateEventTimeDifferences(events.iterator(), evtFilter);
		final LongSummaryStatistics diffSummary = evtTimeDiffs.summaryStatistics();
		out.println("Count\t" + diffSummary.getCount());
		out.println("Max\t" + diffSummary.getMax());
		out.println("Min\t" + diffSummary.getMin());
		out.println("Mean\t" + diffSummary.getSum() / (double) diffSummary.getCount());
	}

}
