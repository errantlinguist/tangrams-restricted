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
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.Timestamp;
import java.util.Arrays;
import java.util.Iterator;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.regex.Pattern;
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

import com.eclipsesource.json.JsonObject;

import iristk.system.Event;
import se.kth.speech.coin.tangrams.CLIParameters;
import se.kth.speech.coin.tangrams.iristk.io.LoggedEvents;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Apr 29, 2017
 *
 */
final class LoggedEventTimeStretcher {

	private enum Parameter implements Supplier<Option> {
		EVENT_SENDER_PATTERN("p") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("pattern")
						.desc("A regular expression matching the sender ID of the events to change.").hasArg()
						.argName("regex").required().build();
			}
		},
		FACTOR("f") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("factor").desc("The amount to stretch the times by.").hasArg()
						.argName("value").required().build();
			}
		},
		HELP("?") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("help").desc("Prints this message.").build();
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
			formatter.printHelp(LoggedEventTimeStretcher.class.getSimpleName() + " INPATH", OPTIONS);
		}

		protected final String optName;

		private Parameter(final String optName) {
			this.optName = optName;
		}
	}

	private static final Logger LOGGER = LoggerFactory.getLogger(LoggedEventTimeStretcher.class);

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

	private static void main(final CommandLine cl) throws IOException, ParseException {
		if (cl.hasOption(Parameter.HELP.optName)) {
			Parameter.printHelp();
		} else {
			final Path[] inpaths = cl.getArgList().stream().map(String::trim).filter(path -> !path.isEmpty())
					.map(Paths::get).toArray(Path[]::new);
			switch (inpaths.length) {
			case 0: {
				throw new MissingOptionException("No input path specified.");
			}
			case 1: {
				final Path inpath = inpaths[0];
				LOGGER.info("Will read event log data from \"{}\".", inpath);
				final Pattern evtSenderPattern = Pattern
						.compile(cl.getOptionValue(Parameter.EVENT_SENDER_PATTERN.optName));
				LOGGER.info("Using \"{}\" to match sender ID.", evtSenderPattern.pattern());
				final BigDecimal stretchFactor = new BigDecimal(cl.getOptionValue(Parameter.FACTOR.optName));
				LOGGER.info("Will stretch logged events by a factor of {}.", stretchFactor);
				try (final PrintWriter out = CLIParameters
						.parseOutpath((File) cl.getParsedOptionValue(Parameter.OUTPATH.optName))) {
					run(inpath, evt -> {
						final String playerId = evt.getString(GameManagementEvent.Attribute.PLAYER_ID.toString());
						return playerId == null ? false : evtSenderPattern.matcher(playerId).matches();
					}, stretchFactor, out);
				}
				break;
			}
			default: {
				throw new IllegalArgumentException("No support for multiple inpaths (yet).");
			}
			}
		}
	}

	private static void run(final Path inpath, final Predicate<? super Event> evtFilter, final BigDecimal stretchFactor,
			final PrintWriter out) throws IOException {
		LOGGER.info("Reading event log data from \"{}\".", inpath);
		final Stream<Event> events = LoggedEvents.parseLoggedEvents(Files.lines(inpath));
		LOGGER.info("Stretching logged events by a factor of {}.", stretchFactor);
		final Stream<Event> shiftedEvents = events.map(event -> {
			if (evtFilter.test(event)) {
				stretchTime(event, stretchFactor);
			}
			return event;
		});

		final Iterator<String> eventReprIter = shiftedEvents.map(Event::toJSON).map(JsonObject::toString).iterator();
		if (eventReprIter.hasNext()) {
			out.print(eventReprIter.next());
			while (eventReprIter.hasNext()) {
				out.println();
				out.print(eventReprIter.next());
			}
		}
	}

	private static void stretchTime(final Event event, final BigDecimal stretchFactor) {
		final Timestamp timestamp = Timestamp.valueOf(event.getTime());
		final BigDecimal newTimeMills = stretchFactor.multiply(new BigDecimal(timestamp.getTime()));
		timestamp.setTime(newTimeMills.setScale(0, RoundingMode.HALF_UP).longValue());
		event.setTime(timestamp.toString());
	}
}
