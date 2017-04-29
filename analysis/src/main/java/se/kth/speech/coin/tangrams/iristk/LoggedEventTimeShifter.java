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
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.sql.Timestamp;
import java.util.Arrays;
import java.util.Iterator;
import java.util.function.Supplier;
import java.util.stream.Stream;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.eclipsesource.json.JsonObject;

import iristk.system.Event;
import se.kth.speech.coin.tangrams.iristk.io.LoggedEvents;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Apr 29, 2017
 *
 */
public final class LoggedEventTimeShifter {

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
				return Option.builder(optName).longOpt("inpath").desc("The path to event log to process.").hasArg()
						.argName("path").type(File.class).required().build();
			}
		},
		ADDEND("a") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("addend").desc("The amount to shift the times by.").hasArg()
						.argName("value").type(Number.class).required().build();
			}
		},
		OUTPATH("o") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("outpath").desc("The path to write the processed data to.")
						.hasArg().argName("path").type(File.class).build();
			}
		};

		protected final String optName;

		private Parameter(final String optName) {
			this.optName = optName;
		}
	}

	private static final Logger LOGGER = LoggerFactory.getLogger(LoggedEventTimeShifter.class);

	private static final Options OPTIONS = createOptions();

	public static void main(final String[] args) throws IOException {
		final CommandLineParser parser = new DefaultParser();
		try {
			final CommandLine cl = parser.parse(OPTIONS, args);
			if (cl.hasOption(Parameter.HELP.optName)) {
				printHelp();
			} else {
				final Path inpath = ((File) cl.getParsedOptionValue(Parameter.INPATH.optName)).toPath();
				LOGGER.info("Reading annotations from \"{}\".", inpath);
				final Stream<Event> events = LoggedEvents.parseLoggedEvents(Files.lines(inpath));
				final double addendInSecs = ((Number) cl.getParsedOptionValue(Parameter.ADDEND.optName)).doubleValue();
				LOGGER.info("Shifting logged events by {} second(s).", addendInSecs);
				final long addendInMills = Math.round(addendInSecs * 1000);
				final Stream<Event> shiftedEvents = events.map(event -> {
					final Timestamp timestamp = Timestamp.valueOf(event.getTime());
					final long newTimeMills = timestamp.getTime() + addendInMills;
					timestamp.setTime(newTimeMills);
					event.setTime(timestamp.toString());
					return event;
				});

				try (final PrintWriter out = parseOutpath(cl)) {
					final Iterator<String> eventReprIter = shiftedEvents.map(Event::toJSON).map(JsonObject::toString)
							.iterator();
					if (eventReprIter.hasNext()) {
						out.print(eventReprIter.next());
						while (eventReprIter.hasNext()) {
							out.println();
							out.print(eventReprIter.next());
						}
					}
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
		formatter.printHelp(LoggedEventTimeShifter.class.getName(), OPTIONS);
	}

	/**
	 *
	 */
	public LoggedEventTimeShifter() {
		// TODO Auto-generated constructor stub
	}

}
