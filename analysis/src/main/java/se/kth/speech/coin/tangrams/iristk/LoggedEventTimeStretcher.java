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
import java.io.UncheckedIOException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.sql.Timestamp;
import java.util.Arrays;
import java.util.Iterator;
import java.util.function.Supplier;
import java.util.stream.Stream;

import javax.swing.JFileChooser;
import javax.swing.filechooser.FileNameExtensionFilter;

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
import se.kth.speech.coin.tangrams.view.UserPrompts;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Apr 29, 2017
 *
 */
public final class LoggedEventTimeStretcher {

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
		FACTOR("f") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("factor").desc("The amount to stretch the times by.").hasArg()
						.argName("value").required().build();
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

	private static final Logger LOGGER = LoggerFactory.getLogger(LoggedEventTimeStretcher.class);

	private static final Options OPTIONS = createOptions();

	public static void main(final String[] args) throws IOException {
		if (args.length < 1) {
			final JFileChooser fileChooser = new JFileChooser(System.getProperty("user.dir"));
			fileChooser.setDialogTitle("Input file");
			fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
			fileChooser.setFileFilter(new FileNameExtensionFilter("Text files (*.txt)", "txt"));
			UserPrompts.promptFile(fileChooser).map(File::toPath).ifPresent(inpath -> {
				LOGGER.info("Will read annotations from \"{}\".", inpath);
				fileChooser.setDialogTitle("Output file");
				fileChooser.setDialogType(JFileChooser.SAVE_DIALOG);
				UserPrompts.promptFile(fileChooser).ifPresent(outpath -> {
					LOGGER.info("Will write data to \"{}\".", outpath);
					UserPrompts.promptDecimalFraction("Enter factor to stretch event times by.")
							.ifPresent(stretchFactor -> {
								LOGGER.info("Will stretch logged events by a factor of {}.", stretchFactor);
								try (PrintWriter writer = new PrintWriter(outpath)) {
									run(inpath, stretchFactor, writer);
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
					final Path inpath = ((File) cl.getParsedOptionValue(Parameter.INPATH.optName)).toPath();
					LOGGER.info("Will read annotations from \"{}\".", inpath);
					final BigDecimal stretchFactor = new BigDecimal(cl.getOptionValue(Parameter.FACTOR.optName));
					LOGGER.info("Will stretch logged events by a factor of {}.", stretchFactor);
					try (final PrintWriter out = parseOutpath(cl)) {
						run(inpath, stretchFactor, out);
					}
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
		formatter.printHelp(LoggedEventTimeStretcher.class.getName(), OPTIONS);
	}

	private static void run(final Path inpath, final BigDecimal stretchFactor, final PrintWriter out)
			throws IOException {
		LOGGER.info("Reading annotations from \"{}\".", inpath);
		final Stream<Event> events = LoggedEvents.parseLoggedEvents(Files.lines(inpath));
		LOGGER.info("Stretching logged events by a factor of {}.", stretchFactor);
		final Stream<Event> shiftedEvents = events.map(event -> {
			final Timestamp timestamp = Timestamp.valueOf(event.getTime());
			final BigDecimal newTimeMills = stretchFactor.multiply(new BigDecimal(timestamp.getTime()));
			timestamp.setTime(newTimeMills.setScale(0, RoundingMode.HALF_UP).longValue());
			event.setTime(timestamp.toString());
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

}
