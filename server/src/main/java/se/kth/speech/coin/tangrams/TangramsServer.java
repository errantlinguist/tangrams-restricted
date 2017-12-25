/*
 *  This file is part of tangrams.
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
package se.kth.speech.coin.tangrams;

import java.awt.Color;
import java.io.File;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Properties;
import java.util.function.Supplier;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.github.errantlinguist.ClassProperties;

import iristk.system.Broker;
import iristk.system.IrisSystem;
import iristk.system.LoggingModule;
import iristk.util.NameFilter;
import se.kth.speech.coin.tangrams.content.BoardArea;
import se.kth.speech.coin.tangrams.game.GameFactory;
import se.kth.speech.coin.tangrams.iristk.GameManagementServerModule;
import se.kth.speech.coin.tangrams.iristk.IrisSystemStopper;

/**
 *
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 10 Jan 2017
 *
 */
public final class TangramsServer implements Runnable { // NO_UCD (use default)

	public static final class Exception extends RuntimeException { // NO_UCD
																	// (use
																	// private)

		/**
		 *
		 */
		private static final long serialVersionUID = 4642677256770391448L;

		private Exception(final Throwable cause) {
			super(cause);
		}

	}

	private enum Parameter implements Supplier<Option> {
		BROKER_HOST("h") {

			@Override
			public Option get() {
				return Option.builder(optName).longOpt("broker-host").desc(
						"The hostname (or IP address) of the IrisTK broker system to use. If none is provided, a new broker system is started on the localhost listening to the provided port.")
						.hasArg().argName("hostname").build();
			}

		},
		BROKER_PORT("p") {

			@Override
			public Option get() {
				return Option.builder(optName).longOpt("broker-port")
						.desc("The port to use to connect to the IrisTK broker system to use.").hasArg().argName("port")
						// See http://stackoverflow.com/a/5955893/1391325
						.type(Number.class).build();
			}

		},
		HELP("?") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("help").desc("Prints this message.").build();
			}
		};

		private static final Options OPTIONS = createOptions();

		private static Options createOptions() {
			final Options result = new Options();
			Arrays.stream(Parameter.values()).map(Parameter::get).forEach(result::addOption);
			return result;
		}

		private static int parseBrokerPort(final CommandLine cl, final int defaultPort) throws ParseException {
			final int result;
			{
				final Number paramValue = (Number) cl.getParsedOptionValue(Parameter.BROKER_PORT.optName);
				if (paramValue == null) {
					result = defaultPort;
					LOGGER.info("No broker port provided; Using default \"{}\".", result);
				} else {
					result = paramValue.intValue();
					LOGGER.info("Using port \"{}\".", result);
				}
			}
			return result;
		}

		private static void printHelp() {
			final HelpFormatter formatter = new HelpFormatter();
			formatter.printHelp(TangramsServer.class.getName(), OPTIONS);
		}

		protected final String optName;

		private Parameter(final String optName) {
			this.optName = optName;
		}

	}

	private static final Logger LOGGER = LoggerFactory.getLogger(TangramsServer.class);

	public static void main(final CommandLine cl) {
		if (cl.hasOption(Parameter.HELP.optName)) {
			Parameter.printHelp();
		} else {
			final String brokerHost = cl.getOptionValue(Parameter.BROKER_HOST.optName);
			try {
				final Properties props = ClassProperties.load(TangramsServer.class);
				final int brokerPort = Parameter.parseBrokerPort(cl,
						Integer.parseInt(props.getProperty("broker.port")));
				// final FlagSettingUncaughtExceptionHandler
				// brokerExceptionHandler = new
				// FlagSettingUncaughtExceptionHandler();
				if (brokerHost == null) {
					LOGGER.info("No broker hostname provided; Starting local IrisTK broker.");
					final Broker broker = new Broker(brokerPort);
					// broker.setUncaughtExceptionHandler(brokerExceptionHandler);
					// TODO: This is copied from
					// "Broker.main(String[])";
					// Ensure that this thread is killed gracefully when
					// this
					// program is ended
					broker.start();

				} else {
					LOGGER.info("Using broker hostname \"{}\".", brokerHost);
				}

				// if (!brokerExceptionHandler.wasExceptionHandled.get()) {
				final TangramsServer server = new TangramsServer(props.getProperty("broker.ticket"), brokerHost,
						brokerPort);
				server.run();
				// }

			} catch (final ParseException e) {
				System.out.println(String.format("Could not parse port: %s", e.getLocalizedMessage()));
				Parameter.printHelp();
			} catch (final IOException e) {
				throw new UncheckedIOException(e);
			}
		}
	}

	public static void main(final String[] args) {
		final CommandLineParser parser = new DefaultParser();
		try {
			final CommandLine cl = parser.parse(Parameter.OPTIONS, args);
			main(cl);
		} catch (final ParseException e) {
			System.out.println(String.format("An error occured while parsing the command-line arguments: %s", e));
			Parameter.printHelp();
		}
	}

	private final String brokerHost;

	private final int brokerPort;

	private final String brokerTicket;

	private TangramsServer(final String brokerTicket, final String brokerHost, final int brokerPort) {
		this.brokerTicket = brokerTicket;
		this.brokerHost = brokerHost;
		this.brokerPort = brokerPort;
	}

	@Override
	public void run() {
		try {
			final IrisSystem system = new IrisSystem(getClass().getSimpleName(), new File("."));
			final IrisSystemStopper irisSystemStopper = new IrisSystemStopper(system);
			Runtime.getRuntime().addShutdownHook(new Thread(irisSystemStopper));
			// Try clause for ensuring that the IrisTK system gets
			// properly shut down in the case an exception occurs
			try {
				system.connectToBroker(brokerTicket, brokerHost, brokerPort);
				final Collection<Color> uiReservedColors = BoardArea.getDefaultBoardAreaColorMap().values();
				system.addModule(new GameManagementServerModule(new GameFactory(uiReservedColors)));
				system.addModule(new LoggingModule(new File("log", "server"), NameFilter.ALL, true));
				system.sendStartSignal();

			} catch (final java.lang.Exception exAfterIrisTKConst) {
				// NOTE: Finally doesn't work because of the threads
				// running in the background: A "finally" block will
				// kill the IrisTK system right after exiting this
				// method
				// even though the session should continue
				LOGGER.error(String.format(
						"An exception occurred sometime after constructing a(n) %s instance; Cleaning up gracefully and re-throwing exception.",
						system.getClass().getSimpleName()));
				irisSystemStopper.run();
				throw exAfterIrisTKConst;
			}
		} catch (final java.lang.Exception runningException) {
			final Exception wrapper = new Exception(runningException);
			LOGGER.error(String.format("An exception occurred while running the server; Re-throwing as a(n) %s.",
					wrapper.getClass().getSimpleName()));
			throw wrapper;
		}

	}

}