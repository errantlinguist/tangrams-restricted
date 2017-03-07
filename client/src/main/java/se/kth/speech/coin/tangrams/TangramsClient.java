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

import java.awt.Component;
import java.awt.EventQueue;
import java.awt.Image;
import java.awt.Point;
import java.io.File;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Date;
import java.util.EnumMap;
import java.util.Map;
import java.util.Objects;
import java.util.Properties;
import java.util.Random;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

import javax.swing.JOptionPane;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;

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

import iristk.system.IrisSystem;
import iristk.system.LoggingModule;
import iristk.util.NameFilter;
import se.kth.speech.coin.tangrams.content.PieceIdImageFactoryFactory;
import se.kth.speech.coin.tangrams.game.LocalController;
import se.kth.speech.coin.tangrams.game.Model;
import se.kth.speech.coin.tangrams.iristk.GameManagementClientModule;
import se.kth.speech.coin.tangrams.iristk.IrisSystemStopper;
import se.kth.speech.coin.tangrams.iristk.LogDirectoryFactory;
import se.kth.speech.coin.tangrams.view.ConnectionStatusFrame;
import se.kth.speech.coin.tangrams.view.GameGUI;

/**
 *
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 16 Nov 2016
 *
 */
public final class TangramsClient implements Runnable {

	private enum Parameter implements Supplier<Option> {
		BROKER_HOST("h") {

			@Override
			public Option get() {
				return Option.builder(optName).longOpt("broker-host")
						.desc("The hostname (or IP address) of the IrisTK broker system to use.").hasArg()
						.argName("hostname").build();
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

		protected final String optName;

		private Parameter(final String optName) {
			this.optName = optName;
		}

	}

	private static final Logger LOGGER = LoggerFactory.getLogger(TangramsClient.class);

	private static final Options OPTIONS = createOptions();

	private static final Properties PROPS;

	static {
		try {
			PROPS = ClassProperties.load(TangramsClient.class);
		} catch (final IOException e) {
			throw new UncheckedIOException(e);
		}
	}

	public static void main(final String[] args) {
		final CommandLineParser parser = new DefaultParser();
		try {
			final CommandLine cl = parser.parse(OPTIONS, args);
			if (cl.hasOption(Parameter.HELP.optName)) {
				printHelp();
			} else {
				final String brokerHost = parseBrokerHost(cl);
				try {
					final int brokerPort = parseBrokerPort(cl);
					final TangramsClient client = new TangramsClient(PROPS.getProperty("broker.ticket"), brokerHost,
							brokerPort);
					client.run();

				} catch (final ParseException e) {
					System.out.println(String.format("Could not parse port: %s", e.getLocalizedMessage()));
					printHelp();
				}
			}
		} catch (final ParseException e) {
			System.out.println(String.format("An error occured while parsing the command-line arguments: %s", e));
			printHelp();
		}
	}

	private static ConnectionStatusFrame createConnectionStatusView(final String gameId, final Runnable shutdownHook) {
		final Map<ConnectionStatusFrame.Status, Consumer<ConnectionStatusFrame>> connectionStatusViewCloseHooks = new EnumMap<>(
				ConnectionStatusFrame.Status.class);
		connectionStatusViewCloseHooks.put(ConnectionStatusFrame.Status.CONNECTED, view -> {
			LOGGER.debug("Successfully connected.");
			// JOptionPane.showMessageDialog(view, "Connected!");
		});
		connectionStatusViewCloseHooks.put(ConnectionStatusFrame.Status.NOT_CONNECTED, view -> {
			LOGGER.debug("User aborted connection.");
			shutdownHook.run();
		});
		return new ConnectionStatusFrame(gameId, ConnectionStatusFrame.Status.NOT_CONNECTED,
				connectionStatusViewCloseHooks);
	}

	private static String createDefaultPlayerId() {
		final String username = Objects.toString(System.getProperty("user.name"), "Tangrams");
		return username + "_" + System.currentTimeMillis();
	}

	private static Options createOptions() {
		final Options result = new Options();
		Arrays.stream(Parameter.values()).map(Parameter::get).forEach(opt -> result.addOption(opt));
		return result;
	}

	private static String parseBrokerHost(final CommandLine cl) {
		final String result;
		{
			final String optName = Parameter.BROKER_HOST.optName;
			if (cl.hasOption(optName)) {
				result = cl.getOptionValue(optName);
				LOGGER.info("Using broker hostname \"{}\".", result);
			} else {
				result = PROPS.getProperty("broker.host");
				LOGGER.info("No broker hostname provided; Using default \"{}\".", result);
			}
		}
		return result;
	}

	private static int parseBrokerPort(final CommandLine cl) throws ParseException {
		final int result;
		{
			final Number paramValue = (Number) cl.getParsedOptionValue(Parameter.BROKER_PORT.optName);
			if (paramValue == null) {
				result = Integer.parseInt(PROPS.getProperty("broker.port"));
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
		formatter.printHelp(TangramsClient.class.getName(), OPTIONS);
	}

	private static String promptGameId(final Component dialogParentComponent) {
		String result = null;
		boolean parsedInput = false;
		do {
			String newGameName = JOptionPane.showInputDialog(dialogParentComponent, "Number of game to start:",
					"New game", JOptionPane.PLAIN_MESSAGE);
			// If the user cancels input, break out of the loop
			if (newGameName == null) {
				parsedInput = true;
			} else {
				// Pre-process and validate the input before assigning it as the
				// result
				newGameName = newGameName.trim();
				if (newGameName.isEmpty()) {
					JOptionPane.showMessageDialog(dialogParentComponent,
							"The input must contain more than just whitespaces.", "Error", JOptionPane.ERROR_MESSAGE);
				} else {
					result = newGameName;
					parsedInput = true;
				}
			}
		} while (!parsedInput);
		return result;
	}

	private static String promptPlayerId(final Component dialogParentComponent, final String defaultPlayerId) {
		String result = null;
		boolean parsedInput = false;
		do {
			final Object userInput = JOptionPane.showInputDialog(dialogParentComponent, "Player name:", "New game",
					JOptionPane.PLAIN_MESSAGE, null, null, defaultPlayerId);
			// If the user cancels input, break out of the loop
			if (userInput == null) {
				parsedInput = true;
			} else {
				// Pre-process and validate the input before assigning it as the
				// result
				String newPlayerId = (String) userInput;
				newPlayerId = newPlayerId.trim();
				if (newPlayerId.isEmpty()) {
					JOptionPane.showMessageDialog(dialogParentComponent,
							"The input must contain more than just whitespaces.", "Error", JOptionPane.ERROR_MESSAGE);
				} else {
					result = newPlayerId;
					parsedInput = true;
				}
			}
		} while (!parsedInput);
		return result;
	}

	private static void setLookAndFeel() {
		final String lookAndFeelClassName = UIManager.getSystemLookAndFeelClassName();
		try {
			UIManager.setLookAndFeel(lookAndFeelClassName);
		} catch (ClassNotFoundException | InstantiationException | IllegalAccessException
				| UnsupportedLookAndFeelException e) {
			LOGGER.error(String.format(
					"An error occurred while trying to use the look-and-feel class name \"%s\"; Giving up.",
					lookAndFeelClassName), e);
		}
	}

	private final String brokerHost;

	private final int brokerPort;

	private final String brokerTicket;

	public TangramsClient(final String brokerTicket, final String brokerHost, final int brokerPort) {
		this.brokerTicket = brokerTicket;
		this.brokerHost = brokerHost;
		this.brokerPort = brokerPort;
	}

	@Override
	public void run() {
		setLookAndFeel();
		final String playerId = promptPlayerId(null, createDefaultPlayerId());
		if (playerId == null) {
			LOGGER.info("Quitting application before creation of new game was completed.");
		} else {
			final String gameId = promptGameId(null);
			if (gameId == null) {
				LOGGER.info("Quitting application before creation of new game was completed.");
			} else {
				LOGGER.info("Starting IrisTK broker client for player ID \"{}\".", playerId);
				try {
					final IrisSystem system = new IrisSystem(getClass().getSimpleName(), new File("."));
					final Runnable irisSystemStopper = new IrisSystemStopper(system);
					// Try clause for ensuring that the IrisTK system gets
					// properly shut down in the case an exception occurs
					try {
						Runtime.getRuntime().addShutdownHook(new Thread(irisSystemStopper));
						final String logOutdirPath = PROPS.getProperty("log.outdir");
						final File logOutdir = Paths.get(logOutdirPath).toFile();
						final Date systemLoggingStartTime = new Date();
						final File logDir = new LogDirectoryFactory(logOutdir, systemLoggingStartTime).get();
						logDir.mkdirs();
						final RecordingManager recordingManager = new RecordingManager(() -> logDir);
						// Try clause for ensuring that the recorder gets
						// properly stopped even in the case an exception occurs
						try {
							final ConnectionStatusFrame connectionStatusView = createConnectionStatusView(gameId,
									irisSystemStopper);
							// Display the connection status view
							EventQueue.invokeLater(() -> {
								LOGGER.debug("Opening connection status view.");
								connectionStatusView.setLocationRelativeTo(null);
								connectionStatusView.setVisible(true);
							});
							// Try block to handle exceptions after creating the
							// connection status view in order to dispose of it
							try {
								system.connectToBroker(brokerTicket, brokerHost, brokerPort);
								{
									final LoggingModule loggingModule = new LoggingModule(logOutdir, NameFilter.ALL,
											false);
									system.addModule(loggingModule);
									loggingModule.startLogging(systemLoggingStartTime.getTime());
								}
								final GameManagementClientModule gameClientModule = new GameManagementClientModule(
										gameId, playerId, gameEnding -> {
											// Do nothing extra on game over
										}, gameState -> {
											LOGGER.info(
													"Handling game state data received from server for game \"{}\".",
													gameId);
											final LocalController<Integer> localController = gameState
													.getLocalController();
											EventQueue.invokeLater(new SuccessfulConnectionHook(connectionStatusView,
													recordingManager, playerId));
											// Get the position of the
											// connection
											// view
											// for
											// use for positioning the new views
											final Point viewLocation = connectionStatusView.getLocation();

											// Set up game GUI
											final String title = "Tangrams: " + playerId;
											final Model<Integer> model = localController.getModel();
											final int pieceCount = model.getOccupiedCoordinateCount();
											final Random rnd = new Random(gameState.getSeed());
											final Function<Integer, Image> pieceIdImageFactory = new PieceIdImageFactoryFactory(
													rnd).apply(pieceCount);
											final Runnable closeHook = () -> {
												LOGGER.info("Closing main window; Cleaning up background resources.");
												recordingManager.getStopper().run();
												irisSystemStopper.run();
											};
											EventQueue.invokeLater(new GameGUI<>(title, viewLocation, localController,
													gameState.getRemoteController(), gameState.getWinningModel(),
													pieceIdImageFactory,
													() -> logDir.toPath(), closeHook));

										});
								system.addModule(gameClientModule);
								system.sendStartSignal();

								gameClientModule.requestJoinGame();

							} catch (final Exception exAfterConnectionViewConst) {
								LOGGER.error(
										"An exception occurred sometime after creating the connection status view; Disposing of the view and re-throwing exception.");
								connectionStatusView.dispose();
								throw exAfterConnectionViewConst;
							}
						} catch (final Exception exAfterRecorderConst) {
							// NOTE: Finally doesn't work because of the threads
							// running in the background: A "finally" block will
							// kill the recorder right after exiting this method
							// even though the recording should continue
							LOGGER.error(String.format(
									"An exception occurred sometime after starting the %s instance; Cleaning up gracefully and re-throwing exception.",
									recordingManager.getClass().getSimpleName()), exAfterRecorderConst);
							recordingManager.getStopper().run();
							throw exAfterRecorderConst;
						}
					} catch (final Exception exAfterIrisTKConst) {
						// NOTE: Finally doesn't work because of the threads
						// running in the background: A "finally" block will
						// kill the IrisTK system right after exiting this
						// method
						// even though the recording should continue
						LOGGER.error(String.format(
								"An exception occurred sometime after constructing a(n) %s instance; Cleaning up gracefully and re-throwing exception.",
								system.getClass().getSimpleName()));
						irisSystemStopper.run();
						throw exAfterIrisTKConst;
					}
				} catch (final Exception runningException) {
					final RuntimeException wrapper = new RuntimeException(runningException);
					LOGGER.error(
							String.format("An exception occurred while running the client; Re-throwing as a(n) %s.",
									wrapper.getClass().getSimpleName()));
					throw wrapper;
				}
			}
		}
	}

}