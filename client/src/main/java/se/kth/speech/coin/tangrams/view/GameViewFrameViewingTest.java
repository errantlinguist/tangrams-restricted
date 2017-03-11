/*
 *  This file is part of client.
 *
 *  client is free software: you can redistribute it and/or modify
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
package se.kth.speech.coin.tangrams.view;

import java.awt.EventQueue;
import java.util.Arrays;
import java.util.List;
import java.util.Random;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import javax.swing.WindowConstants;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;
import se.kth.speech.coin.tangrams.content.RandomPieceImageDataFactory;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 7 Mar 2017
 *
 */
public final class GameViewFrameViewingTest implements Runnable {

	private enum Parameter implements Supplier<Option> {
		ALLOW_FAILED_PLACEMENTS("f") {

			@Override
			public Option get() {
				return Option.builder(optName).longOpt("allow-failed-placements")
						.desc("Allows the board to be displayed even if not all pieces could be successfully placed.")
						.build();
			}

		},
		GAME_ID("i") {

			@Override
			public Option get() {
				return Option.builder(optName).longOpt("game").desc("The ID of the game to display.").hasArg()
						.argName("id")
						// See http://stackoverflow.com/a/5955893/1391325
						.type(Number.class).required().build();
			}

		},
		MAX_PLACEMENT_RETRIES("r") {

			@Override
			public Option get() {
				return Option.builder(optName).longOpt("max-placement-retries")
						.desc("The number of times to re-try placing any image which could not be placed on the board the first time.")
						.hasArg().argName("count")
						// See http://stackoverflow.com/a/5955893/1391325
						.type(Number.class).build();
			}

		},
		IMG_PLACEMENT_COUNT("p") {

			@Override
			public Option get() {
				return Option.builder(optName).longOpt("img-placements")
						.desc("The number of images to place on the board").hasArg().argName("count")
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

	private static final Options OPTIONS = createOptions();

	private static final Logger LOGGER = LoggerFactory.getLogger(GameViewFrameViewingTest.class);

	private static int DEFAULT_MAX_PLACEMENT_RETRIES = 3;

	private static final int DEFAULT_IMG_PLACEMENT_COUNT = 20;

	public static void main(final String[] args) {
		try {
			final CommandLine cl = new DefaultParser().parse(OPTIONS, args);
			if (cl.hasOption(Parameter.HELP.optName)) {
				printHelp();
			} else {
				final long gameId = ((Number) cl.getParsedOptionValue(Parameter.GAME_ID.optName)).longValue();
				final int imgPlacementCount = parseImgPlacementCount(cl);
				final int maxPlacementRetries = parseMaxPlacementRetries(cl);
				final boolean allowFailedPlacements = cl.hasOption(Parameter.ALLOW_FAILED_PLACEMENTS.optName);
				LOGGER.info("Creating view for game \"{}\".", gameId);
				final Random rnd = new Random(gameId);
				final GameViewFrameViewingTest testInstance = new GameViewFrameViewingTest(rnd, imgPlacementCount,
						maxPlacementRetries, allowFailedPlacements);
				EventQueue.invokeLater(testInstance);
			}
		} catch (final ParseException e) {
			System.out.println(String.format("An error occured while parsing the command-line arguments: %s", e));
			printHelp();
		}
	}

	private static Options createOptions() {
		final Options result = new Options();
		Arrays.stream(Parameter.values()).map(Parameter::get).forEach(opt -> result.addOption(opt));
		return result;
	}

	private static int parseImgPlacementCount(final CommandLine cl) throws ParseException {
		final int result;
		final Number parsedVal = (Number) cl.getParsedOptionValue(Parameter.IMG_PLACEMENT_COUNT.optName);
		if (parsedVal == null) {
			result = DEFAULT_IMG_PLACEMENT_COUNT;
			LOGGER.info("No max image placement count supplied; Using default ({}).", result);
		} else {
			result = parsedVal.intValue();
			LOGGER.info("Set max image placement count to {}.", result);
		}
		return result;
	}

	private static int parseMaxPlacementRetries(final CommandLine cl) throws ParseException {
		final int result;
		final Number parsedVal = (Number) cl.getParsedOptionValue(Parameter.MAX_PLACEMENT_RETRIES.optName);
		if (parsedVal == null) {
			result = DEFAULT_MAX_PLACEMENT_RETRIES;
			LOGGER.info("No max placement retry count supplied; Using default ({}).", result);
		} else {
			result = parsedVal.intValue();
			LOGGER.info("Set max placement retry count to {}.", result);
		}
		return result;
	}

	private static void printHelp() {
		final HelpFormatter formatter = new HelpFormatter();
		formatter.printHelp(GameViewFrameViewingTest.class.getName(), OPTIONS);
	}

	private final boolean allowFailedPlacements;

	private final int maxPlacementRetriesPerImg;

	private final Random rnd;

	private final int imgPlacementCount;

	public GameViewFrameViewingTest(final Random rnd, final int imgPlacementCount, final int maxPlacementRetriesPerImg,
			final boolean allowFailedPlacements) {
		this.rnd = rnd;
		this.imgPlacementCount = imgPlacementCount;
		this.maxPlacementRetriesPerImg = maxPlacementRetriesPerImg;
		this.allowFailedPlacements = allowFailedPlacements;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Runnable#run()
	 */
	@Override
	public void run() {
		final RandomPieceImageDataFactory imgDataFactory = new RandomPieceImageDataFactory();
		final List<ImageVisualizationInfo> imgVisualizationInfoData = imgDataFactory.apply(rnd)
				.collect(Collectors.toList());
		final GameBoardPanel gameBoardPanel = new GameBoardPanel(imgVisualizationInfoData, rnd, imgPlacementCount,
				maxPlacementRetriesPerImg, allowFailedPlacements);
		final GameViewFrame frame = new GameViewFrame(gameBoardPanel, rnd);
		frame.pack();
		frame.setLocationByPlatform(true);
		frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		frame.setVisible(true);
	}

}
