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
import java.util.List;
import java.util.Random;
import java.util.stream.Collectors;

import javax.swing.WindowConstants;

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

	private static final Logger LOGGER = LoggerFactory.getLogger(GameViewFrameViewingTest.class);

	public static void main(final String[] args) {
		if (args.length < 1) {
			System.err.println(String.format("Usage: %s <gameId> [maxPlacementRetriesPerImg]",
					GameViewFrameViewingTest.class.getName()));
			System.exit(64);
		} else {
			final String gameId = args[0];
			final int maxPlacementRetriesPerImg = args.length > 1 ? Integer.parseInt(args[1]) : 3;
			LOGGER.info("Creating view for game \"{}\".", gameId);
			final Random rnd = new Random(Long.parseLong(gameId));
			final GameViewFrameViewingTest testInstance = new GameViewFrameViewingTest(rnd, maxPlacementRetriesPerImg,
					true);
			EventQueue.invokeLater(testInstance);
		}
	}

	private final boolean allowFailedPlacements;

	private final int maxPlacementRetriesPerImg;

	private final Random rnd;

	public GameViewFrameViewingTest(final Random rnd, final int maxPlacementRetriesPerImg,
			final boolean allowFailedPlacements) {
		this.rnd = rnd;
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
		final GameBoardPanel gameBoardPanel = new GameBoardPanel(imgVisualizationInfoData, rnd,
				maxPlacementRetriesPerImg, allowFailedPlacements);
		final GameViewFrame frame = new GameViewFrame(gameBoardPanel, rnd);
		frame.pack();
		frame.setLocationByPlatform(true);
		frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		frame.setVisible(true);
	}

}
