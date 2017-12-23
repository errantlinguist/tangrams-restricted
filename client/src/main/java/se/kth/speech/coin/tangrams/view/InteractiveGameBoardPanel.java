/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.client.
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

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Image;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.concurrent.ExecutorService;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import javax.swing.JOptionPane;
import javax.swing.SwingWorker;
import javax.swing.event.AncestorEvent;
import javax.swing.event.AncestorListener;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.github.errantlinguist.ClassProperties;
import com.google.common.collect.Maps;

import se.kth.speech.SpatialMatrix;
import se.kth.speech.SpatialRegion;
import se.kth.speech.awt.DisablingMouseAdapter;
import se.kth.speech.coin.tangrams.game.Controller;
import se.kth.speech.coin.tangrams.game.GameplayController;
import se.kth.speech.coin.tangrams.game.PlayerRole;
import se.kth.speech.coin.tangrams.game.Turn;
import se.kth.speech.coin.tangrams.iristk.events.Move;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 2 Mar 2017
 *
 */
public final class InteractiveGameBoardPanel extends AbstractGameBoardPanel implements Controller.Listener {

	public enum Property {
		MOVE_SUBMISSION_WAIT_TIME("moveSubmissionWaitTime");

		private final String propName;

		private Property(final String propName) {
			this.propName = propName;
		}

		public String getPropName() {
			return propName;
		}

	}

	private static class DelegatingSwingWorker extends SwingWorker<Void, Void> {

		private final Runnable delegate;

		private DelegatingSwingWorker(final Runnable delegate) {
			this.delegate = delegate;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see javax.swing.SwingWorker#doInBackground()
		 */
		@Override
		protected Void doInBackground() {
			delegate.run();
			return null;
		}
	}

	private class SelectingMouseAdapter extends MouseAdapter {
		@Override
		public void mouseClicked(final MouseEvent e) {
			final int x = e.getX();
			final int y = e.getY();
			final Component foundComponent = findComponentAt(e.getX(), e.getY());
			if (foundComponent == InteractiveGameBoardPanel.this) {
				final Entry<Integer, SpatialRegion> biggestPieceRegionUnderSelection = findBiggestPieceRegionUnderSelection(
						x, y);
				if (biggestPieceRegionUnderSelection == null) {
					LOGGER.info("Nothing to select.");
				} else {
					LOGGER.info("Selected {}.", biggestPieceRegionUnderSelection);
					toggleHighlightedRegion(biggestPieceRegionUnderSelection.getValue());
					logScreenshotSelectedPiece(biggestPieceRegionUnderSelection.getKey());
					backgroundJobService.execute(new DelegatingSwingWorker(
							() -> controller.submitSelection(biggestPieceRegionUnderSelection)));
				}

			} else {
				LOGGER.info("The user clicked on an instance of \"{}\", which cannot be selected.",
						foundComponent.getClass());
			}
		}

	}

	private static final int IMG_SIDE_PADDING;

	private static final int IMG_TOTAL_PADDING;

	private static final Logger LOGGER = LoggerFactory.getLogger(InteractiveGameBoardPanel.class);

	private static final int MIN_GRID_SQUARE_LENGTH;

	private static final int MOVE_SUBMISSION_WAIT_TIME_MILLS;

	/**
	 *
	 */
	private static final long serialVersionUID = 6258829324465894025L;

	private static final ThreadLocal<SimpleDateFormat> TIME_FORMAT = new ThreadLocal<SimpleDateFormat>() {

		/*
		 * (non-Javadoc)
		 *
		 * @see java.lang.ThreadLocal#initialValue()
		 */
		@Override
		protected SimpleDateFormat initialValue() {
			return new SimpleDateFormat("HHmmssSSS");
		}

	};

	static {
		try {
			final Properties props = ClassProperties.load(InteractiveGameBoardPanel.class);
			MOVE_SUBMISSION_WAIT_TIME_MILLS = Integer
					.parseInt(props.getProperty(Property.MOVE_SUBMISSION_WAIT_TIME.getPropName()));
		} catch (final IOException e) {
			throw new UncheckedIOException(e);
		}
	}

	static {
		final int highlightingStrokeWidth = 4;
		final int spaceBetweenImgAndHighlight = 5;
		IMG_SIDE_PADDING = highlightingStrokeWidth + spaceBetweenImgAndHighlight;
		IMG_TOTAL_PADDING = IMG_SIDE_PADDING * 2;
		MIN_GRID_SQUARE_LENGTH = 10 + IMG_TOTAL_PADDING;
	}

	private static IntStream createMinimumDimLengths(final int[] dims) {
		return Arrays.stream(dims).map(dim -> dim * MIN_GRID_SQUARE_LENGTH);
	}

	private final ExecutorService backgroundJobService;

	private final GameplayController controller;

	private final BiConsumer<? super InteractiveGameBoardPanel, ? super Turn> localTurnCompletionViewLogger;

	private final SpatialMatrix<Integer> posMatrix;

	private final DisablingMouseAdapter selectingMouseListener;

	private final BiConsumer<Component, String> viewLogger;

	InteractiveGameBoardPanel(final SpatialMatrix<Integer> posMatrix,
			final Function<? super Integer, ? extends Image> pieceIdImageFactory, final GameplayController controller,
			final Color highlightColor, final BiConsumer<? super Component, ? super String> screenshotLogger,
			final ExecutorService backgroundJobService) {
		this(posMatrix, pieceIdImageFactory, controller, highlightColor, screenshotLogger, backgroundJobService, false);
	}

	InteractiveGameBoardPanel(final SpatialMatrix<Integer> posMatrix,
			final Function<? super Integer, ? extends Image> pieceIdImageFactory, final GameplayController controller,
			final Color highlightColor, final BiConsumer<? super Component, ? super String> screenshotLogger,
			final ExecutorService backgroundJobService, final boolean analysisEnabled) {
		super(posMatrix, pieceIdImageFactory, highlightColor, analysisEnabled);
		this.posMatrix = posMatrix;
		this.controller = controller;
		this.backgroundJobService = backgroundJobService;
		controller.getListeners().add(this);

		final Supplier<SimpleDateFormat> dateFormatSupplier = TIME_FORMAT::get;
		localTurnCompletionViewLogger = new TimestampingCompletedTurnLogger(screenshotLogger, dateFormatSupplier);
		viewLogger = new TimestampingScreenshotLogger(screenshotLogger, dateFormatSupplier);
		// https://stackoverflow.com/a/5777914/1391325
		addAncestorListener(new AncestorListener() {

			@Override
			public void ancestorAdded(final AncestorEvent event) {
				final String filenamePrefix = "round-1-";
				viewLogger.accept(InteractiveGameBoardPanel.this, filenamePrefix);
				InteractiveGameBoardPanel.this.removeAncestorListener(this);
			}

			@Override
			public void ancestorMoved(final AncestorEvent event) {
				// Do nothing
			}

			@Override
			public void ancestorRemoved(final AncestorEvent event) {
				// Do nothing
			}

		});
		selectingMouseListener = new DisablingMouseAdapter(new SelectingMouseAdapter());
		updateMouseListener(controller.getRole());
		addDisablingMouseListener(selectingMouseListener);

		{
			final int[] minSizeDims = createMinimumDimLengths(posMatrix.getDimensions()).toArray();
			// NOTE: "rows" in the matrix go top-bottom and "cols" go
			// left-right
			final Dimension minSize = new Dimension(minSizeDims[1], minSizeDims[0]);
			LOGGER.debug("Setting minimum component size to {}.", minSize);
			setMinimumSize(minSize);
		}
	}

	@Override
	public void updateNextMove(final Move move) {
		LOGGER.debug("Observed event representing the subbmission of a move by a player.");
	}

	@Override
	public void updatePlayerJoined(final String joinedPlayerId, final long time) {
		LOGGER.debug("Observed event representing the joining of a player.");
	}

	@Override
	public void updatePlayerRole(final PlayerRole newRole) {
		LOGGER.debug("Observed event representing a change in player role.");
		updateMouseListener(newRole);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see se.kth.speech.coin.tangrams.iristk.Controller.Listener#
	 * updatePlayerSelection(se.kth.speech.coin.tangrams.iristk.events.
	 * Selection)
	 */
	@Override
	public void updatePlayerSelection(final Integer pieceId, final SpatialRegion region) {
		LOGGER.debug("Observed event representing a user selection.");
		final boolean isSelectionCorrect = controller.isSelectionCorrect();
		if (isSelectionCorrect) {
			JOptionPane.showMessageDialog(this, "The other player selected the right piece!", "Good selection",
					JOptionPane.INFORMATION_MESSAGE);
			backgroundJobService.execute(new DelegatingSwingWorker(controller::submitTurnComplete));

			getHighlightedRegions().clear();
			repaint();
		} else {
			JOptionPane.showMessageDialog(this, "The other player selected the wrong piece!", "Bad selection",
					JOptionPane.ERROR_MESSAGE);
			backgroundJobService.execute(new DelegatingSwingWorker(controller::submitSelectionRejection));
		}
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * se.kth.speech.coin.tangrams.game.Controller.Listener#updateScore(int)
	 */
	@Override
	public void updateScore(final int score) {
		LOGGER.debug("Notified of new score.");
	}

	@Override
	public void updateSelectionRejected(final Integer pieceId, final SpatialRegion region) {
		LOGGER.debug("Observed event representing the rejection of the last selection.");
		final boolean wasRemoved = getHighlightedRegions().remove(region);
		assert wasRemoved;
		repaint();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see se.kth.speech.coin.tangrams.iristk.Controller.Listener#
	 * updateTurnCompletion(se.kth.speech.coin.tangrams.iristk.events.Turn)
	 */
	@Override
	public void updateTurnCompleted(final Turn turn) {
		final String turnPlayerId = turn.getPlayerId();
		LOGGER.debug("Observed event representing a turn completed by \"{}\".", turnPlayerId);
		final SpatialRegion moveSource = turn.getMove().getKey();
		final boolean wasRemoved = getHighlightedRegions().remove(moveSource);
		assert wasRemoved;
		repaint();
		localTurnCompletionViewLogger.accept(this, turn);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * se.kth.speech.coin.tangrams.game.Controller.Listener#updateMoveCount(int)
	 */
	@Override
	public void updateTurnCount(final int newCount) {
		LOGGER.debug("Notified of new turn count.");
	}

	private void addDisablingMouseListener(final DisablingMouseAdapter mouseListener) {
		addMouseListener(mouseListener);
		addMouseMotionListener(mouseListener);
	}

	private Entry<Integer, SpatialRegion> findBiggestPieceRegionUnderSelection(final int x, final int y) {
		final int rowIdx = getRowIdx(y);
		final int colIdx = getColIdx(x);
		final Stream<Entry<SpatialRegion, Integer>> selectedRegionPieceIds = posMatrix
				.getIntersectedElements(rowIdx, colIdx)
				.filter(selectedRegionPieceId -> selectedRegionPieceId.getValue() != null);
		final Map<Integer, SpatialRegion> pieceBiggestRegions = Maps.newHashMapWithExpectedSize(1);
		selectedRegionPieceIds.forEach(selectedRegionPieceId -> {
			final SpatialRegion region = selectedRegionPieceId.getKey();
			final Integer pieceId = selectedRegionPieceId.getValue();
			pieceBiggestRegions.compute(pieceId, (k, oldVal) -> {
				SpatialRegion newVal;
				if (oldVal == null) {
					newVal = region;
				} else {
					if (oldVal.getGridArea() < region.getGridArea()) {
						newVal = region;
					} else {
						newVal = oldVal;
					}
				}
				return newVal;
			});
		});

		final Entry<Integer, SpatialRegion> result;
		if (pieceBiggestRegions.isEmpty()) {
			result = null;
		} else {
			final List<Entry<Integer, SpatialRegion>> pieceBiggestRegionsDescendingSize = new ArrayList<>(
					pieceBiggestRegions.entrySet());
			final Comparator<Entry<Integer, SpatialRegion>> sizeComparator = Comparator
					.comparing(piece -> piece.getValue().getGridArea());
			Collections.sort(pieceBiggestRegionsDescendingSize, sizeComparator.reversed());
			result = pieceBiggestRegionsDescendingSize.iterator().next();
		}
		return result;
	}

	private int getColIdx(final int x) {
		final int colWidth = getGridColWidth();
		return x / colWidth;
	}

	private int getGridColWidth() {
		final int[] matrixDims = posMatrix.getDimensions();
		return getWidth() / matrixDims[1];
	}

	private int getGridRowHeight() {
		final int[] matrixDims = posMatrix.getDimensions();
		return getHeight() / matrixDims[0];
	}

	private int getRowIdx(final int y) {
		final int rowHeight = getGridRowHeight();
		return y / rowHeight;
	}

	private void logScreenshotSelectedPiece(final Integer pieceId) {
		viewLogger.accept(this, "selection-piece-id" + pieceId + "-");
	}

	private void updateMouseListener(final PlayerRole role) {
		final boolean canSelect = role.equals(PlayerRole.SELECTING);
		selectingMouseListener.setEnabled(canSelect);
	}

	@Override
	protected void notifyNextMove(final SpatialRegion source, final SpatialRegion target, final Integer pieceId) {
		LOGGER.debug("Notified of continue event.");
		final PlayerRole role = controller.getRole();
		switch (role) {
		case MOVE_SUBMISSION: {
			backgroundJobService
					.execute(new DelegatingSwingWorker(() -> controller.submitNextMove(source, target, pieceId)));
			backgroundJobService.execute(new SwingWorker<Void, Void>() {

				@Override
				protected Void doInBackground() throws InterruptedException {
					Thread.sleep(MOVE_SUBMISSION_WAIT_TIME_MILLS);
					toggleHighlightedRegion(source);
					return null;
				}

			});

			break;
		}
		default: {
			throw new IllegalStateException(String.format("Cannot continue while in role %s.", role));
		}

		}
	}

}
