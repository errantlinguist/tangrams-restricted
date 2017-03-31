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

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.RenderingHints;
import java.awt.Stroke;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.function.BiConsumer;
import java.util.function.IntFunction;
import java.util.function.Supplier;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.SwingWorker;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Maps;
import com.google.common.collect.Sets;

import se.kth.speech.Matrix;
import se.kth.speech.SpatialMatrix;
import se.kth.speech.SpatialRegion;
import se.kth.speech.awt.ComponentResizedEventListener;
import se.kth.speech.awt.DisablingMouseAdapter;
import se.kth.speech.coin.tangrams.game.Controller;
import se.kth.speech.coin.tangrams.game.PlayerRole;
import se.kth.speech.coin.tangrams.game.Turn;
import se.kth.speech.coin.tangrams.iristk.events.Move;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 2 Mar 2017
 *
 */
final class GameBoardPanel extends JPanel implements Controller.Listener {

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
			if (foundComponent == GameBoardPanel.this) {
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

	private static final Stroke GRID_STROKE = new BasicStroke(1.0f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_BEVEL, 1.0f,
			new float[] { 1.0f }, 0);

	private static final Stroke HIGHLIGHTING_STROKE;

	private static final int IMG_SIDE_PADDING;

	private static final int IMG_TOTAL_PADDING;

	private static final Logger LOGGER = LoggerFactory.getLogger(GameBoardPanel.class);

	private static final int MIN_GRID_SQUARE_LENGTH;

	private static final RenderingHints RENDERING_HINTS = new RenderingHints(RenderingHints.KEY_RENDERING,
			RenderingHints.VALUE_RENDER_QUALITY);

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
		final int highlightingStrokeWidth = 4;
		HIGHLIGHTING_STROKE = new BasicStroke(highlightingStrokeWidth, BasicStroke.CAP_SQUARE, BasicStroke.JOIN_MITER,
				10.0f, new float[] { 5.0f, 5.0f }, 0);
		final int spaceBetweenImgAndHighlight = 5;
		IMG_SIDE_PADDING = highlightingStrokeWidth + spaceBetweenImgAndHighlight;
		IMG_TOTAL_PADDING = IMG_SIDE_PADDING * 2;
		MIN_GRID_SQUARE_LENGTH = 10 + IMG_TOTAL_PADDING;
	}

	private static int[] createComponentCoordSizeArray(final SpatialRegion region, final int colWidth,
			final int rowHeight) {
		// "y" is matrix columns, thus left-right
		final int regionGridColCount = region.getLengthY();
		LOGGER.debug("Region occupies {} grid column(s).", regionGridColCount);
		final int width = colWidth * regionGridColCount;
		// "x" is matrix rows, thus top-to-bottom
		final int regionGridRowCount = region.getLengthX();
		LOGGER.debug("Region occupies {} grid row(s).", regionGridRowCount);
		final int height = rowHeight * regionGridRowCount;
		return new int[] { width, height };
	}

	private static int[] createComponentCoordStartIdxArray(final SpatialRegion region, final int colWidth,
			final int rowHeight) {
		// NOTE: occupied region is denoted in matrix rows*columns
		// "y" is matrix columns, thus left-to-right
		final int startX = colWidth * region.getYLowerBound();
		// "x" is matrix rows, thus top-to-bottom
		final int startY = rowHeight * region.getXLowerBound();
		return new int[] { startX, startY };
	}

	private static IntStream createMinimumDimLengths(final int[] dims) {
		return Arrays.stream(dims).map(dim -> dim * MIN_GRID_SQUARE_LENGTH);
	}

	// private static void drawRegionHighlights(final Graphics g, final
	// SpatialRegion region, final int colWidth,
	// final int rowHeight) {
	// final int[] startIdxs = createComponentCoordStartIdxArray(region,
	// colWidth, rowHeight);
	// final int[] size = createComponentCoordSizeArray(region, colWidth,
	// rowHeight);
	// g.drawRect(startIdxs[0], startIdxs[1], size[0], size[1]);
	// }

	private final boolean analysisEnabled;

	private final ExecutorService backgroundJobService;

	private final Map<SpatialRegion, int[]> compCoordSizes;

	private final Map<SpatialRegion, int[]> compCoordStartIdxs;

	private final Controller controller;

	private final Color highlightColor;

	/**
	 * At most one region should be highlighted at a time per player
	 */
	private final Set<SpatialRegion> highlightedRegions = Sets.newHashSetWithExpectedSize(1);

	private final Map<Image, Image> imgsScaledToGridSize;

	private final BiConsumer<? super GameBoardPanel, ? super Turn> localTurnCompletionViewLogger;

	private final IntFunction<? extends Image> pieceIdImageFactory;

	private final SpatialMatrix<Integer> posMatrix;

	private final DisablingMouseAdapter selectingMouseListener;

	private final BiConsumer<Component, String> viewLogger;

	GameBoardPanel(final SpatialMatrix<Integer> posMatrix,
			final IntFunction<? extends Image> pieceIdImageFactory, final Controller controller,
			final Color highlightColor, final BiConsumer<? super Component, ? super String> screenshotLogger,
			final ExecutorService backgroundJobService) {
		this(posMatrix, pieceIdImageFactory, controller, highlightColor, screenshotLogger, backgroundJobService, false);
	}

	GameBoardPanel(final SpatialMatrix<Integer> posMatrix,
			final IntFunction<? extends Image> pieceIdImageFactory, final Controller controller,
			final Color highlightColor, final BiConsumer<? super Component, ? super String> screenshotLogger,
			final ExecutorService backgroundJobService, final boolean analysisEnabled) {
		this.posMatrix = posMatrix;
		this.pieceIdImageFactory = pieceIdImageFactory;
		this.controller = controller;
		this.highlightColor = highlightColor;
		this.backgroundJobService = backgroundJobService;
		controller.getListeners().add(this);

		final Supplier<SimpleDateFormat> dateFormatSupplier = TIME_FORMAT::get;
		localTurnCompletionViewLogger = new TimestampingCompletedTurnLogger(screenshotLogger, dateFormatSupplier);
		viewLogger = new TimestampingScreenshotLogger(screenshotLogger, dateFormatSupplier);
		this.analysisEnabled = analysisEnabled;
		selectingMouseListener = new DisablingMouseAdapter(new SelectingMouseAdapter());
		updateMouseListener(controller.getRole());
		addDisablingMouseListener(selectingMouseListener);

		// Caching of elements which are dependent on the (current) size of this
		// component
		final int uniqueRegionCount = posMatrix.getElementPlacements().getMinimalRegions().size();
		compCoordStartIdxs = Maps.newHashMapWithExpectedSize(uniqueRegionCount);
		addComponentListener(new ComponentResizedEventListener(compCoordStartIdxs::clear));
		compCoordSizes = Maps.newHashMapWithExpectedSize(uniqueRegionCount);
		addComponentListener(new ComponentResizedEventListener(compCoordSizes::clear));
		imgsScaledToGridSize = Maps.newHashMapWithExpectedSize(posMatrix.getUniqueElementCount());
		addComponentListener(new ComponentResizedEventListener(imgsScaledToGridSize::clear));

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
	public void paintComponent(final Graphics g) {
		super.paintComponent(g);
		((Graphics2D) g).addRenderingHints(RENDERING_HINTS);
		// http://stackoverflow.com/a/21989406/1391325
		{
			final Graphics2D gridDrawingG = (Graphics2D) g.create();
			// set the stroke of the copy, not the original
			gridDrawingG.setStroke(GRID_STROKE);
			try {
				drawBorder(gridDrawingG);
				// Draw a grid (for debugging/devel)
				if (analysisEnabled) {
					drawGrid(gridDrawingG);
					drawPieceIds(g);
				}
			} finally {
				gridDrawingG.dispose();
			}
		}

		{
			final Graphics2D regionHighlightingG = createRegionHighlightDrawingGraphics(g);
			regionHighlightingG.setStroke(HIGHLIGHTING_STROKE);
			regionHighlightingG.setColor(highlightColor);
			try {
				drawRegionHighlights(regionHighlightingG);
			} finally {
				regionHighlightingG.dispose();
			}
		}
		drawPieceImages(g);
	}

	// private void clearRegionHighlights(final Graphics g, final SpatialRegion
	// region) {
	// final int colWidth = getGridColWidth();
	// final int rowHeight = getGridRowHeight();
	// final int[] startIdxs = createComponentCoordStartIdxArray(region,
	// colWidth, rowHeight);
	// final int[] size = createComponentCoordSizeArray(region, colWidth,
	// rowHeight);
	// g.clearRect(startIdxs[0], startIdxs[1], size[0], size[1]);
	// }

	// private Graphics2D createRegionHighlightClearingGraphics(final Graphics
	// g) {
	// final Graphics2D result = (Graphics2D) g.create();
	// result.setStroke(new BasicStroke(REGION_HIGHLIGHT_STROKE_WIDTH));
	// result.setColor(getBackground());
	// return result;
	// }

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

			highlightedRegions.clear();
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
		final boolean wasRemoved = highlightedRegions.remove(region);
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
		final boolean wasRemoved = highlightedRegions.remove(moveSource);
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

	private Graphics2D createRegionHighlightDrawingGraphics(final Graphics g) {
		final Graphics2D result = (Graphics2D) g.create();
		result.setStroke(HIGHLIGHTING_STROKE);
		result.setColor(Color.MAGENTA);
		return result;
	}

	private void drawBorder(final Graphics2D gridDrawingG) {
		final Dimension size = getSize();
		gridDrawingG.drawRect(0, 0, size.width - 1, size.height - 1);
	}

	private void drawGrid(final Graphics2D gridDrawingG) {
		LOGGER.debug("Drawing grid.");
		final Matrix<Integer> posMatrix = this.posMatrix.getPositionMatrix();
		final int[] matrixDims = posMatrix.getDimensions();
		try {
			// Row lines
			final int rowHeight = getGridRowHeight();
			int nextRowY = 0;
			for (final ListIterator<List<Integer>> matrixRowIter = posMatrix.rowIterator(); matrixRowIter
					.hasNext(); matrixRowIter.next()) {
				gridDrawingG.drawLine(0, nextRowY, getWidth(), nextRowY);
				nextRowY += rowHeight;
			}
			LOGGER.debug("Finished drawing row lines.");

			// Column lines
			final int colWidth = getGridColWidth();
			int nextColX = 0;
			for (int colIdx = 0; colIdx < matrixDims[1]; ++colIdx) {
				gridDrawingG.drawLine(nextColX, 0, nextColX, getHeight());
				nextColX += colWidth;
			}
			LOGGER.debug("Finished drawing column lines.");
		} finally {
			gridDrawingG.dispose();
		}
		LOGGER.debug("Finished drawing grid.");
	}

	private void drawPieceIds(final Graphics g) {
		LOGGER.debug("Drawing piece IDs.");
		final int colWidth = getGridColWidth();
		final int rowHeight = getGridRowHeight();
		final FontMetrics fm = g.getFontMetrics();
		final int pieceTextHeight = fm.getAscent();
		final int textYOffset = rowHeight - pieceTextHeight;
		int nextRowY = 0;
		for (final ListIterator<List<Integer>> matrixRowIter = posMatrix.getPositionMatrix()
				.rowIterator(); matrixRowIter.hasNext();) {
			final List<Integer> matrixRow = matrixRowIter.next();
			int nextColX = 0;
			for (final ListIterator<Integer> matrixRowCellIter = matrixRow.listIterator(); matrixRowCellIter
					.hasNext();) {
				final Integer pieceId = matrixRowCellIter.next();
				if (pieceId != null) {
					final String pieceText = pieceId.toString();
					final int pieceTextWidth = fm.stringWidth(pieceText);
					final int textXOffset = (colWidth - pieceTextWidth) / 2;
					g.drawString(pieceText, nextColX + textXOffset, nextRowY + textYOffset);
				}
				nextColX += colWidth;
			}
			nextRowY += rowHeight;
		}
		LOGGER.debug("Finished drawing piece IDs.");
	}

	private void drawPieceImages(final Graphics g) {
		final int colWidth = getGridColWidth();
		final int rowHeight = getGridRowHeight();

		posMatrix.getElementPlacements().getMinimalRegionElements().entries().parallelStream()
				.forEach(piecePlacement -> {
					final SpatialRegion region = piecePlacement.getKey();
					final Integer pieceId = piecePlacement.getValue();
					final Image initialImg = pieceIdImageFactory.apply(pieceId);
					final Image scaledImg = fetchImageScaledToGridSize(initialImg, region, colWidth, rowHeight);
					final int[] startIdxs = fetchComponentCoordStartIdxArray(region, colWidth, rowHeight);
					g.drawImage(scaledImg, startIdxs[0] + IMG_SIDE_PADDING, startIdxs[1] + IMG_SIDE_PADDING, null);
				});
	}

	private void drawRegionHighlights(final Graphics g) {
		final int colWidth = getGridColWidth();
		final int rowHeight = getGridRowHeight();
		highlightedRegions.forEach(region -> {
			drawRegionHighlights(g, region, colWidth, rowHeight);
		});
	}

	private void drawRegionHighlights(final Graphics g, final SpatialRegion region) {
		final int colWidth = getGridColWidth();
		final int rowHeight = getGridRowHeight();
		drawRegionHighlights(g, region, colWidth, rowHeight);
	}

	private void drawRegionHighlights(final Graphics g, final SpatialRegion region, final int colWidth,
			final int rowHeight) {
		final int[] startIdxs = fetchComponentCoordStartIdxArray(region, colWidth, rowHeight);
		final int[] size = fetchComponentCoordSizeArray(region, colWidth, rowHeight);
		g.drawRect(startIdxs[0], startIdxs[1], size[0], size[1]);
	}

	private int[] fetchComponentCoordSizeArray(final SpatialRegion region, final int colWidth, final int rowHeight) {
		return compCoordSizes.computeIfAbsent(region, k -> createComponentCoordSizeArray(k, colWidth, rowHeight));
	}

	private int[] fetchComponentCoordStartIdxArray(final SpatialRegion region, final int colWidth,
			final int rowHeight) {
		return compCoordStartIdxs.computeIfAbsent(region,
				k -> createComponentCoordStartIdxArray(k, colWidth, rowHeight));
	}

	private Image fetchImageScaledToGridSize(final Image img, final SpatialRegion occupiedGridRegion,
			final int colWidth, final int rowHeight) {
		return imgsScaledToGridSize.computeIfAbsent(img,
				k -> scaleImageToGridSize(k, occupiedGridRegion, colWidth, rowHeight));
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

	private Image scaleImageToGridSize(final Image img, final SpatialRegion occupiedGridRegion, final int colWidth,
			final int rowHeight) {
		final int[] size = fetchComponentCoordSizeArray(occupiedGridRegion, colWidth, rowHeight);
		return img.getScaledInstance(Math.max(MIN_GRID_SQUARE_LENGTH, size[0] - IMG_TOTAL_PADDING),
				Math.max(MIN_GRID_SQUARE_LENGTH, size[1] - IMG_TOTAL_PADDING), Image.SCALE_SMOOTH);
	}

	private boolean toggleHighlightedRegion(final SpatialRegion region) {
		boolean result;

		if (result = highlightedRegions.add(region)) {
			final Graphics2D regionHighlightingG = createRegionHighlightDrawingGraphics(getGraphics());
			try {
				drawRegionHighlights(regionHighlightingG, region);
			} finally {
				regionHighlightingG.dispose();
			}
		} else if (result = highlightedRegions.remove(region)) {
			repaint();
			// final Graphics2D regionHighlightingG =
			// createRegionHighlightClearingGraphics(getGraphics());
			// try {
			// clearRegionHighlights(regionHighlightingG, region);
			// } finally {
			// regionHighlightingG.dispose();
			// }

		}

		return result;
	}

	private void updateMouseListener(final PlayerRole role) {
		final boolean canSelect = role.equals(PlayerRole.SELECTING);
		selectingMouseListener.setEnabled(canSelect);
	}

	/**
	 *
	 */
	void notifyNextMove(final SpatialRegion source, final SpatialRegion target, final Integer pieceId) {
		LOGGER.debug("Notified of continue event.");
		final PlayerRole role = controller.getRole();
		switch (role) {
		case MOVE_SUBMISSION: {
			toggleHighlightedRegion(source);
			backgroundJobService
					.execute(new DelegatingSwingWorker(() -> controller.submitNextMove(source, target, pieceId)));
			break;
		}
		default: {
			throw new IllegalStateException(String.format("Cannot continue while in role %s.", role));
		}

		}
	}

}
