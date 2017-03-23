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
import java.awt.Stroke;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Random;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import javax.swing.JOptionPane;
import javax.swing.JPanel;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Maps;
import com.google.common.collect.Sets;

import se.kth.speech.Matrix;
import se.kth.speech.MutablePair;
import se.kth.speech.RandomCollections;
import se.kth.speech.SpatialMap;
import se.kth.speech.SpatialMatrix;
import se.kth.speech.SpatialRegion;
import se.kth.speech.awt.DisablingMouseAdapter;
import se.kth.speech.coin.tangrams.game.Controller;
import se.kth.speech.coin.tangrams.game.PlayerRole;
import se.kth.speech.coin.tangrams.game.Turn;
import se.kth.speech.coin.tangrams.iristk.events.Move;
import se.kth.speech.coin.tangrams.iristk.events.Selection;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 2 Mar 2017
 *
 */
final class GameBoardPanel extends JPanel implements Controller.Listener {

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
					controller.submitSelection(biggestPieceRegionUnderSelection);
				}

			} else {
				LOGGER.info("The user clicked on an instance of \"{}\", which cannot be selected.",
						foundComponent.getClass());
			}
		}

	}

	private static final Stroke GRID_STROKE = new BasicStroke(1, BasicStroke.CAP_BUTT, BasicStroke.JOIN_BEVEL, 0,
			new float[] { 1 }, 0);

	private static final int IMG_PADDING;

	private static final int IMG_SCALING_HINTS = Image.SCALE_SMOOTH;

	private static final Logger LOGGER = LoggerFactory.getLogger(GameBoardPanel.class);

	private static final int MIN_GRID_SQUARE_LENGTH;

	private static final int REGION_HIGHLIGHT_STROKE_WIDTH;

	/**
	 *
	 */
	private static final long serialVersionUID = 6258829324465894025L;

	static {
		REGION_HIGHLIGHT_STROKE_WIDTH = 2;
		IMG_PADDING = REGION_HIGHLIGHT_STROKE_WIDTH * 2;
		MIN_GRID_SQUARE_LENGTH = 10 + IMG_PADDING;
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

	private static void drawRegionHighlights(final Graphics g, final SpatialRegion region, final int colWidth,
			final int rowHeight) {
		final int[] startIdxs = createComponentCoordStartIdxArray(region, colWidth, rowHeight);
		final int[] size = createComponentCoordSizeArray(region, colWidth, rowHeight);
		g.drawRect(startIdxs[0], startIdxs[1], size[0], size[1]);
	}

	private static Image scaleImageToGridSize(final Image img, final SpatialRegion occupiedGridRegion,
			final int colWidth, final int rowHeight) {
		final int[] size = createComponentCoordSizeArray(occupiedGridRegion, colWidth, rowHeight);
		return img.getScaledInstance(Math.max(MIN_GRID_SQUARE_LENGTH, size[0] - IMG_PADDING),
				Math.max(MIN_GRID_SQUARE_LENGTH, size[1] - IMG_PADDING), IMG_SCALING_HINTS);
	}

	private final Controller controller;

	private final boolean debugEnabled;

	private final Color highlightColor;

	/**
	 * At most one region should be highlighted at a time per player
	 */
	private final Set<SpatialRegion> highlightedRegions = Sets.newHashSetWithExpectedSize(1);

	private final BiConsumer<? super GameBoardPanel, ? super Selection> localSelectionHook;

	private final BiConsumer<? super GameBoardPanel, ? super Turn> localTurnCompletionHook;

	private Entry<SpatialRegion, Map<Integer, SpatialRegion>> nextMove;

	private final Map<Integer, Image> pieceImgs;

	private final SpatialMatrix<Integer> posMatrix;

	private final DisablingMouseAdapter selectingMouseListener;

	GameBoardPanel(final SpatialMatrix<Integer> posMatrix, final Map<Integer, Image> pieceImgs,
			final Controller controller, final Color highlightColor,
			final BiConsumer<? super GameBoardPanel, ? super Turn> localTurnCompletionHook,
			final BiConsumer<? super GameBoardPanel, ? super Selection> localSelectionHook) {
		this(posMatrix, pieceImgs, controller, highlightColor, localTurnCompletionHook, localSelectionHook, false);
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

	GameBoardPanel(final SpatialMatrix<Integer> posMatrix, final Map<Integer, Image> pieceImgs,
			final Controller controller, final Color highlightColor,
			final BiConsumer<? super GameBoardPanel, ? super Turn> localTurnCompletionHook,
			final BiConsumer<? super GameBoardPanel, ? super Selection> localSelectionHook,
			final boolean debugEnabled) {
		this.posMatrix = posMatrix;
		this.pieceImgs = pieceImgs;
		this.controller = controller;
		this.highlightColor = highlightColor;
		controller.getListeners().add(this);
		this.localTurnCompletionHook = localTurnCompletionHook;
		this.localSelectionHook = localSelectionHook;
		this.debugEnabled = debugEnabled;
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
	public void paintComponent(final Graphics g) {
		super.paintComponent(g);

		// http://stackoverflow.com/a/21989406/1391325
		{
			final Graphics2D gridDrawingG = (Graphics2D) g.create();
			// set the stroke of the copy, not the original
			gridDrawingG.setStroke(GRID_STROKE);
			try {
				drawBorder(gridDrawingG);
				// Draw a grid (for debugging/devel)
				if (debugEnabled) {
					drawGrid(gridDrawingG);
					drawPieceIds(g);
				}
			} finally {
				gridDrawingG.dispose();
			}
		}

		{
			final Graphics2D regionHighlightingG = createRegionHighlightDrawingGraphics(g);
			regionHighlightingG.setStroke(new BasicStroke(REGION_HIGHLIGHT_STROKE_WIDTH));
			regionHighlightingG.setColor(highlightColor);
			try {
				drawRegionHighlights(regionHighlightingG);
			} finally {
				regionHighlightingG.dispose();
			}
		}
		drawPieceImages(g);
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
	public void updatePlayerSelection(final Selection selection) {
		LOGGER.debug("Observed event representing a user selection.");
		// FIXME: NPE on repaint???
		// localSelectionHook.accept(this, selection);
		final boolean isSelectionCorrect = controller.isSelectionCorrect();
		if (isSelectionCorrect) {
			controller.submitTurnComplete();
			highlightedRegions.clear();
			repaint();
		} else {
			controller.submitSelectionRejection();
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

	/*
	 * (non-Javadoc)
	 *
	 * @see se.kth.speech.coin.tangrams.iristk.Controller.Listener#
	 * updateSelectionRejected(se.kth.speech.coin.tangrams.iristk.events.
	 * Selection)
	 */
	@Override
	public void updateSelectionRejected(final Selection selection) {
		LOGGER.debug("Observed event representing the rejection of the last selection.");
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
		// FIXME: NPE at FilteredImageSource.java:181
		// localTurnCompletionHook.accept(this, turn);
		final SpatialRegion moveSource = turn.getMove().getKey();
		highlightedRegions.remove(moveSource);
		repaint();
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

	private Entry<SpatialRegion, Map<Integer, SpatialRegion>> createRandomMove(final Random rnd) {
		// TODO: Change probability of a piece being selected for moving based
		// on if it was moved before: E.g. cannot move a given piece more than
		// twice in a row
		final List<SpatialRegion> regionsToTry = posMatrix.getElementPlacements().getMinimalRegions();
		// TODO: estimate number of failed tries
		final Set<SpatialRegion> failedRegions = new HashSet<>();
		Entry<SpatialRegion, Map<Integer, SpatialRegion>> pieceMove = null;
		do {
			final SpatialRegion occupiedRegion = RandomCollections.getRandomElement(regionsToTry, rnd);
			final Map<Integer, SpatialRegion> pieceMoveTargets = createRandomValidMoveTargetMap(occupiedRegion, rnd);
			if (pieceMoveTargets.isEmpty()) {
				LOGGER.debug("No valid moves for piece(s) from region {}.", occupiedRegion);
				failedRegions.add(occupiedRegion);
			} else {
				pieceMove = new MutablePair<>(occupiedRegion, pieceMoveTargets);
			}
		} while (pieceMove == null && failedRegions.size() < regionsToTry.size());

		return pieceMove;
	}

	private Map<Integer, SpatialRegion> createRandomValidMoveTargetMap(final SpatialRegion occupiedRegion,
			final Random rnd) {
		final Set<SpatialRegion> regionValidMoves = posMatrix.createValidMoveSet(occupiedRegion);
		final Map<Integer, SpatialRegion> result;
		if (regionValidMoves.isEmpty()) {
			result = Collections.emptyMap();
		} else {
			final SpatialMap<Integer> piecePlacements = posMatrix.getElementPlacements();
			final Collection<Integer> pieceIds = piecePlacements.getMinimalRegionElements().get(occupiedRegion);
			// NOTE: The iterator should only have one element here
			result = Maps.newHashMapWithExpectedSize(pieceIds.size());
			for (final Integer pieceId : pieceIds) {
				final SpatialRegion moveTarget = RandomCollections.getRandomElement(regionValidMoves, rnd);
				assert !piecePlacements.isOccupied(moveTarget);
				result.put(pieceId, moveTarget);
			}
		}
		return result;
	}

	private Graphics2D createRegionHighlightDrawingGraphics(final Graphics g) {
		final Graphics2D result = (Graphics2D) g.create();
		result.setStroke(new BasicStroke(REGION_HIGHLIGHT_STROKE_WIDTH));
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
					final int textYOffset = (rowHeight - pieceTextHeight) / 2;
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

		for (final Entry<SpatialRegion, Integer> piecePlacement : posMatrix.getElementPlacements()
				.getMinimalRegionElements().entries()) {
			final SpatialRegion region = piecePlacement.getKey();
			final Integer pieceId = piecePlacement.getValue();
			final Image initialImg = pieceImgs.get(pieceId);
			final Image scaledImg = scaleImageToGridSize(initialImg, region, colWidth, rowHeight);
			final int[] startIdxs = createComponentCoordStartIdxArray(region, colWidth, rowHeight);
			g.drawImage(scaledImg, startIdxs[0] + REGION_HIGHLIGHT_STROKE_WIDTH,
					startIdxs[1] + REGION_HIGHLIGHT_STROKE_WIDTH, null);
		}
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

	private void notifyNoValidMoves() {
		JOptionPane.showMessageDialog(this, "No more moves available.");
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
	 * @return the controller
	 */
	Controller getController() {
		return controller;
	}

	/**
	 * @return the posMatrix
	 */
	SpatialMatrix<Integer> getPosMatrix() {
		return posMatrix;
	}

	/**
	 *
	 */
	synchronized void notifyContinue(final Random rnd) {
		LOGGER.debug("Notified of continue event.");
		final PlayerRole role = controller.getRole();
		switch (role) {
		case MOVE_SUBMISSION: {
			LOGGER.debug("Generating random move for next turn.");
			nextMove = createRandomMove(rnd);
			if (nextMove == null) {
				// No pieces left to be moved; Game cannot continue
				notifyNoValidMoves();
			} else {
				final SpatialRegion sourceRegion = nextMove.getKey();
				final Collection<Entry<Integer, SpatialRegion>> pieceTargetRegions = nextMove.getValue().entrySet();
				if (pieceTargetRegions.size() > 1) {
					throw new UnsupportedOperationException("Cannot notify multiple moved pieces in one turn.");
				} else {
					toggleHighlightedRegion(sourceRegion);
					final Entry<Integer, SpatialRegion> pieceTargetRegion = pieceTargetRegions.iterator().next();
					controller.submitNextMove(sourceRegion, pieceTargetRegion.getValue(), pieceTargetRegion.getKey());
				}
			}
			break;
		}
		default: {
			JOptionPane.showMessageDialog(this, String.format("Cannot continue while in role %s.", role));
		}

		}
	}

}
