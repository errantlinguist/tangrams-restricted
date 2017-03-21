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
import java.util.Objects;
import java.util.Observable;
import java.util.Observer;
import java.util.Random;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.Supplier;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

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
import se.kth.speech.coin.tangrams.game.AreaSpatialRegionFactory;
import se.kth.speech.coin.tangrams.game.PlayerRole;
import se.kth.speech.coin.tangrams.iristk.LocalController;
import se.kth.speech.coin.tangrams.iristk.events.Area2D;
import se.kth.speech.coin.tangrams.iristk.events.GameEnding;
import se.kth.speech.coin.tangrams.iristk.events.Move;
import se.kth.speech.coin.tangrams.iristk.events.PlayerRoleChange;
import se.kth.speech.coin.tangrams.iristk.events.Selection;
import se.kth.speech.coin.tangrams.iristk.events.Turn;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 2 Mar 2017
 *
 */
final class GameBoardPanel extends JPanel implements Observer {

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
					final SpatialRegion region = biggestPieceRegionUnderSelection.getValue();
					toggleHighlightedRegion(region);
					final int moveConfirmation = MoveDialogs.showSelectConfirmDialog(GameBoardPanel.this);
					switch (moveConfirmation) {
					case JOptionPane.YES_OPTION: {
						localController.submitSelection(biggestPieceRegionUnderSelection);
						break;

					}
					default: {
						break;
					}
					}
					// Remove the highlighting either after submitting the
					// selection or cancelling it
					toggleHighlightedRegion(region);
				}

			} else {
				LOGGER.info("The user clicked on an instance of \"{}\", which cannot be selected.",
						foundComponent.getClass());
			}
		}

	}

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

	private static Image scaleImageToGridSize(final Image img, final SpatialRegion occupiedGridRegion,
			final int colWidth, final int rowHeight) {
		final int[] size = createComponentCoordSizeArray(occupiedGridRegion, colWidth, rowHeight);
		return img.getScaledInstance(Math.max(MIN_GRID_SQUARE_LENGTH, size[0] - IMG_PADDING),
				Math.max(MIN_GRID_SQUARE_LENGTH, size[1] - IMG_PADDING), IMG_SCALING_HINTS);
	}

	private transient final Supplier<String> playerIdGetter;

	/**
	 * At most one region should be highlighted at a time per player
	 */
	private final Set<SpatialRegion> highlightedRegions = Sets.newHashSetWithExpectedSize(1);

	private final Map<Integer, Image> pieceImgs;

	private final SpatialMatrix<Integer> posMatrix;

	private final BiConsumer<? super GameBoardPanel, ? super Turn> localTurnCompletionHook;

	private final BiConsumer<? super GameBoardPanel, ? super Selection> localSelectionHook;

	private final LocalController localController;

	private final AreaSpatialRegionFactory areaRegionFactory;

	private final DisablingMouseAdapter selectingMouseListener;

	private Entry<SpatialRegion, Map<Integer, SpatialRegion>> nextMove;

	GameBoardPanel(final SpatialMatrix<Integer> posMatrix, final Map<Integer, Image> pieceImgs,
			final LocalController localController,
			final BiConsumer<? super GameBoardPanel, ? super Turn> localTurnCompletionHook,
			final BiConsumer<? super GameBoardPanel, ? super Selection> localSelectionHook) {
		this.posMatrix = posMatrix;
		areaRegionFactory = new AreaSpatialRegionFactory(this.posMatrix);
		this.pieceImgs = pieceImgs;
		this.localController = localController;
		localController.addObserver(this);
		playerIdGetter = localController::getPlayerId;
		this.localTurnCompletionHook = localTurnCompletionHook;
		this.localSelectionHook = localSelectionHook;
		selectingMouseListener = new DisablingMouseAdapter(new SelectingMouseAdapter());
		updateMouseListener(localController.getRole());
		addDisablingMouseListener(selectingMouseListener);

		// http://stackoverflow.com/a/1936582/1391325
		final Dimension screenSize = getToolkit().getScreenSize();
		// final Dimension maxSize = Dimensions.createScaledDimension(boardSize,
		// screenSize);
		LOGGER.debug("Setting maximum component size to {}.", screenSize);
		setMaximumSize(screenSize);
		{
			final int shortestScreenLength = (int) (Math.min(screenSize.width, screenSize.height) * 0.75);
			final Dimension boardSize = new Dimension(shortestScreenLength, shortestScreenLength);
			setPreferredSize(boardSize);
		}
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
		// Draw a grid (for debugging/devel)
		// drawGrid(g);
		// drawPieceIds(g);

		{
			final Graphics2D regionHighlightingG = createRegionHighlightDrawingGraphics(g);
			regionHighlightingG.setStroke(new BasicStroke(REGION_HIGHLIGHT_STROKE_WIDTH));
			regionHighlightingG.setColor(Color.MAGENTA);
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

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.Observer#update(java.util.Observable, java.lang.Object)
	 */
	@Override
	public void update(final Observable o, final Object arg) {
		final Stream<?> args;
		if (arg instanceof Stream<?>) {
			args = (Stream<?>) arg;
		} else if (arg instanceof Collection<?>) {
			args = ((Collection<?>) arg).stream();
		} else if (arg instanceof Iterable<?>) {
			final Iterable<?> iter = (Iterable<?>) arg;
			args = StreamSupport.stream(iter.spliterator(), false);
		} else {
			args = Stream.of(arg);
		}
		synchronized (this) {
			// Synchronize once for the whole set of events so that multiple
			// event streams don't interleave in cases where this is still
			// processing one set when another update notification is received
			args.forEach(this::handleUpdateArg);
		}
	}

	// private Graphics2D createRegionHighlightClearingGraphics(final Graphics
	// g) {
	// final Graphics2D result = (Graphics2D) g.create();
	// result.setStroke(new BasicStroke(REGION_HIGHLIGHT_STROKE_WIDTH));
	// result.setColor(getBackground());
	// return result;
	// }

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

	private void drawGrid(final Graphics g) {
		LOGGER.debug("Drawing grid.");
		final Matrix<Integer> posMatrix = this.posMatrix.getPositionMatrix();
		final int[] matrixDims = posMatrix.getDimensions();
		// http://stackoverflow.com/a/21989406/1391325
		// creates a copy of the Graphics instance
		final Graphics2D gridDrawingG = (Graphics2D) g.create();
		try {
			// Row lines
			final int rowHeight = getGridRowHeight();
			int nextRowY = 0;
			// set the stroke of the copy, not the original
			final Stroke dashed = new BasicStroke(1, BasicStroke.CAP_BUTT, BasicStroke.JOIN_BEVEL, 0, new float[] { 1 },
					0);
			gridDrawingG.setStroke(dashed);
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
		highlightedRegions.forEach(region -> {
			drawRegionHighlights(g, region);
		});
	}

	private void drawRegionHighlights(final Graphics g, final SpatialRegion region) {
		final int colWidth = getGridColWidth();
		final int rowHeight = getGridRowHeight();
		final int[] startIdxs = createComponentCoordStartIdxArray(region, colWidth, rowHeight);
		final int[] size = createComponentCoordSizeArray(region, colWidth, rowHeight);
		g.drawRect(startIdxs[0], startIdxs[1], size[0], size[1]);
	}

	private boolean equalsPlayerId(final String str) {
		return Objects.equals(playerIdGetter.get(), str);
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
	
	private void updateMouseListener(PlayerRole role){
		final boolean canSelect = role.equals(PlayerRole.SELECTING);
		selectingMouseListener.setEnabled(canSelect);		
	}

	private void handleUpdateArg(final Object arg) {
		if (arg instanceof PlayerRoleChange) {
			LOGGER.debug("Observed event representing a change in the currently-active player.");
			final PlayerRoleChange change = (PlayerRoleChange) arg;
			if (equalsPlayerId(change.getPlayerId())) {
				// This client initiated the handover
				final PlayerRole newRole = change.getRole();
				updateMouseListener(newRole);
			}
		} else if (arg instanceof GameEnding) {
			LOGGER.debug("Observed event representing a game ending.");
			final GameEnding ending = (GameEnding) arg;
			final GameEnding.Outcome outcome = ending.getOutcome();
			switch (outcome) {
			case ABORT:
				selectingMouseListener.setEnabled(false);
				break;
			case WIN: {
				selectingMouseListener.setEnabled(false);
				break;
			}
			default:
				throw new AssertionError(String.format("No logic for handling outcome %s.", outcome));
			}

		} else if (arg instanceof Selection) {
			LOGGER.debug("Observed event representing a user selection.");
			if (localController.getRole().equals(PlayerRole.WAITING_FOR_SELECTION)) {
				final boolean isSelectionCorrect = localController.isSelectionCorrect();
				if (isSelectionCorrect) {
					JOptionPane.showMessageDialog(this, "Correct move!", "Good selection",
							JOptionPane.INFORMATION_MESSAGE);
					localController.confirmSelection();
				} else {
					JOptionPane.showMessageDialog(this, "Incorrect move!", "Bad selection", JOptionPane.ERROR_MESSAGE);
					localController.rejectSelection();
				}
				Selection selection = (Selection) arg;
				Integer pieceId = selection.getPieceId();
				Area2D area = selection.getArea();
				SpatialRegion region = areaRegionFactory.apply(area);
				toggleHighlightedRegion(region);
			}

		} else if (arg instanceof Turn) {
			final Turn turn = (Turn) arg;
			final String turnPlayerId = turn.getPlayerId();
			LOGGER.debug("Observed event representing a turn completed by \"{}\".", turnPlayerId);
			if (equalsPlayerId(turnPlayerId)) {
				LOGGER.debug("Skipping view update for remote notification about player's own turn.");
				localTurnCompletionHook.accept(this, turn);
			} else {
				final Move move = turn.getMove();
				updatePiecePositions(move);
				repaint();
			}

		} else {
			LOGGER.debug("Ignoring observed event arg object of type \"{}\".", arg.getClass().getName());
		}
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

	private void updatePiecePositions(final Entry<SpatialRegion, Map<Integer, SpatialRegion>> pieceMove) {
		final SpatialRegion occupiedRegion = pieceMove.getKey();
		final Map<Integer, SpatialRegion> pieceMoveTargets = pieceMove.getValue();
		for (final Entry<Integer, SpatialRegion> pieceMoveTarget : pieceMoveTargets.entrySet()) {
			final Integer pieceId = pieceMoveTarget.getKey();
			final SpatialRegion moveTarget = pieceMoveTarget.getValue();
			if (!Arrays.equals(occupiedRegion.getDimensions(), moveTarget.getDimensions())) {
				throw new IllegalArgumentException(String.format(
						"Target region does not have the same dimensions (%s) as the source (%s).",
						Arrays.toString(occupiedRegion.getDimensions()), Arrays.toString(moveTarget.getDimensions())));
			}

			posMatrix.placeElement(pieceId, moveTarget);
		}
		posMatrix.clearRegion(occupiedRegion);
	}

	private void updatePiecePositions(final Move move) {
		final SpatialRegion source = areaRegionFactory.apply(move.getSource());
		final SpatialRegion target = areaRegionFactory.apply(move.getTarget());
		updatePiecePositions(source, target);
	}

	private void updatePiecePositions(final SpatialRegion source, final SpatialRegion target) {
		final SpatialMap<Integer> piecePlacements = posMatrix.getElementPlacements();
		final Collection<Integer> pieceIds = piecePlacements.getMinimalRegionElements().get(source);
		for (final Integer pieceId : pieceIds) {
			posMatrix.placeElement(pieceId, target);
		}
		posMatrix.clearRegion(source);
	}

	/**
	 * @return the localController
	 */
	LocalController getLocalController() {
		return localController;
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
		switch (localController.getRole()) {
		case SELECTING: {
			JOptionPane.showMessageDialog(this, "You must choose which piece is to be moved before continuing.");
			break;
		}
		case TURN_SUBMISSION: {
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
					localController.submitNextMove(sourceRegion, pieceTargetRegion.getValue(),
							pieceTargetRegion.getKey());
				}
			}
			break;
		}
		case WAITING_FOR_SELECTION: {
			JOptionPane.showMessageDialog(this, "The other player must choose a piece before continuing.");
			LOGGER.debug("Still waiting for the other player to select a piece; Cannot continue to the next turn.");
			break;
		}
		case WAITING_FOR_SELECTION_CONFIRMATION: {
			JOptionPane.showMessageDialog(this, "The other player must confirm the piece selection before continuing.");
			LOGGER.debug("Still waiting for the other player to confirm move.");
			break;
		}
		default: {
			throw new AssertionError("No logic for case: " + localController.getRole());
		}

		}
	}

	/**
	 *
	 */
	synchronized void notifyUndo() {
		LOGGER.debug("Notified of undo event.");
		// TODO Auto-generated method stub

	}

}
