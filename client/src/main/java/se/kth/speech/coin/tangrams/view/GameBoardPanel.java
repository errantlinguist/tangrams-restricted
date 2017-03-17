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
import java.awt.Dimension;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Stroke;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Random;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.IntStream;

import javax.swing.JOptionPane;
import javax.swing.JPanel;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Maps;
import com.google.common.collect.Sets;

import se.kth.speech.IntArrays;
import se.kth.speech.Matrix;
import se.kth.speech.MutablePair;
import se.kth.speech.RandomCollections;
import se.kth.speech.SpatialMap;
import se.kth.speech.SpatialMatrix;
import se.kth.speech.SpatialRegion;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 2 Mar 2017
 *
 */
final class GameBoardPanel extends JPanel {

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

	private static <E> SpatialMatrix<Integer, E> createPosMatrix(final int[] gridSize,
			final Function<? super E, Integer> pieceIdGetter, final SpatialMap<E> posMap) {
		LOGGER.info("Creating a position matrix of size {}.", gridSize);
		final Integer[] posMatrixBackingArray = new Integer[IntArrays.product(gridSize)];
		final Matrix<Integer> backingPosMatrix = new Matrix<>(posMatrixBackingArray, gridSize[1]);
		return new SpatialMatrix<>(backingPosMatrix, pieceIdGetter, posMap);
	}

	private static Image scaleImageToGridSize(final Image img, final SpatialRegion occupiedGridRegion,
			final int colWidth, final int rowHeight) {
		final int[] size = createComponentCoordSizeArray(occupiedGridRegion, colWidth, rowHeight);
		return img.getScaledInstance(Math.max(MIN_GRID_SQUARE_LENGTH, size[0] - IMG_PADDING),
				Math.max(MIN_GRID_SQUARE_LENGTH, size[1] - IMG_PADDING), IMG_SCALING_HINTS);
	}

	/**
	 * At most one region should be highlighted at a time per player
	 */
	private final Set<SpatialRegion> highlightedRegions = Sets.newHashSetWithExpectedSize(1);

	private final Map<ImageViewInfo, Image> pieceImgs;

	// GameBoardPanel(final Map<ImageViewInfo, Image> pieceImgs, final Random
	// rnd, final double occupiedGridArea) {
	// this.pieceImgs = pieceImgs;
	// final Map<ImageViewInfo, Integer> pieceIds =
	// Maps.newHashMapWithExpectedSize(pieceImgs.size());
	// final Function<ImageViewInfo, Integer> incrementingPieceIdGetter = piece
	// -> pieceIds.computeIfAbsent(piece,
	// k -> pieceIds.size());
	// // http://stackoverflow.com/a/1936582/1391325
	// final Dimension screenSize = getToolkit().getScreenSize();
	// // final Dimension maxSize = Dimensions.createScaledDimension(boardSize,
	// // screenSize);
	// LOGGER.debug("Setting maximum component size to {}.", screenSize);
	// setMaximumSize(screenSize);
	// {
	// final int shortestScreenLength = (int) (Math.min(screenSize.width,
	// screenSize.height) * 0.75);
	// final Dimension boardSize = new Dimension(shortestScreenLength,
	// shortestScreenLength);
	// setPreferredSize(boardSize);
	// final PositionGridSizeSummary posGridSizeSummary =
	// createPositionGridSizeSummary(
	// pieceImgs.keySet().iterator());
	// LOGGER.info("Position grid size summary: {}", posGridSizeSummary);
	// final int[] gridSize = createPositionGridSize(posGridSizeSummary,
	// boardSize, occupiedGridArea);
	// LOGGER.info("Creating a position matrix of size {}.", gridSize);
	// final Integer[] posMatrixBackingArray = new
	// Integer[IntArrays.product(gridSize)];
	// final Matrix<Integer> backingPosMatrix = new
	// Matrix<>(posMatrixBackingArray, gridSize[1]);
	// if (!isDimensionDivisibleIntoGrid(boardSize, backingPosMatrix)) {
	// throw new IllegalArgumentException(
	// String.format("Board %s not divisble into matrix with dimensions %s.",
	// boardSize,
	// Arrays.toString(backingPosMatrix.getDimensions())));
	// }
	// final Function<? super ImageViewInfo, int[]> piecePosMatrixSizeFactory =
	// imgViewInfo -> imgViewInfo
	// .getGridSize(IMAGE_SIZE_FACTORS);
	// final Collection<ImageViewInfo> pieces = pieceImgs.keySet();
	// final SpatialMap<ImageViewInfo> posMap = new SpatialMap<>(pieces.size());
	// posMatrix = new SpatialMatrix<>(backingPosMatrix, pieceIds::get, posMap);
	// final RandomMatrixPositionFiller<Integer, ImageViewInfo> matrixFiller =
	// new RandomMatrixPositionFiller<>(
	// posMatrix, incrementingPieceIdGetter, rnd, piecePosMatrixSizeFactory);
	// matrixFiller.apply(pieces);
	// final Collection<ImageViewInfo> allElements = posMap.getAllElements();
	// LOGGER.info("Created a spatial map with element IDs {}.",
	// allElements.stream().map(pieceIds::get)
	// .collect(Collectors.toCollection(() -> new
	// ArrayList<>(allElements.size()))));
	// }
	// {
	// final int[] minSizeDims =
	// createMinimumDimLengths(posMatrix.getDimensions()).toArray();
	// // NOTE: "rows" in the matrix go top-bottom and "cols" go
	// // left-right
	// final Dimension minSize = new Dimension(minSizeDims[1], minSizeDims[0]);
	// LOGGER.debug("Setting minimum component size to {}.", minSize);
	// setMinimumSize(minSize);
	// }
	// // Finished with creating necessary data structures
	// System.out.println("IMAGE PLACEMENTS");
	// System.out.println(createMatrixReprString(posMatrix.getPositionMatrix()));
	// }

	private final SpatialMatrix<Integer, ImageViewInfo> posMatrix;

	GameBoardPanel(final int[] gridSize, final Function<? super ImageViewInfo, Integer> pieceIdGetter,
			final int uniqueImgResourceCount) {
		posMatrix = createPosMatrix(gridSize, pieceIdGetter, new SpatialMap<>(uniqueImgResourceCount));
		// Use linked map in order to preserve iteration order in provided
		// sequence
		pieceImgs = Maps.newLinkedHashMapWithExpectedSize(uniqueImgResourceCount);
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

	GameBoardPanel(final SpatialMatrix<Integer, ImageViewInfo> posMatrix, final int uniqueImgResourceCount) {
		this.posMatrix = posMatrix;
		// Use linked map in order to preserve iteration order in provided
		// sequence
		pieceImgs = Maps.newLinkedHashMapWithExpectedSize(uniqueImgResourceCount);
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

	GameBoardPanel(final SpatialMatrix<Integer, ImageViewInfo> posMatrix, final Map<ImageViewInfo, Image> pieceImgs) {
		this.posMatrix = posMatrix;
		this.pieceImgs = pieceImgs;
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
		drawGrid(g);
		drawPieceIds(g);

		drawPieceImages(g);
		{
			final Graphics2D regionHighlightingG = (Graphics2D) g.create();
			regionHighlightingG.setStroke(new BasicStroke(REGION_HIGHLIGHT_STROKE_WIDTH));
			regionHighlightingG.setColor(Color.MAGENTA);
			try {
				drawRegionHighlights(regionHighlightingG);
			} finally {
				regionHighlightingG.dispose();
			}
		}

	}

	private Map<ImageViewInfo, SpatialRegion> createRandomValidMoveTargetMap(final SpatialRegion occupiedRegion,
			final Random rnd) {
		final Map<SpatialRegion, Set<SpatialRegion>> validMoves = posMatrix.createValidMoveMap();
		final Map<ImageViewInfo, SpatialRegion> result;
		final Set<SpatialRegion> regionValidMoves = validMoves.get(occupiedRegion);
		if (regionValidMoves.isEmpty()) {
			result = Collections.emptyMap();
		} else {
			final SpatialMap<ImageViewInfo> piecePlacements = posMatrix.getElementPlacements();
			final Collection<ImageViewInfo> pieces = piecePlacements.getMinimalRegionElements().get(occupiedRegion);
			// NOTE: The iterator should only have one element here
			result = Maps.newHashMapWithExpectedSize(pieces.size());
			for (final ImageViewInfo piece : pieces) {
				final SpatialRegion moveTarget = RandomCollections.getRandomElement(regionValidMoves, rnd);
				assert !piecePlacements.isOccupied(moveTarget);
				result.put(piece, moveTarget);
			}
		}
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

		for (final Entry<SpatialRegion, ImageViewInfo> piecePlacement : posMatrix.getElementPlacements()
				.getMinimalRegionElements().entries()) {
			final SpatialRegion region = piecePlacement.getKey();
			final ImageViewInfo pieceViewInfo = piecePlacement.getValue();
			final Image initialImg = pieceImgs.get(pieceViewInfo);
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
			final int[] startIdxs = createComponentCoordStartIdxArray(region, colWidth, rowHeight);
			final int[] size = createComponentCoordSizeArray(region, colWidth, rowHeight);
			g.drawRect(startIdxs[0], startIdxs[1], size[0], size[1]);
		});
	}

	private int getGridColWidth() {
		final int[] matrixDims = posMatrix.getDimensions();
		return getWidth() / matrixDims[1];
	}

	private int getGridRowHeight() {
		final int[] matrixDims = posMatrix.getDimensions();
		return getHeight() / matrixDims[0];
	}

	private void notifyNoValidMoves() {
		JOptionPane.showMessageDialog(this, "No more moves available.");
	}

	/**
	 * @return the pieceImgs
	 */
	Map<ImageViewInfo, Image> getPieceImgs() {
		return pieceImgs;
	}

	/**
	 * @return the posMatrix
	 */
	SpatialMatrix<Integer, ImageViewInfo> getPosMatrix() {
		return posMatrix;
	}

	/**
	 *
	 */
	synchronized void notifyContinue(final Random rnd) {
		LOGGER.debug("Notified of continue event.");
		LOGGER.debug("Moving random piece.");
		// TODO: Change probability of a piece being selected for moving based
		// on if it was moved before: E.g. cannot move a given piece more than
		// twice in a row
		final List<SpatialRegion> regionsToTry = posMatrix.getElementPlacements().getMinimalRegions();
		// TODO: estimate number of failed tries
		final Set<SpatialRegion> failedRegions = new HashSet<>();
		Entry<SpatialRegion, Map<ImageViewInfo, SpatialRegion>> pieceMove = null;
		do {
			final SpatialRegion occupiedRegion = RandomCollections.getRandomElement(regionsToTry, rnd);
			final Map<ImageViewInfo, SpatialRegion> pieceMoveTargets = createRandomValidMoveTargetMap(occupiedRegion,
					rnd);
			if (pieceMoveTargets.isEmpty()) {
				LOGGER.debug("No valid moves for piece(s) from region {}.", occupiedRegion);
				failedRegions.add(occupiedRegion);
			} else {
				pieceMove = new MutablePair<>(occupiedRegion, pieceMoveTargets);
			}
		} while (pieceMove == null && failedRegions.size() < regionsToTry.size());

		if (pieceMove == null) {
			// No pieces left to be moved; Game cannot continue
			notifyNoValidMoves();
		} else {
			notifyMove(pieceMove);
			LOGGER.debug("Finished randomly moving piece(s) at {}.", pieceMove.getKey());
		}
	}

	void notifyMove(final Entry<SpatialRegion, Map<ImageViewInfo, SpatialRegion>> pieceMove) {
		final SpatialRegion occupiedRegion = pieceMove.getKey();
		final Map<ImageViewInfo, SpatialRegion> pieceMoveTargets = pieceMove.getValue();
		// System.out.println("BEFORE" + System.lineSeparator() +
		// System.lineSeparator()
		// + new
		// MatrixStringReprFactory().apply(posMatrix.getPositionMatrix()));
		highlightedRegions.add(occupiedRegion);
		for (final Entry<ImageViewInfo, SpatialRegion> pieceMoveTarget : pieceMoveTargets.entrySet()) {
			final ImageViewInfo piece = pieceMoveTarget.getKey();
			final SpatialRegion moveTarget = pieceMoveTarget.getValue();
			if (!Arrays.equals(occupiedRegion.getDimensions(), moveTarget.getDimensions())) {
				throw new IllegalArgumentException(String.format(
						"Target region does not have the same dimensions (%s) as the source (%s).",
						Arrays.toString(occupiedRegion.getDimensions()), Arrays.toString(moveTarget.getDimensions())));
			}

			posMatrix.placeElement(piece, moveTarget);
		}
		posMatrix.clearRegion(occupiedRegion);
		// System.out.println(System.lineSeparator() + System.lineSeparator() +
		// "AFTER" + System.lineSeparator() + System.lineSeparator()
		// + new
		// MatrixStringReprFactory().apply(posMatrix.getPositionMatrix()));
		repaint();
	}

	/**
	 *
	 */
	synchronized void notifyUndo() {
		LOGGER.debug("Notified of undo event.");
		// TODO Auto-generated method stub

	}

}
