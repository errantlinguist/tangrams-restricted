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
import java.awt.Canvas;
import java.awt.Dimension;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Stroke;
import java.util.Arrays;
import java.util.List;
import java.util.ListIterator;
import java.util.Map.Entry;
import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.Matrix;
import se.kth.speech.SpatialMap;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 2 Mar 2017
 *
 */
final class GameBoardPanel extends Canvas {

	private static final Logger LOGGER = LoggerFactory.getLogger(GameBoardPanel.class);

	/**
	 *
	 */
	private static final long serialVersionUID = 6258829324465894025L;

	private static boolean isDimensionDivisibleIntoGrid(final Dimension dim, final Matrix<?> matrix) {
		final int[] matrixDims = matrix.getDimensions();
		return dim.getHeight() % matrixDims[0] == 0 && dim.getWidth() % matrixDims[1] == 0;
	}

	private final Matrix<Integer> posMatrix;

	private final SpatialMap<? extends Entry<? extends Image, ImageViewInfo>> imagePlacements;

	public GameBoardPanel(final Dimension boardSize,
			final List<? extends Entry<? extends Image, ImageViewInfo>> imgViewInfoDataList,
			final Matrix<Integer> posMatrix,
			final SpatialMap<? extends Entry<? extends Image, ImageViewInfo>> imagePlacements) {
		if (!isDimensionDivisibleIntoGrid(boardSize, posMatrix)) {
			throw new IllegalArgumentException(String.format("Board %s not divisble into matrix with dimensions %s.",
					boardSize, Arrays.toString(posMatrix.getDimensions())));
		}
		this.posMatrix = posMatrix;
		this.imagePlacements = imagePlacements;
		setSize(boardSize);

	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.awt.Canvas#paint(java.awt.Graphics)
	 */
	@Override
	public void paint(final Graphics g) {
		final int[] matrixDims = posMatrix.getDimensions();
		final int rowHeight = getHeight() / matrixDims[0];
		final int colWidth = getWidth() / matrixDims[1];

		// Draw a grid (for debugging/devel)
		// drawGrid(g);
		drawPieceIds(g);

		final Iterable<? extends Entry<? extends Entry<? extends Image, ImageViewInfo>, SpatialMap.Region>> elementRegions = imagePlacements
				.elementRegions();
		for (final Entry<? extends Entry<? extends Image, ImageViewInfo>, SpatialMap.Region> elementRegion : elementRegions) {
			final Entry<? extends Image, ImageViewInfo> pieceDisplayInfo = elementRegion.getKey();
			final Image img = pieceDisplayInfo.getKey();
			final SpatialMap.Region region = elementRegion.getValue();

			final int imgStartX = region.getXLowerBound() * colWidth;
			final int imgStartY = region.getYLowerBound() * rowHeight;
			// final int imgEndX = region.getXUpperBound() * colWidth;
			// final int imgEndY = region.getYUpperBound() * rowHeight;
			// g.drawImage(img, imgStartX, imgStartY, null);
		}
	}

	private void drawGrid(final Graphics g) {
		final int[] matrixDims = posMatrix.getDimensions();

		// Row lines
		final int rowHeight = getHeight() / matrixDims[0];
		int nextRowY = 0;
		// http://stackoverflow.com/a/21989406/1391325
		// creates a copy of the Graphics instance
		final Graphics2D gridDrawingG = (Graphics2D) g.create();
		// set the stroke of the copy, not the original
		final Stroke dashed = new BasicStroke(1, BasicStroke.CAP_BUTT, BasicStroke.JOIN_BEVEL, 0, new float[] { 1 }, 0);
		gridDrawingG.setStroke(dashed);
		for (final ListIterator<List<Integer>> matrixRowIter = posMatrix.rowIterator(); matrixRowIter.hasNext();) {
			gridDrawingG.drawLine(0, nextRowY, getWidth(), nextRowY);
			nextRowY += rowHeight;
		}

		// Column lines
		final int colWidth = getWidth() / matrixDims[1];
		int nextColX = 0;
		for (int colIdx = 0; colIdx < matrixDims[1]; ++colIdx) {
			gridDrawingG.drawLine(nextColX, 0, nextColX, getHeight());
			nextColX += colWidth;
		}
	}

	private void drawPieceIds(final Graphics g) {
		final int[] matrixDims = posMatrix.getDimensions();
		final int rowHeight = getHeight() / matrixDims[0];
		final int colWidth = getWidth() / matrixDims[1];
		final FontMetrics fm = g.getFontMetrics();
		final int pieceTextHeight = fm.getAscent();
		int nextRowY = 0;
		for (final ListIterator<List<Integer>> matrixRowIter = posMatrix.rowIterator(); matrixRowIter.hasNext();) {
			final List<Integer> matrixRow = matrixRowIter.next();
			final int rowIdx = matrixRowIter.nextIndex();

			int nextColX = 0;
			for (final ListIterator<Integer> matrixRowCellIter = matrixRow.listIterator(); matrixRowCellIter
					.hasNext();) {
				final int colIdx = matrixRowCellIter.nextIndex();
				final Integer pieceId = matrixRowCellIter.next();
				if (pieceId != null){
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
	}

}
