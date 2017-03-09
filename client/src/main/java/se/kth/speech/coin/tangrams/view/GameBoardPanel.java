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

import java.awt.Canvas;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.util.Arrays;
import java.util.List;
import java.util.Map.Entry;

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
	
	private static boolean isDimensionDivisibleIntoGrid(Dimension dim, Matrix<?> matrix){
		int[] matrixDims = matrix.getDimensions();
		return dim.getHeight() % matrixDims[0] == 0 &&  dim.getWidth() % matrixDims[1] == 0;
	}

	/**
	 *
	 */
	private static final long serialVersionUID = 6258829324465894025L;
	
	private final Matrix<Integer> posMatrix;

	public GameBoardPanel(final Dimension boardSize, List<Entry<BufferedImage, ImageViewInfo>> imgViewInfoDataList, Matrix<Integer> posMatrix, SpatialMap<Entry<BufferedImage, ImageViewInfo>> imagePlacements) {
		if (!isDimensionDivisibleIntoGrid(boardSize, posMatrix)){
			throw new IllegalArgumentException(String.format("Board %s not divisble into matrix with dimensions %s.", boardSize, Arrays.toString(posMatrix.getDimensions())));
		}
		this.posMatrix = posMatrix;
		setSize(boardSize);
		
		// TODO Auto-generated constructor stub
	}

	/* (non-Javadoc)
	 * @see java.awt.Canvas#paint(java.awt.Graphics)
	 */
	@Override
	public void paint(Graphics g) {
//		posMatrix.get
//		g.drawLine(x1, y1, x2, y2);
	}

}
