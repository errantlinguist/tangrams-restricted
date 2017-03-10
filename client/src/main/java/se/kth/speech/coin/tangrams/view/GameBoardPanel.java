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
import java.awt.image.BufferedImage;
import java.awt.image.FilteredImageSource;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.EnumSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.function.IntSupplier;

import javax.imageio.ImageIO;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Maps;
import com.google.common.collect.Sets;

import se.kth.speech.MathDenominators;
import se.kth.speech.Matrix;
import se.kth.speech.MutablePair;
import se.kth.speech.SpatialMap;
import se.kth.speech.awt.ColorReplacementImageFilter;
import se.kth.speech.coin.tangrams.content.ImageSize;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;
import se.kth.speech.coin.tangrams.view.ImageViewInfo.RasterizationInfo;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 2 Mar 2017
 *
 */
final class GameBoardPanel extends Canvas {

	private static class SizeValidator {

		private enum ValidationComment {
			GCD_BELOW_MIN, GCD_NOT_MULTIPLE_OF_LCD, HEIGHT_BELOW_MIN, WIDTH_BELOW_MIN;
		}

		private final int lcd;

		private final int minDimLength;

		private final int minGcd;

		private SizeValidator(final int minDimLength, final int minGcd, final int lcd) {
			this.minDimLength = minDimLength;
			this.minGcd = minGcd;
			this.lcd = lcd;
		}

		private EnumSet<ValidationComment> validate(final int width, final int height, final int imgGcd) {
			final EnumSet<ValidationComment> result = EnumSet.noneOf(ValidationComment.class);
			if (width < minDimLength) {
				result.add(ValidationComment.WIDTH_BELOW_MIN);
			}
			if (height < minDimLength) {
				result.add(ValidationComment.HEIGHT_BELOW_MIN);
			}
			if (imgGcd < minGcd) {
				result.add(ValidationComment.GCD_BELOW_MIN);
			}
			if (imgGcd % lcd != 0) {
				result.add(ValidationComment.GCD_NOT_MULTIPLE_OF_LCD);
			}
			return result;
		}

	}

	private static final int IMG_SCALING_HINTS = Image.SCALE_SMOOTH;

	private static final Logger LOGGER = LoggerFactory.getLogger(GameBoardPanel.class);

	/**
	 *
	 */
	private static final long serialVersionUID = 6258829324465894025L;

	private static void appendRowTableRepr(final Iterator<?> rowCellIter, final StringBuilder sb) {
		final String nullValRepr = "-";
		if (rowCellIter.hasNext()) {
			final Object first = rowCellIter.next();
			final String firstRepr = Objects.toString(first, nullValRepr);
			sb.append(firstRepr);

			while (rowCellIter.hasNext()) {
				sb.append('\t');
				final Object next = rowCellIter.next();
				final String nextRepr = Objects.toString(next, nullValRepr);
				sb.append(nextRepr);
			}
		}
	}

	private static String createImageInfoTable(
			final Collection<? extends Entry<?, ? extends Entry<ImageViewInfo.RasterizationInfo, ?>>> namedImgValData) {
		final String header = "PATH\tWIDTH\tHEIGHT\tGCD\tCOMMENT";
		final StringBuilder sb = new StringBuilder(header.length() + 16 * namedImgValData.size());
		sb.append(header);
		for (final Entry<?, ? extends Entry<ImageViewInfo.RasterizationInfo, ?>> namedImgValDatumComments : namedImgValData) {
			sb.append(System.lineSeparator());
			sb.append(namedImgValDatumComments.getKey());
			sb.append('\t');
			final Entry<ImageViewInfo.RasterizationInfo, ?> imgValDatumComments = namedImgValDatumComments.getValue();
			final ImageViewInfo.RasterizationInfo imgVisualizationInfoDatum = imgValDatumComments.getKey();
			sb.append(imgVisualizationInfoDatum.getWidth());
			sb.append('\t');
			sb.append(imgVisualizationInfoDatum.getHeight());
			sb.append('\t');
			sb.append(imgVisualizationInfoDatum.getGcd());
			sb.append('\t');
			sb.append(imgValDatumComments.getValue());
		}
		return sb.toString();
	}

	private static String createMatrixReprString(final Matrix<?> matrix) {
		final int cellCount = matrix.getValues().size();
		final StringBuilder sb = new StringBuilder(cellCount * 4);

		final Iterator<? extends List<?>> rowIter = matrix.rowIterator();
		if (rowIter.hasNext()) {
			final List<?> first = rowIter.next();
			appendRowTableRepr(first.iterator(), sb);
			while (rowIter.hasNext()) {
				sb.append(System.lineSeparator());
				final List<?> next = rowIter.next();
				appendRowTableRepr(next.iterator(), sb);
			}
		}
		return sb.toString();
	}

	private static Image createScaledImage(final Image origImg, final ImageSize size, final int lesserDimVal,
			final int maxDimLength, final ImageViewInfo.Orientation orientation) {
		final Image result;
		if (lesserDimVal < maxDimLength) {
			switch (orientation) {
			case PORTRAIT: {
				result = origImg.getScaledInstance(-1, maxDimLength, IMG_SCALING_HINTS);
				break;
			}
			default: {
				result = origImg.getScaledInstance(-1, maxDimLength, IMG_SCALING_HINTS);
				break;
			}
			}
		} else {
			// Just use the original, unscaled image
			result = origImg;
		}
		return result;
	}

	private static boolean isDimensionDivisibleIntoGrid(final Dimension dim, final Matrix<?> matrix) {
		final int[] matrixDims = matrix.getDimensions();
		return dim.getHeight() % matrixDims[0] == 0 && dim.getWidth() % matrixDims[1] == 0;
	}

	private static Image scaleImageByLongerDimension(final BufferedImage origImg, final int longerDimVal) {
		final Image result;
		final ImageViewInfo.Orientation orientation = ImageViewInfo.Orientation.getOrientation(origImg.getWidth(),
				origImg.getHeight());
		switch (orientation) {
		case PORTRAIT: {
			result = origImg.getScaledInstance(-1, longerDimVal, IMG_SCALING_HINTS);
			break;
		}
		default: {
			result = origImg.getScaledInstance(longerDimVal, -1, IMG_SCALING_HINTS);
			break;
		}
		}
		return result;
	}

	private static Image scaleImageByShorterDimension(final BufferedImage origImg, final int shorterDimVal) {
		final Image result;
		final ImageViewInfo.Orientation orientation = ImageViewInfo.Orientation.getOrientation(origImg.getWidth(),
				origImg.getHeight());
		switch (orientation) {
		case PORTRAIT: {
			result = origImg.getScaledInstance(shorterDimVal, -1, IMG_SCALING_HINTS);
			break;
		}
		default: {
			result = origImg.getScaledInstance(-1, shorterDimVal, IMG_SCALING_HINTS);
			break;
		}
		}
		return result;
	}

	private static Image scaleImageToGridSize(final Image img, final SpatialMap.Region occupiedGridRegion,
			final int colWidth, final int rowHeight) {
		// NOTE: occupied region is denoted in matrix rows*columns
		// "y" is matrix columns, thus left-to-right
		final int regionGridColCount = occupiedGridRegion.getLengthY();
		LOGGER.debug("Image occupies {} grid column(s).", regionGridColCount);
		final int imgWidth = colWidth * regionGridColCount;
		// "x" is matrix rows, thus top-to-bottom
		final int regionGridRowCount = occupiedGridRegion.getLengthX();
		LOGGER.debug("Image occupies {} grid row(s).", regionGridRowCount);
		final int imgHeight = rowHeight * regionGridRowCount;
		return img.getScaledInstance(imgWidth, imgHeight, IMG_SCALING_HINTS);
	}

	private final SpatialMap<? extends Entry<? extends Image, ImageViewInfo>> piecePlacements;

	private final Matrix<Integer> posMatrix;

	GameBoardPanel(final Collection<ImageVisualizationInfo> imgVisualizationInfoData,
			final BiFunction<? super Matrix<? super Integer>, ? super List<? extends Entry<? extends Image, ImageViewInfo>>, SpatialMap<Entry<? extends Image, ImageViewInfo>>> matrixFiller) {
		// The minimum accepted length of the shortest dimension for an image
		final int minDimLength = 300;
		final SizeValidator validator = new SizeValidator(minDimLength, 50, 50);
		try {
			final LinkedHashMap<Image, ImageViewInfo> imgViewInfoDataMap = createImageViewInfoMap(
					imgVisualizationInfoData, validator);
			// Create a list for assigning an ID (i.e. index) to each image
			final List<Entry<Image, ImageViewInfo>> imgViewInfoDataList = new ArrayList<>(
					imgViewInfoDataMap.entrySet());
			final Set<Integer> dimensionValues = Sets.newHashSetWithExpectedSize(imgViewInfoDataList.size() + 1);
			for (final Entry<Image, ImageViewInfo> imgViewInfoDatum : imgViewInfoDataList) {
				final ImageViewInfo viewInfo = imgViewInfoDatum.getValue();
				dimensionValues.add(viewInfo.getRasterization().getGcd());
			}
			final Dimension boardSize = new Dimension(minDimLength * 5, minDimLength * 4);
			setSize(boardSize);
			dimensionValues.add(boardSize.width);
			dimensionValues.add(boardSize.height);
			// Get the GCD for all components in the view
			final int greatestCommonDenominator = MathDenominators.gcd(dimensionValues.iterator());
			LOGGER.debug("GCD for all components is {}.", greatestCommonDenominator);
			// Validate the size and GCD of all components, including the board
			// itself
			final Set<SizeValidator.ValidationComment> boardValidationComments = validator.validate(boardSize.width,
					boardSize.height, greatestCommonDenominator);
			if (boardValidationComments.isEmpty()) {
				// NOTE: "rows" in the matrix go top-bottom and "cols" go
				// left-right
				final int posMatrixRows = boardSize.height / greatestCommonDenominator;
				final int posMatrixCols = boardSize.width / greatestCommonDenominator;
				LOGGER.info("Creating a position matrix of size {}*{}.", posMatrixRows, posMatrixCols);
				final Integer[] posMatrixBackingArray = new Integer[posMatrixRows * posMatrixCols];
				posMatrix = new Matrix<>(posMatrixBackingArray, posMatrixCols);
				if (!isDimensionDivisibleIntoGrid(boardSize, posMatrix)) {
					throw new IllegalArgumentException(
							String.format("Board %s not divisble into matrix with dimensions %s.", boardSize,
									Arrays.toString(posMatrix.getDimensions())));
				}
				piecePlacements = matrixFiller.apply(posMatrix, imgViewInfoDataList);
				// Finished with creating necessary data structures
				System.out.println("IMAGE PLACEMENTS");
				System.out.println(createMatrixReprString(posMatrix));
			} else {
				throw new IllegalArgumentException(
						String.format("The board as a whole failed validation with dimensions %s; and GCD %d: %s",
								boardSize, greatestCommonDenominator, boardValidationComments));
			}

		} catch (final IOException e) {
			throw new UncheckedIOException(e);
		}
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.awt.Canvas#paint(java.awt.Graphics)
	 */
	@Override
	public void paint(final Graphics g) {
		// Draw a grid (for debugging/devel)
		drawGrid(g);
		drawPieceIds(g);

		drawPieceImages(g);

	}

	private LinkedHashMap<Image, ImageViewInfo> createImageViewInfoMap(
			final Collection<ImageVisualizationInfo> imgVisualizationInfoData, final SizeValidator validator)
			throws IOException {
		// Use linked map in order to preserve iteration order in provided
		// sequence
		final LinkedHashMap<Image, ImageViewInfo> result = Maps
				.newLinkedHashMapWithExpectedSize(imgVisualizationInfoData.size());

		final Map<URL, Entry<ImageViewInfo.RasterizationInfo, Set<SizeValidator.ValidationComment>>> badImgs = Maps
				.newHashMapWithExpectedSize(0);
		for (final ImageVisualizationInfo imgVisualizationInfoDatum : imgVisualizationInfoData) {
			final URL imgResourceLoc = imgVisualizationInfoDatum.getResourceLoc();
			final BufferedImage initialImg = ImageIO.read(imgResourceLoc);
			final IntSupplier widthGetter = initialImg::getWidth;
			final IntSupplier heightGetter = initialImg::getHeight;

			{
				// Size/aspect ratio calculation
				final ImageViewInfo.RasterizationInfo imgRasterizationInfo = new ImageViewInfo.RasterizationInfo(
						widthGetter, heightGetter);
				final Image coloredImg = getToolkit().createImage(new FilteredImageSource(initialImg.getSource(),
						new ColorReplacementImageFilter(imgVisualizationInfoDatum.getColor())));
				result.put(coloredImg, new ImageViewInfo(imgVisualizationInfoDatum, imgRasterizationInfo));
				{
					// Validate image
					final Set<SizeValidator.ValidationComment> validationComments = validator.validate(
							widthGetter.getAsInt(), heightGetter.getAsInt(),
							MathDenominators.gcd(widthGetter.getAsInt(), heightGetter.getAsInt()));
					if (!validationComments.isEmpty()) {
						badImgs.put(imgResourceLoc, new MutablePair<>(imgRasterizationInfo, validationComments));
					}
				}
			}

		}

		if (badImgs.isEmpty()) {
			return result;
		} else {
			throw new IllegalArgumentException("One or more images failed validation:" + System.lineSeparator()
					+ createImageInfoTable(badImgs.entrySet()));
		}
	}

	private void drawGrid(final Graphics g) {
		LOGGER.debug("Drawing grid.");
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
		for (final ListIterator<List<Integer>> matrixRowIter = posMatrix.rowIterator(); matrixRowIter.hasNext();) {
			final List<Integer> matrixRow = matrixRowIter.next();
			final int rowIdx = matrixRowIter.nextIndex();

			int nextColX = 0;
			for (final ListIterator<Integer> matrixRowCellIter = matrixRow.listIterator(); matrixRowCellIter
					.hasNext();) {
				final int colIdx = matrixRowCellIter.nextIndex();
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

		final Iterable<? extends Entry<? extends Entry<? extends Image, ImageViewInfo>, SpatialMap.Region>> piecePlacementIter = piecePlacements
				.elementRegions();
		for (final Entry<? extends Entry<? extends Image, ImageViewInfo>, SpatialMap.Region> piecePlacement : piecePlacementIter) {
			final Entry<? extends Image, ImageViewInfo> pieceDisplayInfo = piecePlacement.getKey();
			final Image initialImg = pieceDisplayInfo.getKey();
			final SpatialMap.Region region = piecePlacement.getValue();
			final Image scaledImg = scaleImageToGridSize(initialImg, region, colWidth, rowHeight);
			final ImageViewInfo viewInfo = pieceDisplayInfo.getValue();
			final ImageVisualizationInfo visualizationInfo = viewInfo.getVisualization();

			// NOTE: occupied region is denoted in matrix rows*columns
			// "y" is matrix columns, thus left-to-right
			final int imgStartX = colWidth * region.getYLowerBound();
			// "x" is matrix rows, thus top-to-bottom
			final int imgStartY = rowHeight * region.getXLowerBound();
			g.drawImage(scaledImg, imgStartX, imgStartY, null);
		}
	}

	private int getGridColWidth() {
		final int[] matrixDims = posMatrix.getDimensions();
		return getWidth() / matrixDims[1];
	}

	private int getGridRowHeight() {
		final int[] matrixDims = posMatrix.getDimensions();
		return getHeight() / matrixDims[0];
	}

	private void scaleImage(final Image img, final ImageViewInfo viewInfo, final SpatialMap.Region occupiedGridRegion) {
		final RasterizationInfo rasterizationInfo = viewInfo.getRasterization();
		final ImageVisualizationInfo visualizationInfo = viewInfo.getVisualization();
		final ImageSize size = visualizationInfo.getSize();
		// NOTE: occupied region is denoted in matrix rows*columns
		// "y" is matrix columns, thus left-to-right
		final int colWidth = getGridColWidth();
		final int regionGridColCount = occupiedGridRegion.getLengthY();
		LOGGER.debug("Image occupies {} grid column(s).", regionGridColCount);
		final int imgWidth = colWidth * regionGridColCount;
		final int rowHeight = getGridRowHeight();
		// "x" is matrix rows, thus top-to-bottom
		final int regionGridRowCount = occupiedGridRegion.getLengthX();
		LOGGER.debug("Image occupies {} grid row(s).", regionGridRowCount);
		final int imgHeight = rowHeight * regionGridRowCount;
	}

	/**
	 *
	 */
	void notifyContinue() {
		LOGGER.debug("Notified of continue event.");
		// TODO Auto-generated method stub

	}

	/**
	 *
	 */
	void notifyUndo() {
		LOGGER.debug("Notified of undo event.");
		// TODO Auto-generated method stub

	}

}
