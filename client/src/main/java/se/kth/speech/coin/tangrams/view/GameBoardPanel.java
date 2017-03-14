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
import java.awt.Dimension;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Stroke;
import java.awt.Toolkit;
import java.awt.image.BufferedImage;
import java.awt.image.FilteredImageSource;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Random;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.IntSupplier;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import javax.imageio.ImageIO;
import javax.swing.JPanel;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Maps;
import com.google.common.collect.Sets;

import se.kth.speech.MathDenominators;
import se.kth.speech.Matrix;
import se.kth.speech.MutablePair;
import se.kth.speech.RandomCollections;
import se.kth.speech.SpatialMap;
import se.kth.speech.SpatialMap.Region;
import se.kth.speech.SpatialMatrix;
import se.kth.speech.awt.ColorReplacementImageFilter;
import se.kth.speech.coin.tangrams.content.ImageSize;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;
import se.kth.speech.coin.tangrams.view.ImageViewInfo.RasterizationInfo;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 2 Mar 2017
 *
 */
final class GameBoardPanel extends JPanel {

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

	private static final BiFunction<Image, GameBoardPanel, Image> DEFAULT_POST_COLORING_IMG_TRANSFORMER = new BiFunction<Image, GameBoardPanel, Image>() {

		@Override
		public Image apply(final Image img, final GameBoardPanel gameBoardPanel) {
			// Do nothing
			LOGGER.debug("Created instance {} for {}.", img, gameBoardPanel);
			return img;
		}

	};

	private static final int IMG_SCALING_HINTS = Image.SCALE_SMOOTH;

	private static final Logger LOGGER = LoggerFactory.getLogger(GameBoardPanel.class);

	/**
	 *
	 */
	private static final long serialVersionUID = 6258829324465894025L;

	private static final Collector<CharSequence, ?, String> TABLE_ROW_CELL_JOINER;

	private static final String TABLE_STRING_REPR_COL_DELIMITER;

	private static final String TABLE_STRING_REPR_ROW_DELIMITER = System.lineSeparator();

	static {
		TABLE_STRING_REPR_COL_DELIMITER = "\t";
		TABLE_ROW_CELL_JOINER = Collectors.joining(TABLE_STRING_REPR_COL_DELIMITER);
	}

	private static void appendRowTableRepr(final int rowIdx, final Iterator<?> rowCellIter, final StringBuilder sb) {
		sb.append(rowIdx);
		final String nullValRepr = "-";
		while (rowCellIter.hasNext()) {
			sb.append(TABLE_STRING_REPR_COL_DELIMITER);
			final Object next = rowCellIter.next();
			final String nextRepr = Objects.toString(next, nullValRepr);
			sb.append(nextRepr);
		}
	}

	private static String createImageInfoTable(
			final Collection<? extends Entry<?, ? extends Entry<ImageViewInfo.RasterizationInfo, ?>>> namedImgValData) {
		final Stream<String> colNames = Stream.of("PATH", "WIDTH", "HEIGHT", "GCD", "COMMENT");
		final String header = colNames.collect(TABLE_ROW_CELL_JOINER);
		final StringBuilder sb = new StringBuilder(header.length() + 16 * namedImgValData.size());
		sb.append(header);
		for (final Entry<?, ? extends Entry<ImageViewInfo.RasterizationInfo, ?>> namedImgValDatumComments : namedImgValData) {
			sb.append(TABLE_STRING_REPR_ROW_DELIMITER);
			sb.append(namedImgValDatumComments.getKey());
			sb.append(TABLE_STRING_REPR_COL_DELIMITER);
			final Entry<ImageViewInfo.RasterizationInfo, ?> imgValDatumComments = namedImgValDatumComments.getValue();
			final ImageViewInfo.RasterizationInfo imgVisualizationInfoDatum = imgValDatumComments.getKey();
			sb.append(imgVisualizationInfoDatum.getWidth());
			sb.append(TABLE_STRING_REPR_COL_DELIMITER);
			sb.append(imgVisualizationInfoDatum.getHeight());
			sb.append(TABLE_STRING_REPR_COL_DELIMITER);
			sb.append(imgVisualizationInfoDatum.getGcd());
			sb.append(TABLE_STRING_REPR_COL_DELIMITER);
			sb.append(imgValDatumComments.getValue());
		}
		return sb.toString();
	}

	private static String createMatrixReprString(final Matrix<?> matrix) {
		final Stream<String> colNames = Stream.of("ROW_IDX", "COL_IDXS...");
		final String header = colNames.collect(TABLE_ROW_CELL_JOINER);

		final int[] dims = matrix.getDimensions();
		final Stream.Builder<String> subheaderBuilder = Stream.builder();
		subheaderBuilder.accept("");
		IntStream.range(0, dims[1]).mapToObj(Integer::toString).forEach(subheaderBuilder);
		final String subHeader = TABLE_STRING_REPR_ROW_DELIMITER
				+ subheaderBuilder.build().collect(TABLE_ROW_CELL_JOINER);
		final int cellCount = matrix.getValues().size();
		final StringBuilder sb = new StringBuilder(header.length() + subHeader.length() + cellCount * 16);
		sb.append(header);
		sb.append(subHeader);
		final Iterator<? extends List<?>> rowIter = matrix.rowIterator();
		int rowIdx = 0;
		if (rowIter.hasNext()) {
			do {
				sb.append(TABLE_STRING_REPR_ROW_DELIMITER);
				final List<?> row = rowIter.next();
				appendRowTableRepr(rowIdx++, row.iterator(), sb);
			} while (rowIter.hasNext());
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

	private final Map<ImageViewInfo, Image> pieceImgs;

	private transient final SpatialMap<ImageViewInfo> piecePlacements;

	private final SpatialMatrix<Integer> posMatrix;

	private final BiFunction<? super Image, ? super GameBoardPanel, ? extends Image> postColoringImgTransformer;

	private final Map<SpatialMap.Region, Set<SpatialMap.Region>> validMoves;

	// TODO: Finish
	// private final Function<? super ImageViewInfo, int[]>
	// piecePosMatrixSizeFactory = new CachingPieceMatrixBoundsArrayFactory();

	// private final Function<? super SpatialMap.Region, int[]>
	// regionDimensionFactory;

	// private void findValidMoves(int[] regionDims){
	//
	// }
	//

	private final Map<ImageViewInfo, Integer> pieceIds;

	private final Function<? super ImageViewInfo, int[]> piecePosMatrixSizeFactory;

	private transient final Function<SpatialMap.Region, Set<SpatialMap.Region>> newRegionPossibleMoveSetFactory;

	GameBoardPanel(final Collection<ImageVisualizationInfo> imgVisualizationInfoData, final Random rnd,
			final int maxImgPlacements, final int maxPlacementRetriesPerImg, final boolean allowFailedPlacements) {
		this(imgVisualizationInfoData, rnd, maxImgPlacements, maxPlacementRetriesPerImg, allowFailedPlacements,
				DEFAULT_POST_COLORING_IMG_TRANSFORMER);
	}

	GameBoardPanel(final Collection<ImageVisualizationInfo> imgVisualizationInfoData, final Random rnd,
			final int maxImgPlacements, final int maxPlacementRetriesPerImg, final boolean allowFailedPlacements,
			final BiFunction<? super Image, ? super GameBoardPanel, ? extends Image> postColoringImgTransformer) {
		this.postColoringImgTransformer = postColoringImgTransformer;
		pieceIds = Maps.newHashMapWithExpectedSize(maxImgPlacements * 2);
		piecePosMatrixSizeFactory = new CachingPieceMatrixBoundsArrayFactory();
		final Function<ImageViewInfo, Integer> incrementingPieceIdGetter = piece -> pieceIds.computeIfAbsent(piece,
				k -> pieceIds.size());
		final RandomMatrixImagePositionFiller<Integer> matrixFiller = new RandomMatrixImagePositionFiller<>(
				incrementingPieceIdGetter, rnd, maxImgPlacements, maxPlacementRetriesPerImg, allowFailedPlacements,
				piecePosMatrixSizeFactory);
		// The minimum accepted length of the shortest dimension for an image
		final int minDimLength = 300;
		final SizeValidator validator = new SizeValidator(minDimLength, 50, 50);
		try {
			pieceImgs = createImageViewInfoMap(imgVisualizationInfoData, validator);
			final Set<Integer> dimensionValues = Sets.newHashSetWithExpectedSize(pieceImgs.size() + 1);
			for (final ImageViewInfo pieceImgViewInfo : pieceImgs.keySet()) {
				dimensionValues.add(pieceImgViewInfo.getRasterization().getGcd());
			}
			final Dimension boardSize = new Dimension(minDimLength * 5, minDimLength * 4);
			setPreferredSize(boardSize);
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
				final Matrix<Integer> backingPosMatrix = new Matrix<>(posMatrixBackingArray, posMatrixCols);
				if (!isDimensionDivisibleIntoGrid(boardSize, backingPosMatrix)) {
					throw new IllegalArgumentException(
							String.format("Board %s not divisble into matrix with dimensions %s.", boardSize,
									Arrays.toString(backingPosMatrix.getDimensions())));
				}
				posMatrix = new SpatialMatrix<>(backingPosMatrix);
				newRegionPossibleMoveSetFactory = region -> {
					final int occupiedRegionArea = region.getLengthX() * region.getLengthY();
					return Sets.newHashSetWithExpectedSize(posMatrix.getPosMatrix().getValues().size() / occupiedRegionArea);
				};
				piecePlacements = matrixFiller.apply(posMatrix, pieceImgs.keySet());
				validMoves = createValidMoveMap(backingPosMatrix, piecePlacements.getMinimalRegionElements().keySet());
				// Finished with creating necessary data structures
				System.out.println("IMAGE PLACEMENTS");
				System.out.println(createMatrixReprString(backingPosMatrix));
			} else {
				throw new IllegalArgumentException(
						String.format("The board as a whole failed validation with dimensions %s; and GCD %d: %s",
								boardSize, greatestCommonDenominator, boardValidationComments));
			}

		} catch (final IOException e) {
			throw new UncheckedIOException(e);
		}
	}

	@Override
	public void paintComponent(final Graphics g) {
		super.paintComponent(g);
		// Draw a grid (for debugging/devel)
		drawGrid(g);
		drawPieceIds(g);

		drawPieceImages(g);

	}

	private LinkedHashMap<ImageViewInfo, Image> createImageViewInfoMap(
			final Collection<ImageVisualizationInfo> imgVisualizationInfoData, final SizeValidator validator)
			throws IOException {
		// Use linked map in order to preserve iteration order in provided
		// sequence
		final LinkedHashMap<ImageViewInfo, Image> result = Maps
				.newLinkedHashMapWithExpectedSize(imgVisualizationInfoData.size());

		final Map<URL, Entry<ImageViewInfo.RasterizationInfo, Set<SizeValidator.ValidationComment>>> badImgs = Maps
				.newHashMapWithExpectedSize(0);
		final Toolkit toolkit = getToolkit();
		for (final ImageVisualizationInfo imgVisualizationInfoDatum : imgVisualizationInfoData) {
			final URL imgResourceLoc = imgVisualizationInfoDatum.getResourceLoc();
			final BufferedImage initialImg = ImageIO.read(imgResourceLoc);
			final IntSupplier widthGetter = initialImg::getWidth;
			final IntSupplier heightGetter = initialImg::getHeight;
			{
				// Size/aspect ratio calculation
				final ImageViewInfo.RasterizationInfo imgRasterizationInfo = new ImageViewInfo.RasterizationInfo(
						widthGetter, heightGetter);
				{
					// Validate image
					final Set<SizeValidator.ValidationComment> validationComments = validator.validate(
							widthGetter.getAsInt(), heightGetter.getAsInt(),
							MathDenominators.gcd(widthGetter.getAsInt(), heightGetter.getAsInt()));
					if (validationComments.isEmpty()) {
						final Image coloredImg = toolkit.createImage(new FilteredImageSource(initialImg.getSource(),
								new ColorReplacementImageFilter(imgVisualizationInfoDatum.getColor())));
						final ImageViewInfo imgInfo = new ImageViewInfo(imgVisualizationInfoDatum,
								imgRasterizationInfo);
						final Image transformedImg = postColoringImgTransformer.apply(coloredImg, this);
						final Image oldImg = result.put(imgInfo, transformedImg);
						assert oldImg == null : String.format("Key already found in map: %s", imgInfo);
					} else {
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

	private Stream<Entry<Image, ImageViewInfo>> createImageViewInfoStream(
			final Stream<ImageVisualizationInfo> imgVisualizationInfoData, final SizeValidator validator) {
		return imgVisualizationInfoData.flatMap(imgVisualizationInfoDatum -> {
			final Stream<Entry<Image, ImageViewInfo>> singleResult;

			final URL imgResourceLoc = imgVisualizationInfoDatum.getResourceLoc();
			try {
				final BufferedImage initialImg = ImageIO.read(imgResourceLoc);
				final IntSupplier widthGetter = initialImg::getWidth;
				final IntSupplier heightGetter = initialImg::getHeight;

				{
					// Size/aspect ratio calculation
					final ImageViewInfo.RasterizationInfo imgRasterizationInfo = new ImageViewInfo.RasterizationInfo(
							widthGetter, heightGetter);
					final Image coloredImg = getToolkit().createImage(new FilteredImageSource(initialImg.getSource(),
							new ColorReplacementImageFilter(imgVisualizationInfoDatum.getColor())));
					{
						// Validate image
						final Set<SizeValidator.ValidationComment> validationComments = validator.validate(
								widthGetter.getAsInt(), heightGetter.getAsInt(),
								MathDenominators.gcd(widthGetter.getAsInt(), heightGetter.getAsInt()));
						if (validationComments.isEmpty()) {
							singleResult = Stream.of(new MutablePair<>(coloredImg,
									new ImageViewInfo(imgVisualizationInfoDatum, imgRasterizationInfo)));
						} else {
							singleResult = Stream.empty();
						}
					}
				}
			} catch (final IOException e) {
				throw new UncheckedIOException(e);
			}

			return singleResult;
		});
	};

	private Map<SpatialMap.Region, Set<SpatialMap.Region>> createValidMoveMap(final Matrix<?> posMatrix,
			final Set<SpatialMap.Region> regionElements) {
		final Map<SpatialMap.Region, Set<SpatialMap.Region>> result = Maps
				.newHashMapWithExpectedSize(regionElements.size());
		final int[] matrixDims = posMatrix.getDimensions();

		for (final SpatialMap.Region occupiedRegion : regionElements) {
			final Set<SpatialMap.Region> possibleMoveRegions = result.computeIfAbsent(occupiedRegion,
					newRegionPossibleMoveSetFactory);
			final int maxXLowerBound = matrixDims[0] - occupiedRegion.getLengthX();
			final int maxYLowerBound = matrixDims[1] - occupiedRegion.getLengthY();
			for (int xLowerBound = 0; xLowerBound < maxXLowerBound; xLowerBound++) {
				final int xUpperBound = xLowerBound + occupiedRegion.getLengthX();
				for (int yLowerBound = 0; yLowerBound < maxYLowerBound; yLowerBound++) {
					final int yUpperBound = yLowerBound + occupiedRegion.getLengthY();
					final Stream<?> possibleMoveRegionValues = posMatrix.getValues(xLowerBound, xUpperBound,
							yLowerBound, yUpperBound);
					if (possibleMoveRegionValues.allMatch(Objects::isNull)) {
						final SpatialMap.Region possibleMoveRegion = new SpatialMap.Region(xLowerBound, xUpperBound,
								yLowerBound, yUpperBound);
						possibleMoveRegions.add(possibleMoveRegion);
					} else {
						if (LOGGER.isDebugEnabled()) {
							LOGGER.debug("Found occupied space at {}.",
									Arrays.toString(new int[] { xLowerBound, xUpperBound, yLowerBound, yUpperBound }));
						}
					}
				}

			}
		}
		return result;
	}

	private void drawGrid(final Graphics g) {
		LOGGER.debug("Drawing grid.");
		final Matrix<Integer> posMatrix = this.posMatrix.getPosMatrix();
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
		for (final ListIterator<List<Integer>> matrixRowIter = posMatrix.getPosMatrix().rowIterator(); matrixRowIter
				.hasNext();) {
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

		for (final Entry<Region, ImageViewInfo> piecePlacement : piecePlacements.getMinimalRegionElements().entries()) {
			final SpatialMap.Region region = piecePlacement.getKey();
			final ImageViewInfo pieceViewInfo = piecePlacement.getValue();
			final Image initialImg = pieceImgs.get(pieceViewInfo);
			final Image scaledImg = scaleImageToGridSize(initialImg, region, colWidth, rowHeight);
			final ImageVisualizationInfo visualizationInfo = pieceViewInfo.getVisualization();

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

	private synchronized Entry<SpatialMap.Region, Map<Integer, SpatialMap.Region>> moveRandomPiece(final Random rnd) {
		// TODO: Change probability of a piece being selected for moving based
		// on if it was moved before: E.g. cannot move a given piece more than
		// twice in a row
		final SpatialMap.Region occupiedRegion = RandomCollections.getRandomElement(piecePlacements.getMinimalRegions(),
				rnd);
		final Map<Integer, SpatialMap.Region> newPositions;
		final Set<SpatialMap.Region> possibleMoves = validMoves.computeIfAbsent(occupiedRegion,
				newRegionPossibleMoveSetFactory);
		if (possibleMoves.isEmpty()) {
			newPositions = Collections.emptyMap();
		} else {
			final Collection<ImageViewInfo> pieces = piecePlacements.getMinimalRegionElements().get(occupiedRegion);
			final Iterator<ImageViewInfo> pieceIter = pieces.iterator();
			// NOTE: The iterator should only have one element here
			newPositions = Maps.newHashMapWithExpectedSize(pieces.size());
			while (pieceIter.hasNext()) {
				final ImageViewInfo piece = pieceIter.next();
				final Integer pieceId = pieceIds.get(piece);
				LOGGER.info("Moving piece \"{}\" to a random location.", pieceId);
				// FIXME: THe system is not detecting occupied areas correctly
				// FIXME: Update map of occupied regions
				final SpatialMap.Region moveTarget = RandomCollections.getRandomElement(possibleMoves, rnd);
				posMatrix.setPositionValues(moveTarget, pieceId);
				piecePlacements.put(piece, moveTarget);
				posMatrix.setPositionValues(occupiedRegion, null);
				newPositions.put(pieceId, moveTarget);
				pieceIter.remove();
			}
		}
		return new MutablePair<>(occupiedRegion, newPositions);
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
	void notifyContinue(final Random rnd) {
		LOGGER.debug("Notified of continue event.");
		LOGGER.info("Moving random piece.");
		final Entry<Region, Map<Integer, Region>> newPlace = moveRandomPiece(rnd);
		LOGGER.info("Finished randomly moving piece(s) at {} to {}.", newPlace.getKey(), newPlace.getValue());
		repaint();
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
