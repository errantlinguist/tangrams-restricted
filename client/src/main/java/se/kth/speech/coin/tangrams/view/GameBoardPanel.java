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
import java.awt.Toolkit;
import java.awt.image.BufferedImage;
import java.awt.image.FilteredImageSource;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.net.URL;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashSet;
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
import javax.swing.JOptionPane;
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

	private static int[] createComponentCoordSizeArray(final SpatialMap.Region region, final int colWidth,
			final int rowHeight) {
		final int regionGridColCount = region.getLengthY();
		LOGGER.debug("Region occupies {} grid column(s).", regionGridColCount);
		final int width = colWidth * regionGridColCount;
		// "x" is matrix rows, thus top-to-bottom
		final int regionGridRowCount = region.getLengthX();
		LOGGER.debug("Region occupies {} grid row(s).", regionGridRowCount);
		final int height = rowHeight * regionGridRowCount;
		return new int[] { width, height };
	}

	private static int[] createComponentCoordStartIdxArray(final SpatialMap.Region region, final int colWidth,
			final int rowHeight) {
		// NOTE: occupied region is denoted in matrix rows*columns
		// "y" is matrix columns, thus left-to-right
		final int startX = colWidth * region.getYLowerBound();
		// "x" is matrix rows, thus top-to-bottom
		final int startY = rowHeight * region.getXLowerBound();
		return new int[] { startX, startY };
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
		final int[] size = createComponentCoordSizeArray(occupiedGridRegion, colWidth, rowHeight);
		return img.getScaledInstance(size[0], size[1], IMG_SCALING_HINTS);
	}

	private final Map<ImageViewInfo, Image> pieceImgs;

	private transient final SpatialMap<ImageViewInfo> piecePlacements;

	private final SpatialMatrix<Integer> posMatrix;

	private final BiFunction<? super Image, ? super GameBoardPanel, ? extends Image> postColoringImgTransformer;

	private final Map<ImageViewInfo, Integer> pieceIds;

	private final Function<? super ImageViewInfo, int[]> piecePosMatrixSizeFactory;

	private transient final Function<SpatialMap.Region, Set<SpatialMap.Region>> newRegionPossibleMoveSetFactory;

	/**
	 * At most one region should be highlighted at a time per player
	 */
	private final Set<SpatialMap.Region> highlightedRegions = Sets.newHashSetWithExpectedSize(1);

	GameBoardPanel(final Collection<ImageVisualizationInfo> imgVisualizationInfoData, final Random rnd,
			final int maxImgPlacements, final int maxPlacementRetriesPerImg, final boolean allowFailedPlacements) {
		this(imgVisualizationInfoData, rnd, maxImgPlacements, maxPlacementRetriesPerImg, allowFailedPlacements,
				DEFAULT_POST_COLORING_IMG_TRANSFORMER);
	};

	GameBoardPanel(final Collection<ImageVisualizationInfo> imgVisualizationInfoData, final Random rnd,
			final int maxImgPlacements, final int maxPlacementRetriesPerImg, final boolean allowFailedPlacements,
			final BiFunction<? super Image, ? super GameBoardPanel, ? extends Image> postColoringImgTransformer) {
		this.postColoringImgTransformer = postColoringImgTransformer;
		pieceIds = Maps.newHashMapWithExpectedSize(maxImgPlacements * 2);
		piecePosMatrixSizeFactory = new CachingPieceMatrixBoundsArrayFactory();
		final Function<ImageViewInfo, Integer> incrementingPieceIdGetter = piece -> pieceIds.computeIfAbsent(piece,
				k -> pieceIds.size());
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
					return Sets.newHashSetWithExpectedSize(
							posMatrix.getPosMatrix().getValues().size() / occupiedRegionArea);
				};
				final RandomMatrixImagePositionFiller<Integer> matrixFiller = new RandomMatrixImagePositionFiller<>(
						posMatrix, incrementingPieceIdGetter, rnd, maxImgPlacements, maxPlacementRetriesPerImg,
						allowFailedPlacements, piecePosMatrixSizeFactory);
				piecePlacements = matrixFiller.apply(pieceImgs.keySet());
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
		{
			final Graphics2D regionHighlightingG = (Graphics2D) g.create();
			regionHighlightingG.setStroke(new BasicStroke(3.0f));
			regionHighlightingG.setColor(Color.MAGENTA);
			try {
				drawRegionHighlights(regionHighlightingG);
			} finally {
				regionHighlightingG.dispose();
			}
		}

	}

	private void clearRegion(final SpatialMap.Region occupiedRegion) {
		final Collection<ImageViewInfo> pieces = piecePlacements.getMinimalRegionElements().get(occupiedRegion);
		// NOTE: Iterator.remove() for the instance returned by the
		// multimap's collection iterator throws a
		// ConcurrentModificationException
		pieces.clear();
		posMatrix.setPositionValues(occupiedRegion, null);
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

	private Map<ImageViewInfo, SpatialMap.Region> createRandomValidMoveTargetMap(final SpatialMap.Region occupiedRegion,
			final Random rnd) {
		final Map<Region, Set<Region>> validMoves = createValidMoveMap();
		final Map<ImageViewInfo, SpatialMap.Region> result;
		final Set<SpatialMap.Region> regionValidMoves = validMoves.computeIfAbsent(occupiedRegion,
				newRegionPossibleMoveSetFactory);
		if (regionValidMoves.isEmpty()) {
			result = Collections.emptyMap();
		} else {
			final Collection<ImageViewInfo> pieces = piecePlacements.getMinimalRegionElements().get(occupiedRegion);
			// NOTE: The iterator should only have one element here
			result = Maps.newHashMapWithExpectedSize(pieces.size());
			for (final ImageViewInfo piece : pieces) {
				final SpatialMap.Region moveTarget = RandomCollections.getRandomElement(regionValidMoves, rnd);
				assert !piecePlacements.isOccupied(moveTarget);
				result.put(piece, moveTarget);
			}
		}
		return result;
	}

	private Map<SpatialMap.Region, Set<SpatialMap.Region>> createValidMoveMap() {
		final Matrix<?> backingMatrix = posMatrix.getPosMatrix();
		final Set<SpatialMap.Region> regionElements = piecePlacements.getMinimalRegionElements().keySet();
		final Map<SpatialMap.Region, Set<SpatialMap.Region>> result = Maps
				.newHashMapWithExpectedSize(regionElements.size());
		final int[] matrixDims = backingMatrix.getDimensions();

		for (final SpatialMap.Region occupiedRegion : regionElements) {
			final Set<SpatialMap.Region> possibleMoveRegions = result.computeIfAbsent(occupiedRegion,
					newRegionPossibleMoveSetFactory);
			final int maxXLowerBound = matrixDims[0] - occupiedRegion.getLengthX();
			final int maxYLowerBound = matrixDims[1] - occupiedRegion.getLengthY();
			for (int xLowerBound = 0; xLowerBound < maxXLowerBound; xLowerBound++) {
				final int xUpperBound = xLowerBound + occupiedRegion.getLengthX();
				for (int yLowerBound = 0; yLowerBound < maxYLowerBound; yLowerBound++) {
					final int yUpperBound = yLowerBound + occupiedRegion.getLengthY();
					final Stream<?> possibleMoveRegionValues = backingMatrix.getValues(xLowerBound, xUpperBound,
							yLowerBound, yUpperBound);
					if (possibleMoveRegionValues.allMatch(Objects::isNull)) {
						final SpatialMap.Region possibleMoveRegion = posMatrix.getRegion(xLowerBound, xUpperBound,
								yLowerBound, yUpperBound);
						possibleMoveRegions.add(possibleMoveRegion);
					} else {
						assert piecePlacements.isOccupied(occupiedRegion);
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
			// final int rowIdx = matrixRowIter.nextIndex();

			int nextColX = 0;
			for (final ListIterator<Integer> matrixRowCellIter = matrixRow.listIterator(); matrixRowCellIter
					.hasNext();) {
				// final int colIdx = matrixRowCellIter.nextIndex();
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
			final int[] startIdxs = createComponentCoordStartIdxArray(region, colWidth, rowHeight);
			g.drawImage(scaledImg, startIdxs[0], startIdxs[1], null);
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

	private void placePieces(final Map<ImageViewInfo, SpatialMap.Region> pieceMoveTargets) {
		for (final Entry<ImageViewInfo, SpatialMap.Region> pieceMoveTarget : pieceMoveTargets.entrySet()) {
			final ImageViewInfo piece = pieceMoveTarget.getKey();
			final Integer pieceId = pieceIds.get(piece);
			final SpatialMap.Region moveTarget = pieceMoveTarget.getValue();
			LOGGER.debug("Moving piece \"{}\" to {}.", pieceId, moveTarget);
			assert !piecePlacements.isOccupied(moveTarget);
			posMatrix.setPositionValues(moveTarget, pieceId);
			piecePlacements.put(piece, moveTarget);
		}
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
		final List<SpatialMap.Region> regionsToTry = piecePlacements.getMinimalRegions();
		// TODO: estimate number of failed tries
		final Set<SpatialMap.Region> failedRegions = new HashSet<>();
		Entry<SpatialMap.Region, Map<ImageViewInfo, SpatialMap.Region>> pieceMove = null;
		do {
			final SpatialMap.Region occupiedRegion = RandomCollections.getRandomElement(regionsToTry, rnd);
			final Map<ImageViewInfo, SpatialMap.Region> pieceMoveTargets = createRandomValidMoveTargetMap(
					occupiedRegion, rnd);
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
			final SpatialMap.Region occupiedRegion = pieceMove.getKey();
			final Map<ImageViewInfo, SpatialMap.Region> pieceMoveTargets = pieceMove.getValue();
			highlightedRegions.add(occupiedRegion);
			placePieces(pieceMoveTargets);
			clearRegion(occupiedRegion);
			LOGGER.debug("Finished randomly moving piece(s) at {}.", occupiedRegion);
			repaint();
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
