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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumMap;
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

import se.kth.speech.IntArrays;
import se.kth.speech.MathDivisors;
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

	private static class PositionGridSizeSummary {
		private final int[] maxImgGridSize;

		private final int[] minImgGridSize;

		private final int totalImgGridArea;

		private final List<Integer> commonDivisors;

		private PositionGridSizeSummary(final int[] minImgGridSize, final int[] maxImgGridSize,
				final int totalImgGridArea, final List<Integer> commonDivisors) {
			this.minImgGridSize = minImgGridSize;
			this.maxImgGridSize = maxImgGridSize;
			this.totalImgGridArea = totalImgGridArea;
			this.commonDivisors = commonDivisors;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.lang.Object#toString()
		 */
		@Override
		public String toString() {
			final StringBuilder builder = new StringBuilder();
			builder.append("PositionGridSizeSummary [minImgGridSize=");
			builder.append(Arrays.toString(minImgGridSize));
			builder.append(", maxImgGridSize=");
			builder.append(Arrays.toString(maxImgGridSize));
			builder.append(", totalImgGridArea=");
			builder.append(totalImgGridArea);
			builder.append(", commonDivisors=");
			builder.append(commonDivisors);
			builder.append("]");
			return builder.toString();
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

	private static final int IMG_PADDING;

	private static final int IMG_SCALING_HINTS = Image.SCALE_SMOOTH;

	private static final Logger LOGGER = LoggerFactory.getLogger(GameBoardPanel.class);

	private static final int MIN_GRID_SQUARE_LENGTH;

	private static final int REGION_HIGHLIGHT_STROKE_WIDTH;

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

	static {
		REGION_HIGHLIGHT_STROKE_WIDTH = 2;
		IMG_PADDING = REGION_HIGHLIGHT_STROKE_WIDTH * 2;
		MIN_GRID_SQUARE_LENGTH = 10 + IMG_PADDING;
	}

	private static final Function<ImageSize, Integer> IMAGE_SIZE_FACTORS = createImageSizeFactorMap()::get;

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

	private static Map<ImageSize, Integer> createImageSizeFactorMap() {
		final Map<ImageSize, Integer> result = new EnumMap<>(ImageSize.class);
		final Iterator<ImageSize> sizes = ImageSize.getSizeOrdering().iterator();
		result.put(sizes.next(), 1);
		result.put(sizes.next(), 2);
		result.put(sizes.next(), 3);
		assert result.size() == ImageSize.values().length;
		return result;
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

	private static IntStream createMinimumDimLengths(final int[] dims) {
		return Arrays.stream(dims).map(dim -> dim * MIN_GRID_SQUARE_LENGTH);
	}

	private static int[] createPositionGridSize(final PositionGridSizeSummary posGridSizeSummary,
			final Dimension maxBoardSize, final double occupiedGridArea) {
		// NOTE: "rows" in the matrix go top-bottom and "cols" go
		// left-right
		int[] result = null;
		final List<Integer> commonDivisors = posGridSizeSummary.commonDivisors;
		LOGGER.info("Trying common divisors {} for board dimensions {}.", commonDivisors, maxBoardSize);
		for (final ListIterator<Integer> imgCommonDivisorIter = commonDivisors
				.listIterator(posGridSizeSummary.commonDivisors.size()); imgCommonDivisorIter.hasPrevious();) {
			final int nextGreatestCommonDivisor = imgCommonDivisorIter.previous();
			if (maxBoardSize.height % nextGreatestCommonDivisor != 0) {
				LOGGER.info("Board height ({}) not divisible by {}.", maxBoardSize.height, nextGreatestCommonDivisor);
			} else {
				final int rows = maxBoardSize.height / nextGreatestCommonDivisor;
				if (maxBoardSize.width % nextGreatestCommonDivisor != 0) {
					LOGGER.info("Board width ({}) not divisible by {}.", maxBoardSize.width, nextGreatestCommonDivisor);
				} else {
					final int cols = maxBoardSize.width / nextGreatestCommonDivisor;
					LOGGER.info("Trying to size the board using a grid of size {}*{}.", rows, cols);
					if (rows < posGridSizeSummary.maxImgGridSize[0]) {
						LOGGER.info("Too few rows ({}) to accommodate biggest image (with a row count of {}).", rows,
								posGridSizeSummary.maxImgGridSize[0]);
					} else if (cols < posGridSizeSummary.maxImgGridSize[1]) {
						LOGGER.info("Too few columns ({}) to accommodate biggest image (with a column count of {}).",
								cols, posGridSizeSummary.maxImgGridSize[1]);
					} else {
						final int totalBoardGridArea = rows * cols;
						final int allowedOccupiedBoardArea = (int) (totalBoardGridArea * occupiedGridArea);
						if (allowedOccupiedBoardArea < posGridSizeSummary.totalImgGridArea) {
							LOGGER.info(
									"Too few grid cells ({} = {} * {}) to accommodate all images (required minimum of {}).",
									new Object[] { allowedOccupiedBoardArea, totalBoardGridArea, occupiedGridArea,
											posGridSizeSummary.totalImgGridArea, });
						} else {
							LOGGER.info("Found valid board size {}*{} (with a common divisor of {}).",
									new Object[] { rows, cols, nextGreatestCommonDivisor });
							result = new int[] { rows, cols };
							break;
						}
					}
				}
			}

		}
		if (result == null) {
			throw new IllegalArgumentException("Could not find a valid board size.");
		}
		return result;
	}

	private static PositionGridSizeSummary createPositionGridSizeSummary(
			final Iterator<ImageViewInfo> imageViewInfoData) {
		final int[] maxImgGridSize = new int[] { Integer.MIN_VALUE, Integer.MIN_VALUE };
		final int[] minImgGridSize = new int[] { Integer.MAX_VALUE, Integer.MAX_VALUE };
		int totalImgGridArea = 0;
		final List<Integer> commonDivisors;
		{
			final ImageViewInfo imgViewInfoDatum = imageViewInfoData.next();
			final int[] imgGridSize = imgViewInfoDatum.getGridSize(IMAGE_SIZE_FACTORS);
			IntArrays.mutate(minImgGridSize, imgGridSize, Math::min);
			IntArrays.mutate(maxImgGridSize, imgGridSize, Math::max);
			final RasterizationInfo rasterInfo = imgViewInfoDatum.getRasterization();
			commonDivisors = MathDivisors.createCommonDivisorList(rasterInfo.getWidth(), rasterInfo.getHeight());
			final int imgGridArea = IntArrays.product(imgGridSize);
			totalImgGridArea += imgGridArea;
		}
		while (imageViewInfoData.hasNext()) {
			final ImageViewInfo imgViewInfoDatum = imageViewInfoData.next();
			final int[] imgGridSize = imgViewInfoDatum.getGridSize(IMAGE_SIZE_FACTORS);
			IntArrays.mutate(minImgGridSize, imgGridSize, Math::min);
			IntArrays.mutate(maxImgGridSize, imgGridSize, Math::max);
			final RasterizationInfo rasterInfo = imgViewInfoDatum.getRasterization();
			MathDivisors.removeNonDivisors(commonDivisors.iterator(), rasterInfo.getWidth(), rasterInfo.getHeight());
			final int imgGridArea = IntArrays.product(imgGridSize);
			totalImgGridArea += imgGridArea;
		}
		LOGGER.debug("Common divisors for all images are {}.", commonDivisors);
		return new PositionGridSizeSummary(minImgGridSize, maxImgGridSize, totalImgGridArea, commonDivisors);
	}

	private static boolean isDimensionDivisibleIntoGrid(final Dimension dim, final Matrix<?> matrix) {
		final int[] matrixDims = matrix.getDimensions();
		return dim.getHeight() % matrixDims[0] == 0 && dim.getWidth() % matrixDims[1] == 0;
	}

	private static Image scaleImageToGridSize(final Image img, final SpatialMap.Region occupiedGridRegion,
			final int colWidth, final int rowHeight) {
		final int[] size = createComponentCoordSizeArray(occupiedGridRegion, colWidth, rowHeight);
		return img.getScaledInstance(Math.max(MIN_GRID_SQUARE_LENGTH, size[0] - IMG_PADDING),
				Math.max(MIN_GRID_SQUARE_LENGTH, size[1] - IMG_PADDING), IMG_SCALING_HINTS);
	}

	/**
	 * At most one region should be highlighted at a time per player
	 */
	private final Set<SpatialMap.Region> highlightedRegions = Sets.newHashSetWithExpectedSize(1);

	private final Map<ImageViewInfo, Image> pieceImgs;

	private final SpatialMatrix<Integer, ImageViewInfo> posMatrix;

	private final BiFunction<? super Image, ? super GameBoardPanel, ? extends Image> postColoringImgTransformer;

	GameBoardPanel(final Collection<ImageVisualizationInfo> imgVisualizationInfoData, final Random rnd,
			final int maxImgPlacements, final int maxPlacementRetriesPerImg, final double occupiedGridArea,
			final boolean allowFailedPlacements, final int uniqueImgResourceCount) {
		this(imgVisualizationInfoData, rnd, maxImgPlacements, maxPlacementRetriesPerImg, occupiedGridArea,
				allowFailedPlacements, uniqueImgResourceCount, DEFAULT_POST_COLORING_IMG_TRANSFORMER);
	}

	GameBoardPanel(final Collection<ImageVisualizationInfo> imgVisualizationInfoData, final Random rnd,
			final int maxImgPlacements, final int maxPlacementRetriesPerImg, final double occupiedGridArea,
			final boolean allowFailedPlacements, final int uniqueImgResourceCount,
			final BiFunction<? super Image, ? super GameBoardPanel, ? extends Image> postColoringImgTransformer) {
		this.postColoringImgTransformer = postColoringImgTransformer;
		final Map<ImageViewInfo, Integer> pieceIds = Maps.newHashMapWithExpectedSize(maxImgPlacements);
		final Function<ImageViewInfo, Integer> incrementingPieceIdGetter = piece -> pieceIds.computeIfAbsent(piece,
				k -> pieceIds.size());
		pieceImgs = createImageViewInfoMap(imgVisualizationInfoData, uniqueImgResourceCount);
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
			final PositionGridSizeSummary posGridSizeSummary = createPositionGridSizeSummary(
					pieceImgs.keySet().iterator());
			LOGGER.info("Position grid size summary: {}", posGridSizeSummary);
			final int[] gridSize = createPositionGridSize(posGridSizeSummary, boardSize, occupiedGridArea);
			LOGGER.info("Creating a position matrix of size {}.", gridSize);
			final Integer[] posMatrixBackingArray = new Integer[IntArrays.product(gridSize)];
			final Matrix<Integer> backingPosMatrix = new Matrix<>(posMatrixBackingArray, gridSize[1]);
			if (!isDimensionDivisibleIntoGrid(boardSize, backingPosMatrix)) {
				throw new IllegalArgumentException(
						String.format("Board %s not divisble into matrix with dimensions %s.", boardSize,
								Arrays.toString(backingPosMatrix.getDimensions())));
			}
			final Function<? super ImageViewInfo, int[]> piecePosMatrixSizeFactory = imgViewInfo -> imgViewInfo
					.getGridSize(IMAGE_SIZE_FACTORS);
			final Collection<ImageViewInfo> pieces = pieceImgs.keySet();
			final SpatialMap<ImageViewInfo> posMap = new SpatialMap<>(pieces.size());
			posMatrix = new SpatialMatrix<>(backingPosMatrix, pieceIds::get, posMap);
			final RandomMatrixPositionFiller<Integer, ImageViewInfo> matrixFiller = new RandomMatrixPositionFiller<>(
					posMatrix, incrementingPieceIdGetter, rnd, piecePosMatrixSizeFactory);
			matrixFiller.apply(pieces);
			final Collection<ImageViewInfo> allElements = posMap.getAllElements();
			LOGGER.info("Created a spatial map with element IDs {}.", allElements.stream().map(pieceIds::get)
					.collect(Collectors.toCollection(() -> new ArrayList<>(allElements.size()))));
		}
		{
			final int[] minSizeDims = createMinimumDimLengths(posMatrix.getDimensions()).toArray();
			// NOTE: "rows" in the matrix go top-bottom and "cols" go
			// left-right
			final Dimension minSize = new Dimension(minSizeDims[1], minSizeDims[0]);
			LOGGER.debug("Setting minimum component size to {}.", minSize);
			setMinimumSize(minSize);
		}
		// Finished with creating necessary data structures
		System.out.println("IMAGE PLACEMENTS");
		System.out.println(createMatrixReprString(posMatrix.getPositionMatrix()));
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

	private LinkedHashMap<ImageViewInfo, Image> createImageViewInfoMap(
			final Collection<ImageVisualizationInfo> imgVisualizationInfoData, final int uniqueImgResourceCount) {
		// Use linked map in order to preserve iteration order in provided
		// sequence
		final LinkedHashMap<ImageViewInfo, Image> result = Maps
				.newLinkedHashMapWithExpectedSize(imgVisualizationInfoData.size());

		final Toolkit toolkit = getToolkit();
		final Map<URL, BufferedImage> resourceImgs = Maps.newHashMapWithExpectedSize(uniqueImgResourceCount);
		for (final ImageVisualizationInfo imgVisualizationInfoDatum : imgVisualizationInfoData) {
			final URL imgResourceLoc = imgVisualizationInfoDatum.getResourceLoc();
			final BufferedImage initialImg = resourceImgs.computeIfAbsent(imgResourceLoc, loc -> {
				try {
					return ImageIO.read(loc);
				} catch (final IOException e) {
					throw new UncheckedIOException(e);
				}
			});
			final IntSupplier widthGetter = initialImg::getWidth;
			final IntSupplier heightGetter = initialImg::getHeight;
			{
				// Size/aspect ratio calculation
				final ImageViewInfo.RasterizationInfo imgRasterizationInfo = new ImageViewInfo.RasterizationInfo(
						widthGetter, heightGetter);
				{
					final Image coloredImg = toolkit.createImage(new FilteredImageSource(initialImg.getSource(),
							new ColorReplacementImageFilter(imgVisualizationInfoDatum.getColor())));
					final ImageViewInfo imgInfo = new ImageViewInfo(imgVisualizationInfoDatum, imgRasterizationInfo);
					final Image transformedImg = postColoringImgTransformer.apply(coloredImg, this);
					final Image oldImg = result.put(imgInfo, transformedImg);
					assert oldImg == null : String.format("Key already found in map: %s", imgInfo);
				}
			}
		}
		return result;
	}

	private Map<ImageViewInfo, SpatialMap.Region> createRandomValidMoveTargetMap(final SpatialMap.Region occupiedRegion,
			final Random rnd) {
		final Map<Region, Set<Region>> validMoves = posMatrix.createValidMoveMap();
		final Map<ImageViewInfo, SpatialMap.Region> result;
		final Set<SpatialMap.Region> regionValidMoves = validMoves.get(occupiedRegion);
		if (regionValidMoves.isEmpty()) {
			result = Collections.emptyMap();
		} else {
			final SpatialMap<ImageViewInfo> piecePlacements = posMatrix.getElementPlacements();
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

		for (final Entry<Region, ImageViewInfo> piecePlacement : posMatrix.getElementPlacements()
				.getMinimalRegionElements().entries()) {
			final SpatialMap.Region region = piecePlacement.getKey();
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
	 *
	 */
	synchronized void notifyContinue(final Random rnd) {
		LOGGER.debug("Notified of continue event.");
		LOGGER.debug("Moving random piece.");
		// TODO: Change probability of a piece being selected for moving based
		// on if it was moved before: E.g. cannot move a given piece more than
		// twice in a row
		final List<SpatialMap.Region> regionsToTry = posMatrix.getElementPlacements().getMinimalRegions();
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
			notifyMove(pieceMove);
			LOGGER.debug("Finished randomly moving piece(s) at {}.", pieceMove.getKey());
		}
	}

	void notifyMove(final Entry<SpatialMap.Region, Map<ImageViewInfo, SpatialMap.Region>> pieceMove) {
		final SpatialMap.Region occupiedRegion = pieceMove.getKey();
		final Map<ImageViewInfo, SpatialMap.Region> pieceMoveTargets = pieceMove.getValue();
		highlightedRegions.add(occupiedRegion);
		pieceMoveTargets.forEach((piece, moveTarget) -> posMatrix.placeElement(piece, moveTarget));
		posMatrix.clearRegion(occupiedRegion);
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
