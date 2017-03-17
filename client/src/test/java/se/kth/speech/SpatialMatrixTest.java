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
package se.kth.speech;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.Assert;
import org.junit.Test;
import org.junit.experimental.theories.DataPoints;
import org.junit.experimental.theories.Theories;
import org.junit.experimental.theories.Theory;
import org.junit.runner.RunWith;

import com.google.common.collect.Maps;
import com.google.common.collect.Table;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 16 Mar 2017
 *
 */
@RunWith(Theories.class)
public final class SpatialMatrixTest {

	@DataPoints("gridDims")
	public static final List<int[]> TEST_DIMENSIONS;

	static {
		TEST_DIMENSIONS = Arrays.asList(new int[] { 1, 2 }, new int[] { 3, 3 }, new int[] { 5, 2 },
				new int[] { 10, 11 }, new int[] { 10, 20 });
	}

	// @Test
	// public final void testCreateRegionPowerSetSize() {
	// final SpatialMap<int[]> piecePositions = new
	// SpatialMap<>(TEST_PIECES.size());
	// final int[] gridDims = new int[] { 10, 10 };
	// final Integer[] posMatrixBackingArray = new
	// Integer[IntArrays.product(gridDims)];
	// final Matrix<Integer> backingPosMatrix = new
	// Matrix<>(posMatrixBackingArray, gridDims[1]);
	//
	// final SpatialMatrix<Integer, int[]> matrix = new
	// SpatialMatrix<>(backingPosMatrix, TEST_PIECE_IDS::get,
	// piecePositions);
	// final Set<SpatialMap.Region> powerSet = matrix.createRegionPowerSet();
	// Assert.assertEquals(matrix.calculateRegionPowerSetSize(),
	// powerSet.size());
	// }

	private static Matrix<Integer> createBackingMatrix(final int[] gridDims) {
		final Integer[] posMatrixBackingArray = new Integer[IntArrays.product(gridDims)];
		Assert.assertEquals(IntArrays.product(gridDims), posMatrixBackingArray.length);
		return new Matrix<>(posMatrixBackingArray, gridDims[1]);
	}

	private static <E> Entry<SpatialMatrix<Integer, E>, Map<E, Integer>> createSpatialMatrixMap(final int[] gridDims,
			final int expectedPieceCount) {
		final Map<E, Integer> pieceIds = Maps.newHashMapWithExpectedSize(expectedPieceCount);
		final Function<E, Integer> incrementingIdGetter = p -> pieceIds.computeIfAbsent(p, k -> pieceIds.size());
		final Matrix<Integer> backingPosMatrix = createBackingMatrix(gridDims);
		return new MutablePair<>(
				new SpatialMatrix<>(backingPosMatrix, incrementingIdGetter, new SpatialMap<>(expectedPieceCount)),
				pieceIds);
	}

	private static void testGetCellsRegion(final int xLowerBound, final int xUpperBound, final int yLowerBound,
			final int yUpperBound) {
		final Entry<SpatialMatrix<Integer, Object>, Map<Object, Integer>> matrixMap = createSpatialMatrixMap(
				SpatialRegion.getDimensions(xLowerBound, xUpperBound, yLowerBound, yUpperBound), 1);
		final SpatialMatrix<Integer, Object> matrix = matrixMap.getKey();
		final Map<Object, Integer> pieceIds = matrixMap.getValue();

		final String piece = "testpiece";
		final SpatialRegion r = matrix.getRegion(xLowerBound, yUpperBound, yLowerBound, yUpperBound);
		matrix.placeElement(piece, r);
		final Integer pieceId = pieceIds.get(piece);
		Assert.assertNotNull(pieceId);
		final Set<Integer> actual = matrix.getCells(r).collect(Collectors.toSet());
		Assert.assertEquals(Collections.singleton(pieceId), actual);
	}

	private static <E> void testPlaceElement(final E piece, final int xLowerBound, final int xUpperBound,
			final int yLowerBound, final int yUpperBound) {
		final Entry<SpatialMatrix<Integer, Object>, Map<Object, Integer>> matrixMap = createSpatialMatrixMap(
				SpatialRegion.getDimensions(xLowerBound, xUpperBound, yLowerBound, yUpperBound), 1);
		final SpatialMatrix<Integer, Object> matrix = matrixMap.getKey();
		final Map<Object, Integer> pieceIds = matrixMap.getValue();

		final SpatialRegion r = matrix.getRegion(xLowerBound, yUpperBound, yLowerBound, yUpperBound);
		matrix.placeElement(piece, r);
		final Integer pieceId = pieceIds.get(piece);
		Assert.assertNotNull(pieceId);
		final SpatialRegion totalRegion = matrix.getRegion(0, xUpperBound, 0, yUpperBound);
		final Map<Integer, Integer> cellValueCounts = Maps.newHashMapWithExpectedSize(2);
		matrix.getCells(totalRegion)
				.forEach(cell -> cellValueCounts.compute(cell, ComparableValueMaps.NULLABLE_INTEGER_VALUE_INCREMENTER));
		final Integer pieceOccupiedCellCount = cellValueCounts.get(pieceId);
		Assert.assertNotNull(pieceOccupiedCellCount);
		final int regionArea = IntArrays.product(r.getDimensions());
		Assert.assertEquals(regionArea, pieceOccupiedCellCount.intValue());
		final Integer nullCount = cellValueCounts.get(null);
		Assert.assertNotNull(nullCount);
		final int totalArea = IntArrays.product(matrix.getDimensions());
		Assert.assertEquals(totalArea - pieceOccupiedCellCount, nullCount.intValue());
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.SpatialMatrix#calculateSubRegionCount()}.
	 */
	@Theory
	public final void testCalculateSubRegionCount(final int[] gridDims) {
		final Entry<SpatialMatrix<Integer, Object>, Map<Object, Integer>> matrixMap = createSpatialMatrixMap(gridDims,
				1);
		final SpatialMatrix<Integer, Object> matrix = matrixMap.getKey();

		final Table<Integer, Integer, Collection<SpatialRegion>> regionPowerSet = matrix
				.createSizeIndexedRegionPowerSet(ArrayList::new);
		regionPowerSet.rowMap().forEach((xLength, yLengths) -> {
			yLengths.forEach((yLength, subRegionSet) -> {
				final int expectedCount = matrix.calculateSubRegionCount(xLength, yLength);
				Assert.assertEquals(expectedCount, subRegionSet.size());
			});
		});
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.SpatialMatrix#createSizeIndexedRegionPowerSet()}.
	 */
	@Theory
	public final void testCreateSizeIndexedRegionPowerSet(final int[] gridDims) {
		final Entry<SpatialMatrix<Integer, Object>, Map<Object, Integer>> matrixMap = createSpatialMatrixMap(gridDims,
				1);
		final SpatialMatrix<Integer, Object> matrix = matrixMap.getKey();

		final int[] dims = matrix.getDimensions();
		final int x = dims[0];
		final int y = dims[1];
		final Table<Integer, Integer, Collection<SpatialRegion>> regionPowerSet = matrix
				.createSizeIndexedRegionPowerSet(ArrayList::new);
		final Set<Integer> rows = regionPowerSet.rowKeySet();
		Assert.assertEquals(Integer.valueOf(x), Collections.max(rows));
		Assert.assertEquals(x, rows.size());
		final Set<Integer> cols = regionPowerSet.columnKeySet();
		Assert.assertEquals(Integer.valueOf(y), Collections.max(cols));
		Assert.assertEquals(y, cols.size());
		regionPowerSet.values().stream().forEach(size -> {
			Assert.assertFalse(size.isEmpty());
		});
		final SpatialRegion totalRegion = matrix.getRegion(0, x, 0, y);
		regionPowerSet.values().stream().flatMap(Collection::stream).forEach(region -> {
			Assert.assertTrue(String.format("%s not subsumed by total %s.", region, totalRegion),
					totalRegion.subsumes(region));
		});
	}

	/**
	 * Test method for {@link se.kth.speech.SpatialMatrix#getCells()}.
	 */
	@Theory
	public final void testGetCells(final int[] gridDims) {
		final Entry<SpatialMatrix<Integer, Object>, Map<Object, Integer>> matrixMap = createSpatialMatrixMap(gridDims,
				1);
		final SpatialMatrix<Integer, Object> matrix = matrixMap.getKey();
		final long expected = IntArrays.product(gridDims);
		final Stream<Integer> cells = matrix.getCells();
		final long actual = cells.count();
		Assert.assertEquals(expected, actual);
	}

	@Test
	public final void testGetCellsRegion1() {
		testGetCellsRegion(0, 0, 0, 0);
	}

	@Test
	public final void testGetCellsRegion2() {
		testGetCellsRegion(1, 11, 5, 16);
	}

	@Theory
	public final void testGetDimensions(final int[] gridDims) {
		final SpatialMatrix<Integer, Object> matrix = createSpatialMatrixMap(gridDims, 1).getKey();
		final int[] dims = matrix.getDimensions();
		Assert.assertArrayEquals(gridDims, dims);
	}

	@Test
	public final void testPlaceElement1() {
		final int xUpperBound = 1;
		final int xLowerBound = 0;
		final int yUpperBound = 1;
		final int yLowerBound = 0;
		testPlaceElement("test1", xLowerBound, xUpperBound, yLowerBound, yUpperBound);
	}

	@Test
	public final void testPlaceElement2() {
		final int xUpperBound = 5;
		final int xLowerBound = 1;
		final int yUpperBound = 6;
		final int yLowerBound = 2;
		testPlaceElement("test2", xLowerBound, xUpperBound, yLowerBound, yUpperBound);
	}

}
