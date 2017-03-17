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
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

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

	@DataPoints
	public static final List<int[]> TEST_PIECES;

	private static final Map<int[], Integer> TEST_PIECE_IDS;

	static {
		TEST_PIECES = Arrays.asList(new int[] { 1, 2 }, new int[] { 3, 3 }, new int[] { 5, 2 });
		TEST_PIECE_IDS = Maps.newHashMapWithExpectedSize(TEST_PIECES.size());
		TEST_PIECES.stream().forEach(piece -> {
			final Integer id = TEST_PIECE_IDS.size();
			TEST_PIECE_IDS.put(piece, id);
		});
	}

	// @Test
	// public final void testCreateRegionPowerSetSize() {
	// final SpatialMap<int[]> piecePositions = new
	// SpatialMap<>(TEST_PIECES.size());
	// final int[] gridSize = new int[] { 10, 10 };
	// final Integer[] posMatrixBackingArray = new
	// Integer[IntArrays.product(gridSize)];
	// final Matrix<Integer> backingPosMatrix = new
	// Matrix<>(posMatrixBackingArray, gridSize[1]);
	//
	// final SpatialMatrix<Integer, int[]> matrix = new
	// SpatialMatrix<>(backingPosMatrix, TEST_PIECE_IDS::get,
	// piecePositions);
	// final Set<SpatialMap.Region> powerSet = matrix.createRegionPowerSet();
	// Assert.assertEquals(matrix.calculateRegionPowerSetSize(),
	// powerSet.size());
	// }

	/**
	 * Test method for
	 * {@link se.kth.speech.SpatialMatrix#calculateSubRegionCount()}.
	 */
	@Test
	public final void testCalculateSubRegionCount() {
		final SpatialMap<int[]> piecePositions = new SpatialMap<>(TEST_PIECES.size());
		final int[] gridSize = new int[] { 10, 11 };
		final Integer[] posMatrixBackingArray = new Integer[IntArrays.product(gridSize)];
		final Matrix<Integer> backingPosMatrix = new Matrix<>(posMatrixBackingArray, gridSize[1]);

		final SpatialMatrix<Integer, int[]> matrix = new SpatialMatrix<>(backingPosMatrix, TEST_PIECE_IDS::get,
				piecePositions);
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
	@Test
	public final void testCreateSizeIndexedRegionPowerSet() {
		final SpatialMap<int[]> piecePositions = new SpatialMap<>(TEST_PIECES.size());
		final int[] gridSize = new int[] { 10, 11 };
		final Integer[] posMatrixBackingArray = new Integer[IntArrays.product(gridSize)];
		final Matrix<Integer> backingPosMatrix = new Matrix<>(posMatrixBackingArray, gridSize[1]);

		final SpatialMatrix<Integer, int[]> matrix = new SpatialMatrix<>(backingPosMatrix, TEST_PIECE_IDS::get,
				piecePositions);
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

	@Theory
	public final void testGetCellsRegion(final int[] piece) {
		final int[] regionCoords = new int[] { 1, 11, 5, 16 };
		final SpatialMap<int[]> piecePositions = new SpatialMap<>(TEST_PIECES.size());
		final int[] gridSize = new int[] { regionCoords[1], regionCoords[3] };
		final Integer[] posMatrixBackingArray = new Integer[IntArrays.product(gridSize)];
		final Matrix<Integer> backingPosMatrix = new Matrix<>(posMatrixBackingArray, gridSize[1]);

		final Function<int[], Integer> pieceIdGetter = TEST_PIECE_IDS::get;
		final SpatialMatrix<Integer, int[]> matrix = new SpatialMatrix<>(backingPosMatrix, pieceIdGetter,
				piecePositions);
		final SpatialRegion r = matrix.getRegion(regionCoords[0], regionCoords[1], regionCoords[2],
				regionCoords[3]);
		matrix.placeElement(piece, r);
		final Set<Integer> expected = Collections.singleton(pieceIdGetter.apply(piece));
		final Set<Integer> actual = matrix.getCells(r).collect(Collectors.toSet());
		Assert.assertEquals(expected, actual);
	}

	@Test
	public final void testGetDimensions() {
		final SpatialMap<int[]> piecePositions = new SpatialMap<>(TEST_PIECES.size());
		final int[] gridSize = new int[] { 10, 10 };
		final Integer[] posMatrixBackingArray = new Integer[IntArrays.product(gridSize)];
		final Matrix<Integer> backingPosMatrix = new Matrix<>(posMatrixBackingArray, gridSize[1]);

		final SpatialMatrix<Integer, int[]> matrix = new SpatialMatrix<>(backingPosMatrix, TEST_PIECE_IDS::get,
				piecePositions);
		final int[] dims = matrix.getDimensions();
		Assert.assertArrayEquals(gridSize, dims);
	}

	@Theory
	public final void testPlaceElement(final int[] piece) {
		final int xUpperBound = 5;
		final int xLowerBound = 1;
		final int yUpperBound = 6;
		final int yLowerBound = 2;
		final SpatialMap<int[]> piecePositions = new SpatialMap<>(TEST_PIECES.size());
		final int[] gridSize = new int[] { xUpperBound, yUpperBound };
		final Integer[] posMatrixBackingArray = new Integer[IntArrays.product(gridSize)];
		final Matrix<Integer> backingPosMatrix = new Matrix<>(posMatrixBackingArray, gridSize[1]);

		final Function<int[], Integer> pieceIdGetter = TEST_PIECE_IDS::get;
		final SpatialMatrix<Integer, int[]> matrix = new SpatialMatrix<>(backingPosMatrix, pieceIdGetter,
				piecePositions);
		final SpatialRegion r = matrix.getRegion(xLowerBound, yUpperBound, yLowerBound, yUpperBound);
		matrix.placeElement(piece, r);
		final Set<Integer> pieceId = Collections.singleton(pieceIdGetter.apply(piece));
		final SpatialRegion totalRegion = matrix.getRegion(0, xUpperBound, 0, yUpperBound);
		final Map<Integer, Integer> cellValueCounts = Maps.newHashMapWithExpectedSize(2);
		matrix.getCells(totalRegion).forEach(cell -> cellValueCounts.compute(cell, (key, oldCount) -> {
			final Integer newCount;
			if (oldCount == null) {
				newCount = 1;
			} else {
				newCount = oldCount + 1;
			}
			return newCount;
		}));
		final Integer pieceCount = cellValueCounts.get(pieceId);
		Assert.assertNotNull(pieceCount);
		final int regionArea = IntArrays.product(r.getDimensions());
		Assert.assertEquals(regionArea, pieceCount.intValue());
		final Integer nullCount = cellValueCounts.get(null);
		Assert.assertNotNull(nullCount);
		final int totalArea = IntArrays.product(matrix.getDimensions());
		Assert.assertEquals(totalArea - pieceCount, nullCount.intValue());
	}

}
