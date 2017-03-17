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

	private static class SpatialMatrixConstructionData<E> {

		private final Map<E, Integer> pieceIds;

		private final Function<E, Integer> incrementingIdGetter;

		private final Matrix<Integer> backingPosMatrix;

		private final SpatialMatrix<Integer, E> matrix;

		private SpatialMatrixConstructionData(final int[] gridDims, final int expectedPieceCount) {
			this.pieceIds = Maps.newHashMapWithExpectedSize(expectedPieceCount);
			this.incrementingIdGetter = p -> pieceIds.computeIfAbsent(p, k -> pieceIds.size());
			this.backingPosMatrix = createBackingMatrix(gridDims);
			this.matrix = new SpatialMatrix<>(backingPosMatrix, incrementingIdGetter,
					new SpatialMap<>(expectedPieceCount));
		}
	}

	@DataPoints("gridDims")
	public static final List<int[]> TEST_DIMENSIONS;

	static {
		TEST_DIMENSIONS = Arrays.asList(new int[] { 1, 2 }, new int[] { 3, 3 }, new int[] { 5, 2 },
				new int[] { 10, 11 }, new int[] { 10, 50 });
	}

	private static Matrix<Integer> createBackingMatrix(final int[] gridDims) {
		final Integer[] posMatrixBackingArray = new Integer[IntArrays.product(gridDims)];
		Assert.assertEquals(IntArrays.product(gridDims), posMatrixBackingArray.length);
		return new Matrix<>(posMatrixBackingArray, gridDims[1]);
	}

	private static void testGetCellsRegion(final int xLowerBound, final int xUpperBound, final int yLowerBound,
			final int yUpperBound) {
		final int[] gridDims = new int[] { xUpperBound, yUpperBound };
		final SpatialMatrixConstructionData<Object> matrixConstData = new SpatialMatrixConstructionData<>(gridDims, 1);
		final SpatialMatrix<Integer, Object> matrix = matrixConstData.matrix;
		final String piece = "testpiece";
		final Integer pieceId = matrixConstData.incrementingIdGetter.apply(piece);
		Assert.assertNotNull(pieceId);
		final List<Integer> cells = matrix.getPositionMatrix().getValues();
		Collections.fill(cells, pieceId);

		final SpatialRegion r = matrix.getRegion(xLowerBound, xUpperBound, yLowerBound, yUpperBound);
		final Set<Integer> actual = matrix.getCells(r).collect(Collectors.toSet());
		Assert.assertEquals(Collections.singleton(pieceId), actual);
	}

	private static <E> void testPlaceElement(final E piece, final int xLowerBound, final int xUpperBound,
			final int yLowerBound, final int yUpperBound) {
		final int[] gridDims = SpatialRegion.getDimensions(xLowerBound, xUpperBound, yLowerBound, yUpperBound);
		final SpatialMatrixConstructionData<Object> matrixConstData = new SpatialMatrixConstructionData<>(gridDims, 1);
		final SpatialMatrix<Integer, Object> matrix = matrixConstData.matrix;
		final Map<Object, Integer> pieceIds = matrixConstData.pieceIds;

		final SpatialRegion r = matrix.getRegion(xLowerBound, xUpperBound, yLowerBound, yUpperBound);
		Assert.assertArrayEquals(gridDims, r.getDimensions());
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
		final Integer nullCount = cellValueCounts.getOrDefault(null, 0);
		final int totalArea = IntArrays.product(matrix.getDimensions());
		Assert.assertEquals(totalArea - pieceOccupiedCellCount, nullCount.intValue());
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.SpatialMatrix#calculateSubRegionCount()}.
	 */
	@Theory
	public final void testCalculateSubRegionCount(final int[] gridDims) {
		final SpatialMatrix<Integer, Object> matrix = new SpatialMatrixConstructionData<>(gridDims, 1).matrix;

		final Table<Integer, Integer, Collection<SpatialRegion>> regionPowerSet = matrix
				.createSizeIndexedRegionPowerSet(ArrayList::new);
		regionPowerSet.rowMap().forEach((xLength, yLengths) -> {
			yLengths.forEach((yLength, subRegionSet) -> {
				final int actual = matrix.calculateSubRegionCount(xLength, yLength);
				Assert.assertEquals(subRegionSet.size(), actual);
			});
		});
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.SpatialMatrix#createSizeIndexedRegionPowerSet()}.
	 */
	@Theory
	public final void testCreateSizeIndexedRegionPowerSet(final int[] gridDims) {
		final SpatialMatrix<Integer, Object> matrix = new SpatialMatrixConstructionData<>(gridDims, 1).matrix;

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
			Assert.assertNotNull(size);
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
	public final void testGetCellsCount(final int[] gridDims) {
		final SpatialMatrix<Integer, Object> matrix = new SpatialMatrixConstructionData<>(gridDims, 1).matrix;
		final long expected = IntArrays.product(gridDims);
		final Stream<Integer> cells = matrix.getCells();
		final long actual = cells.count();
		Assert.assertEquals(expected, actual);
	}

	@Test
	public final void testGetCellsRegion1() {
		testGetCellsRegion(0, 1, 0, 1);
	}

	@Test
	public final void testGetCellsRegion2() {
		testGetCellsRegion(1, 11, 5, 16);
	}

	@Test
	public final void testGetCellsRegion3() {
		testGetCellsRegion(1, 10, 5, 10);
	}

	@Test
	public final void testGetCellsRegion4() {
		testGetCellsRegion(1, 2, 1, 2);
	}

	@Theory
	public final void testGetDimensions(final int[] gridDims) {
		final SpatialMatrix<Integer, Object> matrix = new SpatialMatrixConstructionData<>(gridDims, 1).matrix;
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
