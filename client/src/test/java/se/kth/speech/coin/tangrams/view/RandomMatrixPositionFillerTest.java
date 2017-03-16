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

import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.StringJoiner;
import java.util.function.Function;

import org.junit.Assert;
import org.junit.Test;
import org.junit.experimental.theories.DataPoints;
import org.junit.experimental.theories.Theories;
import org.junit.experimental.theories.Theory;
import org.junit.runner.RunWith;

import com.google.common.collect.Maps;

import se.kth.speech.IntArrays;
import se.kth.speech.Matrix;
import se.kth.speech.MutablePair;
import se.kth.speech.SpatialMap;
import se.kth.speech.SpatialMatrix;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 16 Mar 2017
 *
 */
@RunWith(Theories.class)
public final class RandomMatrixPositionFillerTest {

	private static final List<int[]> TEST_PIECES;

	private static final Map<int[], Integer> TEST_PIECE_IDS;

	@DataPoints
	public static final long[] TEST_SEEDS;

	static {
		TEST_PIECES = Arrays.asList(new int[] { 1, 2 }, new int[] { 3, 3 }, new int[] { 5, 2 });
		TEST_PIECE_IDS = Maps.newHashMapWithExpectedSize(TEST_PIECES.size());
		TEST_PIECES.stream().forEach(piece -> {
			final Integer id = TEST_PIECE_IDS.size();
			TEST_PIECE_IDS.put(piece, id);
		});

		TEST_SEEDS = new Random().longs().limit(100).toArray();
	}

	private static MutablePair<SpatialMatrix<Integer, int[]>, Set<Integer>> apply(final long seed) {
		final SpatialMap<int[]> piecePositions = new SpatialMap<>(TEST_PIECES.size());
		final int[] gridSize = new int[] { 10, 10 };
		final Integer[] posMatrixBackingArray = new Integer[IntArrays.product(gridSize)];
		final Matrix<Integer> backingPosMatrix = new Matrix<>(posMatrixBackingArray, gridSize[1]);

		final Random rnd = new Random(seed);
		final Function<int[], int[]> piecePosMatrixSizeFactory = Function.identity();
		final SpatialMatrix<Integer, int[]> matrix = new SpatialMatrix<>(backingPosMatrix, TEST_PIECE_IDS::get,
				piecePositions);
		final RandomMatrixPositionFiller<Integer, int[]> filler = new RandomMatrixPositionFiller<>(matrix,
				TEST_PIECE_IDS::get, rnd, piecePosMatrixSizeFactory);
		final Set<Integer> addedIds = filler.apply(TEST_PIECES);
		return new MutablePair<>(matrix, addedIds);
	}

	private static String createArrayCollString(final Iterable<int[]> arrs) {
		final StringJoiner sb = new StringJoiner(", ", "[", "]");
		arrs.forEach(arr -> {
			sb.add(Arrays.toString(arr));
		});
		return sb.toString();
	}

	private static <SuperT, SubT extends SuperT> Set<SuperT> createSetDiff(final Collection<SubT> minuend,
			final Collection<SubT> sutrahend) {
		final Set<SuperT> result = new HashSet<>(minuend);
		result.removeAll(sutrahend);
		return result;
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.view.RandomMatrixPositionFiller#apply(Collection)}.
	 */
	@Test
	public final void testApply() {
		final SpatialMap<int[]> piecePositions = new SpatialMap<>(TEST_PIECES.size());
		final int[] gridSize = new int[] { 10, 10 };
		final Integer[] posMatrixBackingArray = new Integer[IntArrays.product(gridSize)];
		final Matrix<Integer> backingPosMatrix = new Matrix<>(posMatrixBackingArray, gridSize[1]);

		final Random rnd = new Random();
		final Function<int[], int[]> piecePosMatrixSizeFactory = Function.identity();
		final SpatialMatrix<Integer, int[]> matrix = new SpatialMatrix<>(backingPosMatrix, TEST_PIECE_IDS::get,
				piecePositions);
		final RandomMatrixPositionFiller<Integer, int[]> filler = new RandomMatrixPositionFiller<>(matrix,
				TEST_PIECE_IDS::get, rnd, piecePosMatrixSizeFactory);
		final Set<Integer> addedIds = filler.apply(TEST_PIECES);
		// Test added IDs
		Assert.assertEquals(TEST_PIECE_IDS.values().size(), addedIds.size());
		{
			final Set<Object> setDiff = createSetDiff(addedIds, TEST_PIECE_IDS.values());
			Assert.assertTrue(
					"There are some piece IDs in the result set not present in the test piece set: " + setDiff,
					setDiff.isEmpty());
		}
		{
			final Set<Object> setDiff = createSetDiff(TEST_PIECE_IDS.values(), addedIds);
			Assert.assertTrue("Some test piece IDs are missing from the result set: " + setDiff, setDiff.isEmpty());
		}

		// Test spatial matrix
		{
			final Set<int[]> setDiff = createSetDiff(piecePositions.getAllElements(), TEST_PIECES);
			Assert.assertTrue("The spatial map contains some pieces not present in the test piece set: "
					+ createArrayCollString(setDiff), setDiff.isEmpty());
		}
		{
			final Set<int[]> setDiff = createSetDiff(TEST_PIECES, piecePositions.getAllElements());
			Assert.assertTrue(
					"Some test pieces are missing from the spatial map element set: " + createArrayCollString(setDiff),
					setDiff.isEmpty());
		}
		{
			final Set<int[]> setDiff = createSetDiff(TEST_PIECES, piecePositions.getAllElements());
			Assert.assertTrue("The spatial map region mapping does not contain all test pieces: "
					+ createArrayCollString(setDiff), setDiff.isEmpty());
		}
		{
			final Set<int[]> setDiff = createSetDiff(piecePositions.getAllElements(), TEST_PIECES);
			Assert.assertTrue("The spatial map region mapping contains pieces not in the set of test pieces: "
					+ createArrayCollString(setDiff), setDiff.isEmpty());
		}
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.view.RandomMatrixPositionFiller#apply(Collection)}.
	 */
	@Theory
	public final void testApplyStable(final long seed) {
		final MutablePair<SpatialMatrix<Integer, int[]>, Set<Integer>> result1 = apply(seed);
		final MutablePair<SpatialMatrix<Integer, int[]>, Set<Integer>> result2 = apply(seed);
		Assert.assertEquals(result1, result2);
	}

}
