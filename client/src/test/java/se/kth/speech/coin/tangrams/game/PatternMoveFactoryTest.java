/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.client.
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
package se.kth.speech.coin.tangrams.game;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Random;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.Assert;
import org.junit.Test;
import org.junit.experimental.theories.DataPoints;
import org.junit.experimental.theories.Theories;
import org.junit.experimental.theories.Theory;
import org.junit.runner.RunWith;

import com.google.common.collect.Sets;

import se.kth.speech.MapEntryRemapping;
import se.kth.speech.Matrix;
import se.kth.speech.SpatialMatrix;
import se.kth.speech.SpatialRegion;
import se.kth.speech.coin.tangrams.MatrixTests;

/**
 *
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 30 Mar 2017
 * 
 */
@RunWith(Theories.class)
public final class PatternMoveFactoryTest {

	@DataPoints("testMatrices")
	public static final Collection<SpatialMatrix<Integer>> TEST_MODELS;

	@DataPoints("positiveInts")
	public static final int[] TEST_POSITIVE_INTS;

	@DataPoints("seeds")
	public static final long[] TEST_SEEDS;

	static {
		final Collection<MatrixTests.Description> testDescs = MatrixTests.getNamedTestDescs().values();
		TEST_MODELS = testDescs.stream().map(desc -> new Matrix<>(desc.getValues(), desc.getColCount()))
				.map(matrix -> SpatialMatrix.Factory.STABLE_ITER_ORDER.create(matrix))
				.collect(Collectors.toCollection(() -> new ArrayList<>(testDescs.size())));
		final Random rnd = new Random();
		TEST_SEEDS = rnd.longs().distinct().limit(5).toArray();
		TEST_POSITIVE_INTS = rnd.ints(2, 12).distinct().limit(10).toArray();
	}

	private static void runTestGetFirstNthPieceIdSeen(final SpatialMatrix<Integer> model, final long seed,
			final int initialRndOnsetLength) {
		final Random rnd = new Random(seed);
		final PatternMoveFactory moveFactory = new PatternMoveFactory(rnd, model, initialRndOnsetLength);
		final Set<Integer> intialPieceIds = Stream.generate(moveFactory).limit(initialRndOnsetLength)
				.map(MapEntryRemapping::getKey)
				.collect(Collectors.toCollection(() -> Sets.newHashSetWithExpectedSize(initialRndOnsetLength)));
		final MapEntryRemapping<Integer, SpatialRegion> firstPostInitialMove = moveFactory.get();
		Assert.assertNotNull(firstPostInitialMove);
		final Integer pieceId = firstPostInitialMove.getKey();
		Assert.assertTrue(intialPieceIds.contains(pieceId));
	}

	@Theory
	public void testGetAlternatingPieceIdsSeen(final SpatialMatrix<Integer> model, final long seed,
			final int initialRndOnsetLength, final int testMoveCount) {
		final Random rnd = new Random(seed);
		final PatternMoveFactory moveFactory = new PatternMoveFactory(rnd, model, initialRndOnsetLength);
		final int expectedUniqueIdCount = initialRndOnsetLength + testMoveCount / 2;
		final Set<Integer> seenPieceIds = Stream.generate(moveFactory).limit(initialRndOnsetLength)
				.map(MapEntryRemapping::getKey)
				.collect(Collectors.toCollection(() -> Sets.newHashSetWithExpectedSize(expectedUniqueIdCount)));
		for (int nextMoveNo = 0; nextMoveNo < testMoveCount; nextMoveNo++) {
			final MapEntryRemapping<Integer, SpatialRegion> nextMove = moveFactory.get();
			Assert.assertNotNull(nextMove);
			final Integer pieceId = nextMove.getKey();
			final boolean shouldBeAlreadySeen = nextMoveNo % 2 == 0;
			Assert.assertEquals(shouldBeAlreadySeen, seenPieceIds.contains(pieceId));
			seenPieceIds.add(pieceId);
		}
		Assert.assertEquals(expectedUniqueIdCount, seenPieceIds.size());
	}

	@Test
	public void testGetFirstNthPieceIdSeen() {
		final SpatialMatrix<Integer> model = TEST_MODELS.iterator().next();
		final int initialRndOnsetLength = 4;
		// final int nthElemRandom = 5;
		runTestGetFirstNthPieceIdSeen(model, 1, initialRndOnsetLength);
	}

	@Theory
	public void testGetFirstNthPieceIdSeen(final SpatialMatrix<Integer> model, final long seed,
			final int initialRndOnsetLength) {
		runTestGetFirstNthPieceIdSeen(model, seed, initialRndOnsetLength);
	}

	@Test
	public void testGetFirstNthPieceIdSeenRegression() {
		final SpatialMatrix<Integer> model = TEST_MODELS.iterator().next();
		final int initialRndOnsetLength = 3;
		// final int nthElemRandom = 7;
		runTestGetFirstNthPieceIdSeen(model, -1808198854872016138l, initialRndOnsetLength);
	}

	@Test
	public void testGetFirstNthPieceIdSeenSameVals() {
		final SpatialMatrix<Integer> model = TEST_MODELS.iterator().next();
		final int initialRndOnsetLength = 4;
		// final int nthElemRandom = 4;
		runTestGetFirstNthPieceIdSeen(model, 1, initialRndOnsetLength);
	}

	@Theory
	public void testGetRndOnsetUniqueMoves(final SpatialMatrix<Integer> model, final long seed,
			final int initialRndOnsetLength) {
		final Random rnd = new Random(seed);
		final PatternMoveFactory moveFactory = new PatternMoveFactory(rnd, model, initialRndOnsetLength);
		final Set<MapEntryRemapping<Integer, SpatialRegion>> distinctMoves = Stream.generate(moveFactory)
				.limit(initialRndOnsetLength)
				.collect(Collectors.toCollection(() -> Sets.newHashSetWithExpectedSize(initialRndOnsetLength)));
		Assert.assertEquals(initialRndOnsetLength, distinctMoves.size());
	}

	@Theory
	public void testGetRndOnsetUniquePieceIds(final SpatialMatrix<Integer> model, final long seed,
			final int initialRndOnsetLength) {
		final Random rnd = new Random(seed);
		final PatternMoveFactory moveFactory = new PatternMoveFactory(rnd, model, initialRndOnsetLength);
		final Set<Integer> distinctMovePieceIds = Stream.generate(moveFactory).limit(initialRndOnsetLength)
				.map(MapEntryRemapping::getKey)
				.collect(Collectors.toCollection(() -> Sets.newHashSetWithExpectedSize(initialRndOnsetLength)));
		Assert.assertEquals(initialRndOnsetLength, distinctMovePieceIds.size());
	}

}
