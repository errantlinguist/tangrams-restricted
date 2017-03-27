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
				.map(SpatialMatrix<Integer>::new)
				.collect(Collectors.toCollection(() -> new ArrayList<>(testDescs.size())));
		final Random rnd = new Random();
		TEST_SEEDS = rnd.longs().distinct().limit(5).toArray();
		TEST_POSITIVE_INTS = rnd.ints(1, 10).distinct().limit(5).toArray();
	}

	private static MapEntryRemapping<Integer, SpatialRegion> findFirstPostInitialMoveWithRandomPieceId(
			final PatternMoveFactory moveFactory, final int initialRndOnsetLength, final int nthElemRandom) {
		MapEntryRemapping<Integer, SpatialRegion> result = null;
		int turnNo = initialRndOnsetLength + 1;
		for (;; turnNo++) {
			final MapEntryRemapping<Integer, SpatialRegion> nextMove = moveFactory.get();
			if (turnNo % nthElemRandom == 0) {
				result = nextMove;
				break;
			}
		}
		Assert.assertNotNull(result);
		return result;
	}

	private static void runTestGetFirstNthMoveUnseen(final SpatialMatrix<Integer> model, final long seed,
			final int initialRndOnsetLength, final int nthElemRandom) {
		final Random rnd = new Random(seed);
		final PatternMoveFactory moveFactory = new PatternMoveFactory(rnd, model, initialRndOnsetLength, nthElemRandom);
		final Set<MapEntryRemapping<Integer, SpatialRegion>> intialMoves = Stream.generate(moveFactory)
				.limit(initialRndOnsetLength)
				.collect(Collectors.toCollection(() -> Sets.newHashSetWithExpectedSize(initialRndOnsetLength)));
		final MapEntryRemapping<Integer, SpatialRegion> firstPostInitialMoveWithRandomPieceId = findFirstPostInitialMoveWithRandomPieceId(
				moveFactory, initialRndOnsetLength, nthElemRandom);
		Assert.assertFalse(intialMoves.contains(firstPostInitialMoveWithRandomPieceId));
	}

	@Theory
	public void testGetFirstNthMoveUnseen(final SpatialMatrix<Integer> model, final long seed,
			final int initialRndOnsetLength, final int nthElemRandom) {
		runTestGetFirstNthMoveUnseen(model, seed, initialRndOnsetLength, nthElemRandom);
	}

	@Test
	public void testGetFirstNthMoveUnseenDivisbleVals() {
		final SpatialMatrix<Integer> model = TEST_MODELS.iterator().next();
		final int initialRndOnsetLength = 4;
		final int nthElemRandom = 8;
		runTestGetFirstNthMoveUnseen(model, 1, initialRndOnsetLength, nthElemRandom);
	}

	@Test
	public void testGetFirstNthMoveUnseenRegression() {
		final SpatialMatrix<Integer> model = TEST_MODELS.iterator().next();
		final int initialRndOnsetLength = 3;
		final int nthElemRandom = 7;
		runTestGetFirstNthMoveUnseen(model, -1808198854872016138l, initialRndOnsetLength, nthElemRandom);
	}

	@Test
	public void testGetFirstNthMoveUnseenSameVals() {
		final SpatialMatrix<Integer> model = TEST_MODELS.iterator().next();
		final int initialRndOnsetLength = 4;
		final int nthElemRandom = 4;
		runTestGetFirstNthMoveUnseen(model, 1, initialRndOnsetLength, nthElemRandom);
	}

	@Test
	public void testGetFirstNthMoveUnseenUnseen() {
		final SpatialMatrix<Integer> model = TEST_MODELS.iterator().next();
		final int initialRndOnsetLength = 4;
		final int nthElemRandom = 5;
		runTestGetFirstNthMoveUnseen(model, 1, initialRndOnsetLength, nthElemRandom);
	}

	@Theory
	public void testGetFirstNthPieceIdUnseen(final SpatialMatrix<Integer> model, final long seed,
			final int initialRndOnsetLength, final int nthElemRandom) {
		final Random rnd = new Random(seed);
		final PatternMoveFactory moveFactory = new PatternMoveFactory(rnd, model, initialRndOnsetLength, nthElemRandom);
		final Set<Integer> initialMovePieceIds = Stream.generate(moveFactory).limit(initialRndOnsetLength)
				.map(MapEntryRemapping::getKey)
				.collect(Collectors.toCollection(() -> Sets.newHashSetWithExpectedSize(initialRndOnsetLength)));
		final MapEntryRemapping<Integer, SpatialRegion> firstPostInitialMoveWithRandomPieceId = findFirstPostInitialMoveWithRandomPieceId(
				moveFactory, initialRndOnsetLength, nthElemRandom);
		final Integer pieceId = firstPostInitialMoveWithRandomPieceId.getKey();
		Assert.assertFalse(initialMovePieceIds.contains(pieceId));
	}

	@Theory
	public void testGetRndOnsetUniqueMoves(final SpatialMatrix<Integer> model, final long seed,
			final int initialRndOnsetLength, final int nthElemRandom) {
		final Random rnd = new Random(seed);
		final PatternMoveFactory moveFactory = new PatternMoveFactory(rnd, model, initialRndOnsetLength, nthElemRandom);
		final Set<MapEntryRemapping<Integer, SpatialRegion>> distinctMoves = Stream.generate(moveFactory)
				.limit(initialRndOnsetLength)
				.collect(Collectors.toCollection(() -> Sets.newHashSetWithExpectedSize(initialRndOnsetLength)));
		Assert.assertEquals(initialRndOnsetLength, distinctMoves.size());
	}

	@Theory
	public void testGetRndOnsetUniquePieceIds(final SpatialMatrix<Integer> model, final long seed,
			final int initialRndOnsetLength, final int nthElemRandom) {
		final Random rnd = new Random(seed);
		final PatternMoveFactory moveFactory = new PatternMoveFactory(rnd, model, initialRndOnsetLength, nthElemRandom);
		final Set<Integer> distinctMovePieceIds = Stream.generate(moveFactory).limit(initialRndOnsetLength)
				.map(MapEntryRemapping::getKey)
				.collect(Collectors.toCollection(() -> Sets.newHashSetWithExpectedSize(initialRndOnsetLength)));
		Assert.assertEquals(initialRndOnsetLength, distinctMovePieceIds.size());
	}

}
