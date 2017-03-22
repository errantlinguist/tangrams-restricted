/*
 *  This file is part of tangrams.
 *
 *  tangrams is free software: you can redistribute it and/or modify
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

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.not;
import static org.junit.Assert.assertTrue;
import static org.junit.Assume.assumeThat;
import static org.junit.Assume.assumeTrue;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.ToIntFunction;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import org.junit.Rule;
import org.junit.experimental.theories.DataPoints;
import org.junit.experimental.theories.FromDataPoints;
import org.junit.experimental.theories.Theories;
import org.junit.experimental.theories.Theory;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;

import se.kth.speech.junit.IteratorEqualityAsserter;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 13 Jan 2017
 *
 */
@RunWith(Theories.class)
public final class MatrixTest {

	@DataPoints("colCounts")
	public static final int[] COL_COUNTS;

	@DataPoints("matrices")
	public static final Collection<Matrix<Object>> MATRICES;

	@DataPoints("matrixIdxPositions")
	public static final int[] MATRIX_IDX_POSITIONS;

	@DataPoints("valArrayIdxs")
	public static final int[] VAL_ARRAY_IDXS;

	@DataPoints("valArrays")
	public static final Object[][] VAL_ARRAYS;

	private static final Map<Matrix<Object>, Object[]> MATRIX_VAL_ARRAYS;

	static {
		COL_COUNTS = IntStream.range(-1, 10).toArray();
		VAL_ARRAYS = createValueArrays();
		final List<Object[]> valueArrayList = Arrays.asList(VAL_ARRAYS);
		final int maxValArrIdx = valueArrayList.stream().mapToInt(valArr -> valArr.length).max().getAsInt();
		VAL_ARRAY_IDXS = IntStream.range(0, maxValArrIdx).toArray();
		MATRIX_VAL_ARRAYS = createMatrixValueArrayMap(valueArrayList);
		MATRICES = MATRIX_VAL_ARRAYS.keySet();

		final int maxMatrixIdxPosition = MATRICES.stream().map(Matrix::getDimensions).mapToInt(dims -> dims.length)
				.max().getAsInt();
		MATRIX_IDX_POSITIONS = IntStream.range(0, maxMatrixIdxPosition).toArray();
	}

	private static final IteratorEqualityAsserter<Object> ITER_ASSERTER = new IteratorEqualityAsserter<>();

	private static <V> Map<Matrix<V>, V[]> createMatrixValueArrayMap(final Collection<? extends V[]> valArrays) {
		final int collSize = valArrays.size();
		final int initialResultCapacity = collSize * (collSize / 2) + 1;
		final Map<Matrix<V>, V[]> result = new HashMap<>(initialResultCapacity);
		for (final V[] valArray : valArrays) {
			for (int colCount = 0; colCount < valArray.length; ++colCount) {
				try {
					final Matrix<V> m = new Matrix<>(valArray, colCount);
					result.put(m, valArray);
				} catch (final IllegalArgumentException e) {
					// Skip ill-formed matrix
				}
			}
		}
		return result;
	}

	private static Integer[][] createValueArrays() {
		return IntStream.range(0, 11).mapToObj(length -> {
			return IntStream.range(0, length).boxed().toArray(Integer[]::new);
		}).toArray(Integer[][]::new);

	}

	@Rule
	public final ExpectedException thrown = ExpectedException.none();

	/**
	 * Test method for {@link se.kth.speech.Matrix#getMatrixIndices(int)}.
	 */
	@Theory
	public void testGetMatrixIndicesMonotonicRow(final Matrix<Object> m,
			@FromDataPoints("valArrayIdxs") final int valArrIdx1,
			@FromDataPoints("valArrayIdxs") final int valArrIdx2) {
		final int[] mIdx1 = m.getMatrixIndices(valArrIdx1);
		final int[] mIdx2 = m.getMatrixIndices(valArrIdx2);
		final ToIntFunction<int[]> valGetter = mIdx -> mIdx[0];
		if (valArrIdx1 < valArrIdx2) {
			assertTrue("A lesser value array index did not result in a lesser-or-equal matrix row index.",
					valGetter.applyAsInt(mIdx1) <= valGetter.applyAsInt(mIdx2));
		} else if (valArrIdx1 > valArrIdx2) {
			assertTrue("A greater value array index did not result in a greater-or-equal matrix row index.",
					valGetter.applyAsInt(mIdx1) >= valGetter.applyAsInt(mIdx2));
		} else {
			assertTrue("An equal value array index did not result in an equal matrix row index.",
					valGetter.applyAsInt(mIdx1) == valGetter.applyAsInt(mIdx2));
		}
	}

	/**
	 * Test method for {@link se.kth.speech.Matrix#getMatrixIndices(int)}.
	 */
	@Theory
	public void testGetMatrixIndicesUpperBound(final Matrix<Object> m,
			@FromDataPoints("valArrayIdxs") final int valArrIdx,
			@FromDataPoints("matrixIdxPositions") final int matrixIdxPosition) {
		final int[] dims = m.getDimensions();
		final int maxIdx = m.getValues().size();
		assumeTrue(String.format(
				"Provided test array index is greater than the maximum (%d) for the %s instance under test.", maxIdx,
				m.getClass().getSimpleName()), valArrIdx <= maxIdx);
		assumeTrue(String.format("The matrix index position to test is too great for the %s instance under test.",
				m.getClass().getSimpleName()), matrixIdxPosition <= dims.length);
		final int[] mIdxs = m.getMatrixIndices(valArrIdx);
		final int mIdx = mIdxs[matrixIdxPosition];
		final int mIdxUpperBound = dims[matrixIdxPosition];
		assertTrue(String.format(
				"The index for matrix dimension %d for value array index %d is greater than the upper bound (%d, upper bound %d).",
				matrixIdxPosition, valArrIdx, mIdx, mIdxUpperBound), mIdx <= mIdxUpperBound);
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.Matrix#getValues(int, int, int, int)}.
	 */
	@Theory
	public void testGetValuesIntIntIntInt(final Matrix<Object> m) {
		final List<Object> expected = m.getValues();
		final int[] dims = m.getDimensions();
		final Stream<Object> actual = m.getValues(0, dims[0], 0, dims[1]);
		ITER_ASSERTER.accept(expected.iterator(), actual.iterator());
	}

	/**
	 * Test method for {@link se.kth.speech.Matrix#Matrix(E[], int)}.
	 */
	@Theory
	public void testMatrixIllformed(final Object[] vals, final int colCount) {
		assumeTrue(colCount > 0);
		assumeThat(vals.length % colCount, not(0));
		thrown.expect(IllegalArgumentException.class);
		new Matrix<>(vals, colCount);
	}

	/**
	 * Test method for {@link se.kth.speech.Matrix#Matrix(E[], int)}.
	 */
	@Theory
	public void testMatrixNonPositiveColCount(final Object[] vals, final int colCount) {
		assumeTrue(colCount < 1);
		thrown.expect(IllegalArgumentException.class);
		new Matrix<>(vals, colCount);
	}

	/**
	 * Test method for {@link se.kth.speech.Matrix#Matrix(E[], int)}.
	 */
	@Theory
	public void testMatrixPositive(final Object[] vals, final int colCount) {
		assumeTrue(colCount > 0);
		assumeTrue(vals.length >= colCount);
		assumeThat(vals.length % colCount, equalTo(0));
		new Matrix<>(vals, colCount);
	}

	/**
	 * Test method for {@link se.kth.speech.Matrix#Matrix(E[], int)}.
	 */
	@Theory
	public void testMatrixTooFewValues(final Object[] vals, final int colCount) {
		assumeTrue(vals.length < colCount);
		thrown.expect(IllegalArgumentException.class);
		new Matrix<>(vals, colCount);
	}

}
