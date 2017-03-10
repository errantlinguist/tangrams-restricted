/*
 *  This file is part of se.kth.speech.coin.tangrams.game.
 *
 *  se.kth.speech.coin.tangrams.game is free software: you can redistribute it and/or modify
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
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 28 Feb 2017
 *
 */
public final class MatrixSimilarity {

	/**
	 * Tests the class
	 * 
	 * @param args
	 *            CLI arguments
	 */
	public static void main(final String[] args) {
		final int colCount = 4;
		final Integer[] mVals1 = new Integer[] { 0, 7, 12, 9, 6, 5, 10, 4, 3, 1, 2, null, 8, null, null, 11 };
		final Matrix<Integer> m1 = new Matrix<>(mVals1, colCount);
		final Integer[] mVals2 = new Integer[] { 0, 7, 12, 9, 6, 5, 10, 4, 3, 1, 2, null, 8, null, 11, null };
		final Matrix<Integer> m2 = new Matrix<>(mVals2, colCount);
		System.out.println(countCellValueInequalities(m1, m2));
		System.out.println(createCellValueSubstitutionMap(m1, m2));

		final Integer[] mVals3 = new Integer[] { 0, 7, 12, 9, 6, 1, 10, 4, 3, 5, 2, null, 8, null, null, 10 };
		final Matrix<Integer> m3 = new Matrix<>(mVals3, colCount);
		System.out.println(Arrays.toString(mVals3));
		final List<Integer> mVals4 = Arrays.asList(Arrays.copyOf(mVals3, mVals3.length));
		System.out.println(mVals4);
		Collections.shuffle(mVals4);
		System.out.println(Arrays.toString(mVals3));
		System.out.println(mVals4);
		final Matrix<Integer> m4 = new Matrix<>(mVals4.toArray(new Integer[mVals4.size()]), colCount);
		System.out.println(countCellValueInequalities(m3, m4));
		System.out.println(createCellValueSubstitutionMap(m3, m4));
	}

	private static <T> int countCellValueInequalities(final Matrix<? extends T> m1, final Matrix<? extends T> m2) {
		final int[] dims = m1.getDimensions();
		if (!Arrays.equals(dims, m2.getDimensions())) {
			throw new IllegalArgumentException("Matrix dimensions not equal.");
		}
		int result = 0;
		for (int row = 0; row < dims[0]; ++row) {
			for (int col = 0; col < dims[1]; ++col) {
				final int[] mIndices = new int[] { row, col };
				final T mVal1 = m1.getValue(mIndices);
				final T mVal2 = m2.getValue(mIndices);
				if (!Objects.equals(mVal1, mVal2)) {
					result++;
				}
			}
		}
		return result;
	}

	private static <T> Map<T, List<T>> createCellValueSubstitutionMap(final Matrix<? extends T> m1, final Matrix<? extends T> m2) {
		final int[] dims = m1.getDimensions();
		if (!Arrays.equals(dims, m2.getDimensions())) {
			throw new IllegalArgumentException("Matrix dimensions not equal.");
		}
		final int valueCount = m1.getValues().size();
		final Map<T, List<T>> result = new HashMap<>(Math.min(valueCount + 1, 16));
		// The number of possible substitutions already found
		int possibleCellValueSubstitutionCount = 0;
		for (int row = 0; row < dims[0]; ++row) {
			for (int col = 0; col < dims[1]; ++col) {
				final int[] mIndices = new int[] { row, col };
				final T mVal1 = m1.getValue(mIndices);
				final T mVal2 = m2.getValue(mIndices);
				if (!Objects.equals(mVal1, mVal2)) {
					{
						final int possibleFutureFoundSubstitutionCount = valueCount
								- possibleCellValueSubstitutionCount;
						final List<T> possibleVal1Substitutions = result.computeIfAbsent(mVal1,
								key -> new ArrayList<>(possibleFutureFoundSubstitutionCount));
						possibleVal1Substitutions.add(mVal2);
						possibleCellValueSubstitutionCount++;
						// Check for a mirror substitution
						result.computeIfPresent(mVal2, (key, oldVal) -> {
							oldVal.remove(mVal1);
							return oldVal.isEmpty() ? null : oldVal;
						});
					}
				}
			}
		}
		return result;
	}

	private MatrixSimilarity() {

	}

}
