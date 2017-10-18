/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.analysis.
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
package se.kth.speech.nlp.google;

import java.util.Arrays;
import java.util.Map;
import java.util.function.Function;
import java.util.function.IntFunction;
import java.util.function.Predicate;

import com.google.cloud.language.v1.DependencyEdge;
import com.google.cloud.language.v1.Token;

final class TransitiveHeadSearcher implements Function<Token, TransitiveHeadSearcher.Result> {

	static final class Result {

		private final Token[] chain;

		private final boolean wasHeadFound;

		private Result(final Token[] chain, final boolean foundHead) {
			this.chain = chain;
			wasHeadFound = foundHead;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.lang.Object#equals(java.lang.Object)
		 */
		@Override
		public boolean equals(final Object obj) {
			if (this == obj) {
				return true;
			}
			if (obj == null) {
				return false;
			}
			if (!(obj instanceof Result)) {
				return false;
			}
			final Result other = (Result) obj;
			if (!Arrays.equals(chain, other.chain)) {
				return false;
			}
			if (wasHeadFound != other.wasHeadFound) {
				return false;
			}
			return true;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.lang.Object#hashCode()
		 */
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + Arrays.hashCode(chain);
			result = prime * result + (wasHeadFound ? 1231 : 1237);
			return result;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.lang.Object#toString()
		 */
		@Override
		public String toString() {
			final StringBuilder builder = new StringBuilder((chain.length + 1) * 16);
			builder.append("Result [chain=");
			builder.append(Arrays.toString(chain));
			builder.append(", wasHeadFound=");
			builder.append(wasHeadFound);
			builder.append("]");
			return builder.toString();
		}

		/**
		 * @return the chain
		 */
		Token[] getChain() {
			return chain;
		}

		/**
		 * @return the wasHeadFound
		 */
		boolean wasHeadFound() {
			return wasHeadFound;
		}
	}

	private static final Token[] EMPTY_ARRAY = new Token[0];

	private final Predicate<? super Token> headTokenFilter;

	private final Map<Token, Result> lookupTable;

	private final IntFunction<Token> tokenByIdxGetter;

	TransitiveHeadSearcher(final IntFunction<Token> tokenByIdxGetter, final Predicate<? super Token> headTokenFilter,
			final Map<Token, Result> lookupTable) {
		this.tokenByIdxGetter = tokenByIdxGetter;
		this.headTokenFilter = headTokenFilter;
		this.lookupTable = lookupTable;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Function#apply(java.lang.Object)
	 */
	@Override
	public Result apply(final Token token) {
		return lookupTable.computeIfAbsent(token, this::search);
	}

	private Result search(final Token token) {
		final Token[] chain;
		boolean wasHeadFound;
		if (headTokenFilter.test(token)) {
			chain = new Token[] { token };
			wasHeadFound = true;
		} else if (token.hasDependencyEdge()) {
			final DependencyEdge dependencyEdge = token.getDependencyEdge();
			final int headTokenIdx = dependencyEdge.getHeadTokenIndex();
			final Token headToken = tokenByIdxGetter.apply(headTokenIdx);
			final Result intermediateResult = apply(headToken);
			final Token[] intermediateChain = intermediateResult.getChain();
			final int intermediateChainLength = intermediateChain.length;
			chain = new Token[intermediateChainLength + 1];
			chain[0] = token;
			System.arraycopy(intermediateChain, 0, chain, 1, intermediateChainLength);
			wasHeadFound = intermediateResult.wasHeadFound();
		} else {
			chain = EMPTY_ARRAY;
			wasHeadFound = false;
		}

		return new Result(chain, wasHeadFound);
	}

}