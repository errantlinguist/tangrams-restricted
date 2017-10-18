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

import java.util.ArrayList;
import java.util.Map;
import java.util.function.Function;
import java.util.function.IntFunction;
import java.util.function.Predicate;

import com.google.cloud.language.v1.DependencyEdge;
import com.google.cloud.language.v1.Token;

final class TransitiveHeadSearcher implements Function<Token, TransitiveHeadSearcher.Result> {

	static final class Result {

		private final ArrayList<Token> chain;

		private final boolean wasHeadFound;

		private Result(final ArrayList<Token> chain, final boolean foundHead) {
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
			if (chain == null) {
				if (other.chain != null) {
					return false;
				}
			} else if (!chain.equals(other.chain)) {
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
			result = prime * result + (chain == null ? 0 : chain.hashCode());
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
			final StringBuilder builder = new StringBuilder((chain.size() + 1) * 16);
			builder.append("Result [chain=");
			builder.append(chain);
			builder.append(", wasHeadFound=");
			builder.append(wasHeadFound);
			builder.append("]");
			return builder.toString();
		}

		/**
		 * @return the chain
		 */
		ArrayList<Token> getChain() {
			return chain;
		}

		/**
		 * @return the wasHeadFound
		 */
		boolean wasHeadFound() {
			return wasHeadFound;
		}
	}

	private final Predicate<? super Token> headTokenFilter;

	private final Map<Token, Result> lookupTable;

	private final IntFunction<Token> tokenByIdxGetter;

	private TransitiveHeadSearcher(final IntFunction<Token> tokenByIdxGetter,
			final Predicate<? super Token> headTokenFilter, final Map<Token, Result> lookupTable) {
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
		Token currentToken = token;
		final ArrayList<Token> chain = new ArrayList<>();
		chain.add(currentToken);

		boolean foundHead = false;
		while (currentToken.hasDependencyEdge()) {
			final DependencyEdge dependencyEdge = currentToken.getDependencyEdge();
			final int headTokenIdx = dependencyEdge.getHeadTokenIndex();
			final Token headToken = tokenByIdxGetter.apply(headTokenIdx);
			chain.add(headToken);
			if (foundHead = headTokenFilter.test(headToken)) {
				break;
			} else {
				currentToken = headToken;
			}
		}
		return new Result(chain, foundHead);
	}

}