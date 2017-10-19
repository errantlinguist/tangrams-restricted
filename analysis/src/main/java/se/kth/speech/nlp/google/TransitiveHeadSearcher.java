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
import java.util.Collections;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.function.IntFunction;
import java.util.function.Predicate;

import com.google.cloud.language.v1.DependencyEdge;
import com.google.cloud.language.v1.Token;
import com.google.common.collect.Sets;

final class TransitiveHeadSearcher implements Function<Token, TransitiveHeadSearcher.Result> {

	static final class Result {

		private final List<Token> chain;

		private final boolean wasHeadFound;

		private Result(final List<Token> chain, final boolean wasHeadFound) {
			this.chain = chain;
			this.wasHeadFound = wasHeadFound;
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
		List<Token> getChain() {
			return chain;
		}

		/**
		 * @return the wasHeadFound
		 */
		boolean wasHeadFound() {
			return wasHeadFound;
		}
	}

	private static final List<Token> EMPTY_LIST = Collections.emptyList();

	private final Predicate<? super Token> headTokenFilter;

	private final Map<Token, Result> lookupTable;

	private final IntFunction<Token> tokenByIdxGetter;

	private final int expectedMaxChainLength;

	TransitiveHeadSearcher(final IntFunction<Token> tokenByIdxGetter, final Predicate<? super Token> headTokenFilter,
			final Map<Token, Result> lookupTable, final int expectedMaxChainLength) {
		this.tokenByIdxGetter = tokenByIdxGetter;
		this.headTokenFilter = headTokenFilter;
		this.lookupTable = lookupTable;
		this.expectedMaxChainLength = expectedMaxChainLength;
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
		final List<Token> chain;
		final boolean wasHeadFound;
		if (headTokenFilter.test(token)) {
			chain = Collections.singletonList(token);
			wasHeadFound = true;
		} else if (token.hasDependencyEdge()) {
			final List<Token> mutableChain = new ArrayList<>(expectedMaxChainLength);
			mutableChain.add(token);
			final Set<Token> visitedTokens = Sets.newHashSetWithExpectedSize(expectedMaxChainLength);
			visitedTokens.add(token);

			Token currentToken = token;
			boolean wasIntermediateHeadFound = false;
			int newChainNodesTraversed = 0;
			do {
				final DependencyEdge dependencyEdge = currentToken.getDependencyEdge();
				final int intermediateTokenIdx = dependencyEdge.getHeadTokenIndex();
				final Token intermediateToken = tokenByIdxGetter.apply(intermediateTokenIdx);
				final Result intermediateResult = lookupTable.get(intermediateToken);
				if (intermediateResult == null) {
					// No memoized intermediate results were found; Compute the
					// path
					if (visitedTokens.add(intermediateToken)) {
						mutableChain.add(intermediateToken);
						newChainNodesTraversed++;
						if (wasIntermediateHeadFound = headTokenFilter.test(intermediateToken)) {
							// The head was found; stop searching
							break;
						} else {
							currentToken = intermediateToken;
						}
					} else {
						// A cycle was found; No head can ever be reached
						wasIntermediateHeadFound = false;
						break;
					}

				} else {
					// The rest of the chain has already been traversed
					mutableChain.addAll(intermediateResult.getChain());
					wasIntermediateHeadFound = intermediateResult.wasHeadFound();
					break;
				}

			} while (currentToken.hasDependencyEdge());

			wasHeadFound = wasIntermediateHeadFound;

			final int totalChainLength = mutableChain.size();
			// Start at index 1 because the start of the chain will be added to
			// the lookup table outside of this method
			final ListIterator<Token> intermediateTokenIter = mutableChain.listIterator(1);
			for (int newChainNodeIterNo = 0; newChainNodeIterNo < newChainNodesTraversed; ++newChainNodeIterNo) {
				final int intermediateTokenIdx = intermediateTokenIter.nextIndex();
				final Token intermediateToken = intermediateTokenIter.next();
				final List<Token> intermediateChain = mutableChain.subList(intermediateTokenIdx, totalChainLength);
				final Result oldIntermediateResult = lookupTable.put(intermediateToken,
						new Result(Collections.unmodifiableList(intermediateChain), wasHeadFound));
				assert oldIntermediateResult == null;
			}

			chain = Collections.unmodifiableList(mutableChain);
		} else {
			chain = EMPTY_LIST;
			wasHeadFound = false;
		}

		return new Result(chain, wasHeadFound);
	}

}