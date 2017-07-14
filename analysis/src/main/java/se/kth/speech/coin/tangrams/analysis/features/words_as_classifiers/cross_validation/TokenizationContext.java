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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.cross_validation;

import java.util.List;
import java.util.Set;
import java.util.concurrent.Executor;
import java.util.function.BiConsumer;

import edu.stanford.nlp.trees.Tree;
import edu.stanford.nlp.util.CoreMap;

final class TokenizationContext {

	private final Set<Cleaning> cleaning;

	private final Executor executor;

	private final TokenType tokenType;

	private final BiConsumer<? super CoreMap, ? super List<Tree>> extractionResultsHook;

	TokenizationContext(final Set<Cleaning> cleaning, final TokenType tokenType, final Executor executor,
			final BiConsumer<? super CoreMap, ? super List<Tree>> extractionResultsHook) {
		this.cleaning = cleaning;
		this.tokenType = tokenType;
		this.executor = executor;
		this.extractionResultsHook = extractionResultsHook;
	}

	/**
	 * @return the cleaning
	 */
	Set<Cleaning> getCleaning() {
		return cleaning;
	}

	/**
	 * @return the executor
	 */
	Executor getExecutor() {
		return executor;
	}

	/**
	 * @return the extractionResultsHook
	 */
	BiConsumer<? super CoreMap, ? super List<Tree>> getExtractionResultsHook() {
		return extractionResultsHook;
	}

	/**
	 * @return the tokenType
	 */
	TokenType getTokenType() {
		return tokenType;
	}
}