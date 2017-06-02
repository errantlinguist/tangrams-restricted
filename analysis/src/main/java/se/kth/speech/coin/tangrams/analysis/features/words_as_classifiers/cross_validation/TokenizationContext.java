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

import java.util.Set;
import java.util.concurrent.Executor;

final class TokenizationContext {

	private final Set<Cleaning> cleaning;

	private final Executor executor;

	private final TokenType tokenType;

	TokenizationContext(final Set<Cleaning> cleaning, final TokenType tokenType, final Executor executor) {
		this.cleaning = cleaning;
		this.tokenType = tokenType;
		this.executor = executor;
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
	 * @return the tokenType
	 */
	TokenType getTokenType() {
		return tokenType;
	}
}