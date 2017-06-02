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

final class TestParameters {

	private final Set<Cleaning> cleaning;

	private final TokenFiltering tokenFiltering;

	private final Tokenization tokenization;

	private final TokenType tokenType;

	private final Training trainingMethod;

	private final UtteranceFiltering uttFiltering;

	TestParameters(final UtteranceFiltering uttFiltering, final Set<Cleaning> cleaning, final Tokenization tokenization,
			final TokenType tokenType, final TokenFiltering tokenFiltering, final Training trainingMethod) {
		this.uttFiltering = uttFiltering;
		this.cleaning = cleaning;
		this.tokenization = tokenization;
		this.tokenType = tokenType;
		this.tokenFiltering = tokenFiltering;
		this.trainingMethod = trainingMethod;
	}

	/**
	 * @return the cleaning
	 */
	public Set<Cleaning> getCleaning() {
		return cleaning;
	}

	/**
	 * @return the tokenFiltering
	 */
	public TokenFiltering getTokenFiltering() {
		return tokenFiltering;
	}

	/**
	 * @return the tokenization
	 */
	public Tokenization getTokenization() {
		return tokenization;
	}

	/**
	 * @return the tokenType
	 */
	public TokenType getTokenType() {
		return tokenType;
	}

	/**
	 * @return the trainingMethod
	 */
	public Training getTrainingMethod() {
		return trainingMethod;
	}

	/**
	 * @return the uttFiltering
	 */
	public UtteranceFiltering getUttFiltering() {
		return uttFiltering;
	}

}