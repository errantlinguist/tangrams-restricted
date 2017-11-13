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

import java.util.Map;
import java.util.Set;

import se.kth.speech.coin.tangrams.analysis.tokenization.Cleaning;
import se.kth.speech.coin.tangrams.analysis.tokenization.TokenFiltering;
import se.kth.speech.coin.tangrams.analysis.tokenization.TokenType;
import se.kth.speech.coin.tangrams.analysis.tokenization.Tokenization;

final class TestParameters {

	private final Set<Cleaning> cleaning;

	private final TokenFiltering tokenFiltering;

	private final Tokenization tokenization;

	private final TokenType tokenType;

	private final Training trainingMethod;

	private final Map<WordClassifierTrainingParameter, Object> trainingParams;

	TestParameters(final Set<Cleaning> cleaning, final Tokenization tokenization, final TokenType tokenType,
			final TokenFiltering tokenFiltering, final Training trainingMethod,
			final Map<WordClassifierTrainingParameter, Object> trainingParams) {
		this.cleaning = cleaning;
		this.tokenization = tokenization;
		this.tokenType = tokenType;
		this.tokenFiltering = tokenFiltering;
		this.trainingMethod = trainingMethod;
		this.trainingParams = trainingParams;
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
		if (!(obj instanceof TestParameters)) {
			return false;
		}
		final TestParameters other = (TestParameters) obj;
		if (cleaning == null) {
			if (other.cleaning != null) {
				return false;
			}
		} else if (!cleaning.equals(other.cleaning)) {
			return false;
		}
		if (tokenFiltering != other.tokenFiltering) {
			return false;
		}
		if (tokenType != other.tokenType) {
			return false;
		}
		if (tokenization != other.tokenization) {
			return false;
		}
		if (trainingMethod != other.trainingMethod) {
			return false;
		}
		if (trainingParams == null) {
			if (other.trainingParams != null) {
				return false;
			}
		} else if (!trainingParams.equals(other.trainingParams)) {
			return false;
		}
		return true;
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
	 * @return the trainingParams
	 */
	public Map<WordClassifierTrainingParameter, Object> getTrainingParams() {
		return trainingParams;
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
		result = prime * result + (cleaning == null ? 0 : cleaning.hashCode());
		result = prime * result + (tokenFiltering == null ? 0 : tokenFiltering.hashCode());
		result = prime * result + (tokenType == null ? 0 : tokenType.hashCode());
		result = prime * result + (tokenization == null ? 0 : tokenization.hashCode());
		result = prime * result + (trainingMethod == null ? 0 : trainingMethod.hashCode());
		result = prime * result + (trainingParams == null ? 0 : trainingParams.hashCode());
		return result;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder(512);
		builder.append("TestParameters [cleaning=");
		builder.append(cleaning);
		builder.append(", tokenFiltering=");
		builder.append(tokenFiltering);
		builder.append(", tokenization=");
		builder.append(tokenization);
		builder.append(", tokenType=");
		builder.append(tokenType);
		builder.append(", trainingMethod=");
		builder.append(trainingMethod);
		builder.append(", trainingParams=");
		builder.append(trainingParams);
		builder.append("]");
		return builder.toString();
	}

}