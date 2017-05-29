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

final class TestParameters {

	private final TokenFiltering tokenFilteringMethod;

	private final Tokenization tokenizationMethod;

	private final Training trainingMethod;

	private final UtteranceFiltering uttFilteringMethod;

	TestParameters(final UtteranceFiltering uttFilteringMethod, final Tokenization tokenizationMethod,
			final TokenFiltering tokenFilteringMethod, final Training trainingMethod) {
		this.uttFilteringMethod = uttFilteringMethod;
		this.tokenizationMethod = tokenizationMethod;
		this.tokenFilteringMethod = tokenFilteringMethod;
		this.trainingMethod = trainingMethod;
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
		if (tokenFilteringMethod != other.tokenFilteringMethod) {
			return false;
		}
		if (tokenizationMethod != other.tokenizationMethod) {
			return false;
		}
		if (trainingMethod != other.trainingMethod) {
			return false;
		}
		if (uttFilteringMethod != other.uttFilteringMethod) {
			return false;
		}
		return true;
	}

	/**
	 * @return the tokenFilteringMethod
	 */
	public TokenFiltering getTokenFilteringMethod() {
		return tokenFilteringMethod;
	}

	/**
	 * @return the tokenizationMethod
	 */
	public Tokenization getTokenizationMethod() {
		return tokenizationMethod;
	}

	/**
	 * @return the trainingMethod
	 */
	public Training getTrainingMethod() {
		return trainingMethod;
	}

	/**
	 * @return the uttFilteringMethod
	 */
	public UtteranceFiltering getUttFilteringMethod() {
		return uttFilteringMethod;
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
		result = prime * result + (tokenFilteringMethod == null ? 0 : tokenFilteringMethod.hashCode());
		result = prime * result + (tokenizationMethod == null ? 0 : tokenizationMethod.hashCode());
		result = prime * result + (trainingMethod == null ? 0 : trainingMethod.hashCode());
		result = prime * result + (uttFilteringMethod == null ? 0 : uttFilteringMethod.hashCode());
		return result;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder();
		builder.append("TestParameters [uttFilteringMethod=");
		builder.append(uttFilteringMethod);
		builder.append(", tokenizationMethod=");
		builder.append(tokenizationMethod);
		builder.append(", tokenFilteringMethod=");
		builder.append(tokenFilteringMethod);
		builder.append(", trainingMethod=");
		builder.append(trainingMethod);
		builder.append("]");
		return builder.toString();
	}

}