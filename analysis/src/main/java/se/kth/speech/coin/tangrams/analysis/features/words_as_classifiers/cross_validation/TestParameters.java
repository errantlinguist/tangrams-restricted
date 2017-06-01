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

	private final Training trainingMethod;

	private final String uttProcessingMethodDesc;

	TestParameters(final String uttProcessingMethodDesc, final Training trainingMethod) {
		this.uttProcessingMethodDesc = uttProcessingMethodDesc;
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
		if (trainingMethod != other.trainingMethod) {
			return false;
		}
		if (uttProcessingMethodDesc == null) {
			if (other.uttProcessingMethodDesc != null) {
				return false;
			}
		} else if (!uttProcessingMethodDesc.equals(other.uttProcessingMethodDesc)) {
			return false;
		}
		return true;
	}

	/**
	 * @return the trainingMethod
	 */
	public Training getTrainingMethod() {
		return trainingMethod;
	}

	/**
	 * @return the uttProcessingMethodDesc
	 */
	public String getUttProcessingMethodDesc() {
		return uttProcessingMethodDesc;
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
		result = prime * result + (trainingMethod == null ? 0 : trainingMethod.hashCode());
		result = prime * result + (uttProcessingMethodDesc == null ? 0 : uttProcessingMethodDesc.hashCode());
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
		builder.append("TestParameters [trainingMethod=");
		builder.append(trainingMethod);
		builder.append(", uttProcessingMethodDesc=");
		builder.append(uttProcessingMethodDesc);
		builder.append("]");
		return builder.toString();
	}

}