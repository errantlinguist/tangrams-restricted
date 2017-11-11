/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.analysis.
 *
 *  se.kth.speech.coin.tangrams-restricted.analysis is free software: you can redistribute it and/or modify
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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers;

import java.util.Arrays;

import it.unimi.dsi.fastutil.objects.Object2DoubleMap;

public final class ReferentConfidenceData {

	private final String oovClassName;

	private final double[] referentConfidenceVals;

	private final Object2DoubleMap<String> wordClassWeights;

	ReferentConfidenceData(final double[] referentConfidenceVals, final Object2DoubleMap<String> wordClassWeights,
			final String oovClassName) {
		this.referentConfidenceVals = referentConfidenceVals;
		this.wordClassWeights = wordClassWeights;
		this.oovClassName = oovClassName;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		ReferentConfidenceData other = (ReferentConfidenceData) obj;
		if (oovClassName == null) {
			if (other.oovClassName != null)
				return false;
		} else if (!oovClassName.equals(other.oovClassName))
			return false;
		if (!Arrays.equals(referentConfidenceVals, other.referentConfidenceVals))
			return false;
		if (wordClassWeights == null) {
			if (other.wordClassWeights != null)
				return false;
		} else if (!wordClassWeights.equals(other.wordClassWeights))
			return false;
		return true;
	}

	public double getOovClassWeight() {
		final double storedValue = wordClassWeights.getDouble(oovClassName);
		return Double.isNaN(storedValue) ? 0 : storedValue;
	}

	/**
	 * @return the referentConfidenceVals
	 */
	public double[] getReferentConfidenceVals() {
		return referentConfidenceVals;
	}

	/**
	 * @return the wordClassWeights
	 */
	public Object2DoubleMap<String> getWordClassWeights() {
		return wordClassWeights;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((oovClassName == null) ? 0 : oovClassName.hashCode());
		result = prime * result + Arrays.hashCode(referentConfidenceVals);
		result = prime * result + ((wordClassWeights == null) ? 0 : wordClassWeights.hashCode());
		return result;
	}

	@Override
	public String toString() {
		return "ReferentConfidenceData [referentConfidenceVals=" + Arrays.toString(referentConfidenceVals)
				+ ", wordClassWeights=" + wordClassWeights + ", oovClassName=" + oovClassName + "]";
	}
}