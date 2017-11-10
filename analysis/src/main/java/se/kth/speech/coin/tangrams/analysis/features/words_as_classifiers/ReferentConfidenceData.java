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

import it.unimi.dsi.fastutil.ints.Int2DoubleMap;
import it.unimi.dsi.fastutil.objects.Object2DoubleMap;

public final class ReferentConfidenceData {

	private final String oovClassName;

	private final Int2DoubleMap referentConfidenceVals;

	private final Object2DoubleMap<String> wordClassWeights;

	ReferentConfidenceData(final Int2DoubleMap referentConfidenceVals, final Object2DoubleMap<String> wordClassWeights,
			final String oovClassName) {
		this.referentConfidenceVals = referentConfidenceVals;
		this.wordClassWeights = wordClassWeights;
		this.oovClassName = oovClassName;
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
		if (!(obj instanceof ReferentConfidenceData)) {
			return false;
		}
		final ReferentConfidenceData other = (ReferentConfidenceData) obj;
		if (oovClassName == null) {
			if (other.oovClassName != null) {
				return false;
			}
		} else if (!oovClassName.equals(other.oovClassName)) {
			return false;
		}
		if (referentConfidenceVals == null) {
			if (other.referentConfidenceVals != null) {
				return false;
			}
		} else if (!referentConfidenceVals.equals(other.referentConfidenceVals)) {
			return false;
		}
		if (wordClassWeights == null) {
			if (other.wordClassWeights != null) {
				return false;
			}
		} else if (!wordClassWeights.equals(other.wordClassWeights)) {
			return false;
		}
		return true;
	}

	public double getOovClassWeight() {
		return wordClassWeights.getDouble(oovClassName);
	}

	/**
	 * @return the referentConfidenceVals
	 */
	public Int2DoubleMap getReferentConfidenceVals() {
		return referentConfidenceVals;
	}

	/**
	 * @return the wordClassWeights
	 */
	public Object2DoubleMap<String> getWordClassWeights() {
		return wordClassWeights;
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
		result = prime * result + (oovClassName == null ? 0 : oovClassName.hashCode());
		result = prime * result + (referentConfidenceVals == null ? 0 : referentConfidenceVals.hashCode());
		result = prime * result + (wordClassWeights == null ? 0 : wordClassWeights.hashCode());
		return result;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder(128);
		builder.append("ReferentConfidenceData [referentConfidenceVals=");
		builder.append(referentConfidenceVals);
		builder.append(", wordClassWeights=");
		builder.append(wordClassWeights);
		builder.append(", oovClassName=");
		builder.append(oovClassName);
		builder.append("]");
		return builder.toString();
	}
}