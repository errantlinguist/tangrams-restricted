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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags;

import java.util.List;

import se.kth.speech.coin.tangrams.analysis.Utterance;

public final class UtteranceRelation {

	private final Utterance acceptanceUtt;

	private final double acceptanceValue;

	private final List<Utterance> prevUtts;

	UtteranceRelation(final Utterance acceptanceUtt, final double acceptanceValue, final List<Utterance> prevUtts) {
		this.acceptanceUtt = acceptanceUtt;
		this.acceptanceValue = acceptanceValue;
		this.prevUtts = prevUtts;
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
		if (!(obj instanceof UtteranceRelation)) {
			return false;
		}
		final UtteranceRelation other = (UtteranceRelation) obj;
		if (prevUtts == null) {
			if (other.prevUtts != null) {
				return false;
			}
		} else if (!prevUtts.equals(other.prevUtts)) {
			return false;
		}
		if (acceptanceUtt == null) {
			if (other.acceptanceUtt != null) {
				return false;
			}
		} else if (!acceptanceUtt.equals(other.acceptanceUtt)) {
			return false;
		}
		if (Double.doubleToLongBits(acceptanceValue) != Double.doubleToLongBits(other.acceptanceValue)) {
			return false;
		}
		return true;
	}

	/**
	 * @return the acceptanceUtt
	 */
	public Utterance getAcceptanceUtt() {
		return acceptanceUtt;
	}

	/**
	 * @return the acceptanceValue
	 */
	public double getAcceptanceValue() {
		return acceptanceValue;
	}

	/**
	 * @return the prevUtts
	 */
	public List<Utterance> getPrevUtts() {
		return prevUtts;
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
		result = prime * result + (prevUtts == null ? 0 : prevUtts.hashCode());
		result = prime * result + (acceptanceUtt == null ? 0 : acceptanceUtt.hashCode());
		long temp;
		temp = Double.doubleToLongBits(acceptanceValue);
		result = prime * result + (int) (temp ^ temp >>> 32);
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
		builder.append("UtteranceRelation [acceptanceUtt=");
		builder.append(acceptanceUtt);
		builder.append(", acceptanceValue=");
		builder.append(acceptanceValue);
		builder.append(", prevUtts=");
		builder.append(prevUtts);
		builder.append("]");
		return builder.toString();
	}
}