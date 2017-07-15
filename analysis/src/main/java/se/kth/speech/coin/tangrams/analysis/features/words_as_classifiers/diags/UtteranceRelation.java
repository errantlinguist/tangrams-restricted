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

	private final List<Utterance> prevUtts;

	private final Utterance sentimentUtt;

	private final double sentimentValue;

	UtteranceRelation(final Utterance sentimentUtt, final double sentimentValue, final List<Utterance> prevUtts) {
		this.sentimentUtt = sentimentUtt;
		this.sentimentValue = sentimentValue;
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
		if (sentimentUtt == null) {
			if (other.sentimentUtt != null) {
				return false;
			}
		} else if (!sentimentUtt.equals(other.sentimentUtt)) {
			return false;
		}
		if (Double.doubleToLongBits(sentimentValue) != Double.doubleToLongBits(other.sentimentValue)) {
			return false;
		}
		return true;
	}

	/**
	 * @return the prevUtts
	 */
	public List<Utterance> getPrevUtts() {
		return prevUtts;
	}

	/**
	 * @return the sentimentUtt
	 */
	public Utterance getSentimentUtt() {
		return sentimentUtt;
	}

	/**
	 * @return the sentimentValue
	 */
	public double getSentimentValue() {
		return sentimentValue;
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
		result = prime * result + (sentimentUtt == null ? 0 : sentimentUtt.hashCode());
		long temp;
		temp = Double.doubleToLongBits(sentimentValue);
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
		builder.append("UtteranceRelation [sentimentUtt=");
		builder.append(sentimentUtt);
		builder.append(", sentimentValue=");
		builder.append(sentimentValue);
		builder.append(", prevUtts=");
		builder.append(prevUtts);
		builder.append("]");
		return builder.toString();
	}
}