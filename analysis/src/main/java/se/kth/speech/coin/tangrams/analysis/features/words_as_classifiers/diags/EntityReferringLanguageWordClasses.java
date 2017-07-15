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

import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.WeightedWordClass;

public final class EntityReferringLanguageWordClasses {

	private final List<WeightedWordClass> otherEntityNegativeExamples;

	private final List<WeightedWordClass> refNegExamples;

	private final List<WeightedWordClass> refPosExamples;

	EntityReferringLanguageWordClasses(final List<WeightedWordClass> refPosExamples,
			final List<WeightedWordClass> refNegExamples, final List<WeightedWordClass> otherEntityNegativeExamples) {
		this.refPosExamples = refPosExamples;
		this.refNegExamples = refNegExamples;
		this.otherEntityNegativeExamples = otherEntityNegativeExamples;
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
		if (!(obj instanceof EntityReferringLanguageWordClasses)) {
			return false;
		}
		final EntityReferringLanguageWordClasses other = (EntityReferringLanguageWordClasses) obj;
		if (otherEntityNegativeExamples == null) {
			if (other.otherEntityNegativeExamples != null) {
				return false;
			}
		} else if (!otherEntityNegativeExamples.equals(other.otherEntityNegativeExamples)) {
			return false;
		}
		if (refNegExamples == null) {
			if (other.refNegExamples != null) {
				return false;
			}
		} else if (!refNegExamples.equals(other.refNegExamples)) {
			return false;
		}
		if (refPosExamples == null) {
			if (other.refPosExamples != null) {
				return false;
			}
		} else if (!refPosExamples.equals(other.refPosExamples)) {
			return false;
		}
		return true;
	}

	/**
	 * @return the otherEntityNegativeExamples
	 */
	public List<WeightedWordClass> getOtherEntityNegativeExamples() {
		return otherEntityNegativeExamples;
	}

	/**
	 * @return the refNegExamples
	 */
	public List<WeightedWordClass> getRefNegExamples() {
		return refNegExamples;
	}

	/**
	 * @return the refPosExamples
	 */
	public List<WeightedWordClass> getRefPosExamples() {
		return refPosExamples;
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
		result = prime * result + (otherEntityNegativeExamples == null ? 0 : otherEntityNegativeExamples.hashCode());
		result = prime * result + (refNegExamples == null ? 0 : refNegExamples.hashCode());
		result = prime * result + (refPosExamples == null ? 0 : refPosExamples.hashCode());
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
		builder.append("EntityReferringLanguageWordClasses [refPosExamples=");
		builder.append(refPosExamples);
		builder.append(", refNegExamples=");
		builder.append(refNegExamples);
		builder.append(", otherEntityNegativeExamples=");
		builder.append(otherEntityNegativeExamples);
		builder.append("]");
		return builder.toString();
	}

}