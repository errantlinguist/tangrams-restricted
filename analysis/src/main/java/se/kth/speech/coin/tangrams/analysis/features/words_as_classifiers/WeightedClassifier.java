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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers;

import weka.classifiers.Classifier;

final class WeightedClassifier {

	private final Classifier classifier;

	private final double weight;

	WeightedClassifier(final Classifier classifier, final double weight) {
		this.classifier = classifier;
		this.weight = weight;
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
		if (!(obj instanceof WeightedClassifier)) {
			return false;
		}
		final WeightedClassifier other = (WeightedClassifier) obj;
		if (classifier == null) {
			if (other.classifier != null) {
				return false;
			}
		} else if (!classifier.equals(other.classifier)) {
			return false;
		}
		if (Double.doubleToLongBits(weight) != Double.doubleToLongBits(other.weight)) {
			return false;
		}
		return true;
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
		result = prime * result + (classifier == null ? 0 : classifier.hashCode());
		long temp;
		temp = Double.doubleToLongBits(weight);
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
		final StringBuilder builder = new StringBuilder(96);
		builder.append("WeightedClassifier [classifier=");
		builder.append(classifier);
		builder.append(", weight=");
		builder.append(weight);
		builder.append(']');
		return builder.toString();
	}

	/**
	 * @return the classifier
	 */
	Classifier getClassifier() {
		return classifier;
	}

	/**
	 * @return the weight
	 */
	double getWeight() {
		return weight;
	}
}