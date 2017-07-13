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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training;

import java.util.List;

import se.kth.speech.coin.tangrams.analysis.features.EntityFeature;

final class BooleanTrainingContexts {

	private final List<EntityFeature.Extractor.Context> negative;

	private final List<EntityFeature.Extractor.Context> positive;

	BooleanTrainingContexts(final List<EntityFeature.Extractor.Context> positive,
			final List<EntityFeature.Extractor.Context> negative) {
		this.positive = positive;
		this.negative = negative;
	}

	/**
	 * @return the negative
	 */
	List<EntityFeature.Extractor.Context> getNegative() {
		return negative;
	}

	/**
	 * @return the positive
	 */
	List<EntityFeature.Extractor.Context> getPositive() {
		return positive;
	}

}