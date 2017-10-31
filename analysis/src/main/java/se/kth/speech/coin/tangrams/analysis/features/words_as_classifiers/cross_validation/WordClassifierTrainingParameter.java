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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.cross_validation;

import java.util.Map;

import org.springframework.beans.factory.BeanFactory;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 30 Oct 2017
 *
 */
public enum WordClassifierTrainingParameter {
	BACKGROUND_DATA_NEGATIVE_EXAMPLE_WEIGHT_FACTOR, BACKGROUND_DATA_POSITIVE_EXAMPLE_WEIGHT_FACTOR, EXPECTED_UNIQUE_UTTERANCE_COUNT, INSTRUCTOR_UTTERANCE_OBSERVATION_WEIGHT, INTERACTION_DATA_NEGATIVE_EXAMPLE_WEIGHT_FACTOR, INTERACTION_DATA_POSITIVE_EXAMPLE_WEIGHT_FACTOR, OTHER_UTTERANCE_OBSERVATION_WEIGHT, TRAINING_SET_SIZE_DISCOUNTING_FACTOR;

	static Map<WordClassifierTrainingParameter, Object> getDefault(final BeanFactory beanFactory) {
		@SuppressWarnings("unchecked")
		final Map<WordClassifierTrainingParameter, Object> result = (Map<WordClassifierTrainingParameter, Object>) beanFactory
				.getBean("training-params");
		return result;
	}
}
