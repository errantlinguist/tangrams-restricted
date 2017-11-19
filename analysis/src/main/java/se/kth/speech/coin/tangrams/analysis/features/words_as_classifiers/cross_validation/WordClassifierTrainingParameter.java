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

import java.math.BigDecimal;
import java.util.EnumMap;
import java.util.Map;

import javax.annotation.Nonnegative;
import javax.annotation.Nonnull;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 30 Oct 2017
 *
 */
public enum WordClassifierTrainingParameter {
	/**
	 * Valid values should be a non-negative {@link BigDecimal}.
	 */
	BACKGROUND_DATA_NEGATIVE_EXAMPLE_WEIGHT_FACTOR {

		/**
		 * @return A {@link BigDecimal} value.
		 */
		@Override
		public Object parseValue(final String value) {
			return parseNonNegativeBigDecimal(value);
		}

	},
	/**
	 * Valid values should be a non-negative {@link BigDecimal}.
	 */
	BACKGROUND_DATA_POSITIVE_EXAMPLE_WEIGHT_FACTOR {

		/**
		 * @return A {@link BigDecimal} value.
		 */
		@Override
		public Object parseValue(final String value) {
			return parseNonNegativeBigDecimal(value);
		}
	},
	/**
	 * Valid values should be a positive {@link Integer}.
	 */
	CROSS_VALIDATION_ITERATION_COUNT {

		/**
		 * @return An {@link Integer} value.
		 */
		@Override
		public Object parseValue(final String value) {
			return parsePositiveInteger(value);
		}
	},
	/**
	 * Valid values should be a {@link String}.
	 */
	DESCRIPTION {

		/**
		 * @return The same object.
		 */
		@Override
		public Object parseValue(final String value) {
			return value;
		}
	},
	/**
	 * Valid values should be a positive {@link Integer}.
	 */
	EXPECTED_UNIQUE_UTTERANCE_COUNT {

		/**
		 * @return An {@link Integer} value.
		 */
		@Override
		public Object parseValue(final String value) {
			return parsePositiveInteger(value);
		}
	},
	/**
	 * Valid values should be a non-negative {@link BigDecimal}.
	 */
	INSTRUCTOR_UTTERANCE_OBSERVATION_WEIGHT {

		/**
		 * @return A {@link BigDecimal} value.
		 */
		@Override
		public Object parseValue(final String value) {
			return parseNonNegativeBigDecimal(value);
		}
	},
	/**
	 * Valid values should be a non-negative {@link BigDecimal}.
	 */
	INTERACTION_DATA_NEGATIVE_EXAMPLE_WEIGHT_FACTOR {

		/**
		 * @return A {@link BigDecimal} value.
		 */
		@Override
		public Object parseValue(final String value) {
			return parseNonNegativeBigDecimal(value);
		}
	},
	/**
	 * Valid values should be a non-negative {@link BigDecimal}.
	 */
	INTERACTION_DATA_POSITIVE_EXAMPLE_WEIGHT_FACTOR {

		/**
		 * @return A {@link BigDecimal} value.
		 */
		@Override
		public Object parseValue(final String value) {
			return parseNonNegativeBigDecimal(value);
		}
	},
	/**
	 * Valid values should be a non-negative {@link BigDecimal}.
	 */
	OTHER_UTTERANCE_OBSERVATION_WEIGHT {

		/**
		 * @return A {@link BigDecimal} value.
		 */
		@Override
		public Object parseValue(final String value) {
			return parseNonNegativeBigDecimal(value);
		}
	},
	/**
	 * Valid values should be a {@link Long}.
	 */
	RANDOM_SEED {

		/**
		 * @return A {@link Long} value.
		 */
		@Override
		public Object parseValue(final String value) {
			return Long.valueOf(value);
		}
	},
	/**
	 * Valid values should be a positive {@link Integer}.
	 */
	SMOOTHING_MIN_COUNT {

		/**
		 * @return An {@link Integer} value.
		 */
		@Override
		public Object parseValue(final String value) {
			return parsePositiveInteger(value);
		}
	},
	/**
	 * Valid values should be a non-negative {@link Integer}.
	 */
	TRAINING_SET_SIZE_DISCOUNTING_CONSTANT {

		/**
		 * @return An {@link Integer} value.
		 */
		@Override
		public Object parseValue(final String value) {
			return parseNonNegativeInteger(value);
		}
	};

	public static Map<WordClassifierTrainingParameter, Object> createDefaultMap() {
		final Map<WordClassifierTrainingParameter, Object> result = new EnumMap<>(
				WordClassifierTrainingParameter.class);
		result.put(BACKGROUND_DATA_POSITIVE_EXAMPLE_WEIGHT_FACTOR, BigDecimal.ONE);
		result.put(BACKGROUND_DATA_NEGATIVE_EXAMPLE_WEIGHT_FACTOR, BigDecimal.ONE);
		result.put(CROSS_VALIDATION_ITERATION_COUNT, 1);
		result.put(DESCRIPTION, "Default");
		result.put(EXPECTED_UNIQUE_UTTERANCE_COUNT, 2000);
		result.put(INTERACTION_DATA_POSITIVE_EXAMPLE_WEIGHT_FACTOR, BigDecimal.ONE);
		result.put(INTERACTION_DATA_NEGATIVE_EXAMPLE_WEIGHT_FACTOR, BigDecimal.ONE);
		result.put(INSTRUCTOR_UTTERANCE_OBSERVATION_WEIGHT, BigDecimal.ONE);
		result.put(OTHER_UTTERANCE_OBSERVATION_WEIGHT, BigDecimal.ZERO);
		result.put(RANDOM_SEED, 1L);
		result.put(SMOOTHING_MIN_COUNT, 3);
		result.put(TRAINING_SET_SIZE_DISCOUNTING_CONSTANT, 0);
		assert result.size() == WordClassifierTrainingParameter.values().length;
		return result;
	}

	@Nonnegative
	private static BigDecimal parseNonNegativeBigDecimal(@Nonnull final String value) {
		final BigDecimal result = new BigDecimal(value);
		if (result.compareTo(BigDecimal.ZERO) >= 0) {
			return result;
		} else {
			throw new IllegalArgumentException(String.format("Parsed \"%s\" as a negative value.", value));
		}
	}

	@Nonnegative
	private static Integer parseNonNegativeInteger(@Nonnull final String value) {
		final Integer result = Integer.valueOf(value);
		if (result < 0) {
			throw new IllegalArgumentException(String.format("Parsed \"%s\" as a negative value.", value));
		} else {
			return result;
		}
	}

	@Nonnegative
	private static Integer parsePositiveInteger(@Nonnull final String value) {
		final Integer result = Integer.valueOf(value);
		if (result < 1) {
			throw new IllegalArgumentException(String.format("Parsed \"%s\" as a non-positive value.", value));
		} else {
			return result;
		}
	}

	/**
	 * Parses a string representation of this
	 * {@link WordClassifierTrainingParameter}'s value.
	 *
	 * @param value
	 *            The {@link String} representation of the parameter value.
	 * @return The parsed value.
	 */
	abstract Object parseValue(String value);
}
