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

import java.util.Arrays;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.DummyEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.EventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.InstructorUtteranceFilteringEventDialogueTransformer;

enum UtteranceFiltering implements Supplier<EventDialogueTransformer>, HasKeyName {
	ALL_UTTS(new DummyEventDialogueTransformer(),
			"allUtts"), INSTRUCTOR_UTTS(new InstructorUtteranceFilteringEventDialogueTransformer(), "instructorUtts");

	private static final Map<String, UtteranceFiltering> INSTANCES_BY_KEY = Arrays.stream(UtteranceFiltering.values())
			.collect(Collectors.toMap(UtteranceFiltering::getKeyName, Function.identity()));

	public static UtteranceFiltering getByKey(final String keyName) {
		final UtteranceFiltering result = INSTANCES_BY_KEY.get(keyName);
		if (result == null) {
			throw new IllegalArgumentException(String.format("No instance for key \"%s\".", keyName));
		}
		return result;
	}

	private final EventDialogueTransformer held;

	private final String keyName;

	private UtteranceFiltering(final EventDialogueTransformer held, final String keyName) {
		this.held = held;
		this.keyName = keyName;
	}

	@Override
	public EventDialogueTransformer get() {
		return held;
	}

	/**
	 * @return the keyName
	 */
	@Override
	public String getKeyName() {
		return keyName;
	}
}