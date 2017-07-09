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

import java.util.Random;
import java.util.function.Function;

import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.ReferentConfidenceMapFactory;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.EventDialogueClassifier;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.IsolatedUtteranceEventDialogueClassifier;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Jul 9, 2017
 *
 */
final class TrainingConstants {

	static final int ESTIMATED_UNIQUE_UTT_COUNT = 2000;

	static final Random RND = new Random(1);

	static final Function<ReferentConfidenceMapFactory, EventDialogueClassifier> SIMPLE_CLASSIFIER_FACTORY = IsolatedUtteranceEventDialogueClassifier::new;

	private TrainingConstants() {
	}

}