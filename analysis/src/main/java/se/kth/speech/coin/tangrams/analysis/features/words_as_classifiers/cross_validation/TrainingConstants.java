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

import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues.ClassificationContext;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues.EventDialogueClassifier;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues.IsolatedUtteranceEventDialogueClassifier;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.ParallelizedWordLogisticClassifierTrainer;
import weka.classifiers.functions.Logistic;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Jul 9, 2017
 *
 */
final class TrainingConstants {

	static final int ESTIMATED_UNIQUE_UTT_COUNT = 2000;

	static final Random RND = new Random(1);

	static final Function<ClassificationContext, EventDialogueClassifier> SIMPLE_CLASSIFIER_FACTORY = classificationContext -> {
		final ParallelizedWordLogisticClassifierTrainer trainer = new ParallelizedWordLogisticClassifierTrainer(
				classificationContext.getTrainingData(), classificationContext.getBackgroundJobExecutor());
		final Function<String, Logistic> wordClassifiers = trainer.get()::get;
		// This classifier is statically-trained, i.e. the word models
		// used for classification are the same no matter what dialogue
		// is being classified
		return new IsolatedUtteranceEventDialogueClassifier(diagToClassify -> wordClassifiers,
				classificationContext.getReferentConfidenceMapFactory());
	};

	private TrainingConstants() {
	}

}
