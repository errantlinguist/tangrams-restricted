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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training;

import java.util.Map;
import java.util.function.BiFunction;
import java.util.function.Function;

import se.kth.speech.coin.tangrams.analysis.GameContext;
import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import weka.classifiers.Classifier;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 20 Oct 2017
 *
 */
public final class IterativeWordLogisticClassifierTrainer<C extends Classifier>
		implements BiFunction<EventDialogue, GameContext, Function<String, C>> {

	private final Function<WordClassificationData, ? extends Map<String, C>> decorated;

	private final AbstractInstanceExtractor instExtractor;

	private final WordClassificationData trainingData;

	public IterativeWordLogisticClassifierTrainer(
			final Function<WordClassificationData, ? extends Map<String, C>> decorated,
			final WordClassificationData trainingData, final AbstractInstanceExtractor instExtractor) {
		this.decorated = decorated;
		this.trainingData = trainingData;
		this.instExtractor = instExtractor;

	}

	@Override
	public Function<String, C> apply(final EventDialogue diagToClassify, final GameContext ctx) {
		final Map<String, C> wordClassifier = decorated.apply(trainingData);
		instExtractor.addTrainingData(diagToClassify, ctx.getHistory(), trainingData);
		return wordClassifier::get;
	}

}
