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

import se.kth.speech.coin.tangrams.analysis.GameHistory;
import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeature;
import se.kth.speech.coin.tangrams.analysis.features.weka.EntityInstanceAttributeContext;
import weka.core.DenseInstance;
import weka.core.Instance;
import weka.core.Instances;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 20 Oct 2017
 *
 */
public abstract class AbstractInstanceExtractor {

	private final EntityInstanceAttributeContext entityInstAttrCtx;

	protected AbstractInstanceExtractor(final EntityInstanceAttributeContext entityInstAttrCtx) {
		this.entityInstAttrCtx = entityInstAttrCtx;
	}

	protected abstract void addTrainingData(final EventDialogue uttDialogue, final GameHistory history,
			final WordClassificationData trainingData, double positiveExampleWeightFactor, double negativeExampleWeightFactor);

	protected final Instance createTokenInstance(final Instances classInsts,
			final EntityFeature.Extractor.Context extractionContext, final String classValue) {
		final Instance result = new DenseInstance(entityInstAttrCtx.getAttrs().size());
		result.setDataset(classInsts);
		entityInstAttrCtx.getExtractor().accept(result, extractionContext);
		result.setClassValue(classValue);
		return result;
	}

}
