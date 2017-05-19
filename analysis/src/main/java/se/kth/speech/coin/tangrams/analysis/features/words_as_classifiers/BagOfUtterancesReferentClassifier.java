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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers;

import java.util.ArrayList;
import java.util.List;

import it.unimi.dsi.fastutil.ints.Int2DoubleMap;
import se.kth.speech.coin.tangrams.analysis.GameContext;
import se.kth.speech.coin.tangrams.analysis.Utterance;
import se.kth.speech.coin.tangrams.analysis.features.ClassificationException;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 18 May 2017
 *
 */
public final class BagOfUtterancesReferentClassifier implements UtteranceSequenceClassifier {

	private final ReferentConfidenceMapFactory referentConfidenceMapFactory;

	public BagOfUtterancesReferentClassifier(ReferentConfidenceMapFactory referentConfidenceMapFactory) {
		this.referentConfidenceMapFactory = referentConfidenceMapFactory;
	}

	/* (non-Javadoc)
	 * @see se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.UtteranceClassifier#apply(se.kth.speech.coin.tangrams.analysis.Utterance[], se.kth.speech.coin.tangrams.analysis.GameContext)
	 */
	@Override
	public Int2DoubleMap apply(final List<Utterance> dialogueUtts, final GameContext uttCtx)
			throws ClassificationException {
		final int estAvgUttTokenCount = 8;
		final List<String> diagTokens = new ArrayList<>(dialogueUtts.size() * estAvgUttTokenCount);
		for (final Utterance dialogUtt : dialogueUtts) {
			diagTokens.addAll(dialogUtt.getTokens());
		}
		return referentConfidenceMapFactory.apply(diagTokens, uttCtx);
	}

}