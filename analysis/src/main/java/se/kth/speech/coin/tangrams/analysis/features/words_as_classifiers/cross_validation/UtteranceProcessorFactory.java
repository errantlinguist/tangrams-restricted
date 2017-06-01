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

import java.util.Collection;
import java.util.concurrent.Executor;
import java.util.function.Function;

import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.EventDialogueTransformer;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 1 Jun 2017
 *
 */
public final class UtteranceProcessorFactory implements Function<Executor, EventDialogueTransformer> {

	private final Collection<UtteranceProcessingOption> uttPreprocessingOptions;

	/**
	 * 
	 */
	public UtteranceProcessorFactory(Collection<UtteranceProcessingOption> uttPreprocessingOptions, Tokenization tokenization) {
		this.uttPreprocessingOptions = uttPreprocessingOptions;
		// TODO Auto-generated constructor stub
	}

	/* (non-Javadoc)
	 * @see java.util.function.Function#apply(java.lang.Object)
	 */
	@Override
	public EventDialogueTransformer apply(Executor t) {
		// TODO Auto-generated method stub
		return null;
	}

}
