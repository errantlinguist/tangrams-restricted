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

import java.util.concurrent.ExecutorService;

import org.springframework.context.ApplicationContext;

import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.EventDialogueTransformer;

final class TrainingContext {

	private final ApplicationContext appCtx;

	private final EventDialogueTransformer diagTransformer;

	// private final TokenFiltering tokenFilteringMethod;
	//
	// private final Tokenization tokenizationMethod;
	//
	// private final UtteranceFiltering uttFilteringMethod;

	private final ExecutorService backgroundJobExecutor;

	TrainingContext(final UtteranceFiltering uttFilteringMethod, final Tokenization tokenizationMethod,
			final TokenFiltering tokenFilteringMethod, final EventDialogueTransformer diagTransformer,
			final ApplicationContext appCtx, final ExecutorService backgroundJobExecutor) {
		// this.uttFilteringMethod = uttFilteringMethod;
		// this.tokenizationMethod = tokenizationMethod;
		// this.tokenFilteringMethod = tokenFilteringMethod;
		this.diagTransformer = diagTransformer;
		this.appCtx = appCtx;
		this.backgroundJobExecutor = backgroundJobExecutor;
	}

	/**
	 * @return the appCtx
	 */
	ApplicationContext getAppCtx() {
		return appCtx;
	}

	/**
	 * @return the backgroundJobExecutor
	 */
	ExecutorService getBackgroundJobExecutor() {
		return backgroundJobExecutor;
	}

	/**
	 * @return the diagTransformer
	 */
	EventDialogueTransformer getDiagTransformer() {
		return diagTransformer;
	}

}