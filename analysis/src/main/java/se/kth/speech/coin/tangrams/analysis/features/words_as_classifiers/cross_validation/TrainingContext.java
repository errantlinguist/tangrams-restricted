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

import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.function.Consumer;

import org.springframework.context.ApplicationContext;

import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.EventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.UtteranceRelation;

final class TrainingContext {

	private final ApplicationContext appCtx;

	private final ExecutorService backgroundJobExecutor;

	private final EventDialogueTransformer diagTransformer;

	private final Consumer<? super List<UtteranceRelation>> uttRelHandler;

	TrainingContext(final EventDialogueTransformer diagTransformer, final ApplicationContext appCtx,
			final ExecutorService backgroundJobExecutor,
			final Consumer<? super List<UtteranceRelation>> uttRelHandler) {
		this.diagTransformer = diagTransformer;
		this.appCtx = appCtx;
		this.backgroundJobExecutor = backgroundJobExecutor;
		this.uttRelHandler = uttRelHandler;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(final Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (!(obj instanceof TrainingContext)) {
			return false;
		}
		final TrainingContext other = (TrainingContext) obj;
		if (appCtx == null) {
			if (other.appCtx != null) {
				return false;
			}
		} else if (!appCtx.equals(other.appCtx)) {
			return false;
		}
		if (backgroundJobExecutor == null) {
			if (other.backgroundJobExecutor != null) {
				return false;
			}
		} else if (!backgroundJobExecutor.equals(other.backgroundJobExecutor)) {
			return false;
		}
		if (diagTransformer == null) {
			if (other.diagTransformer != null) {
				return false;
			}
		} else if (!diagTransformer.equals(other.diagTransformer)) {
			return false;
		}
		if (uttRelHandler == null) {
			if (other.uttRelHandler != null) {
				return false;
			}
		} else if (!uttRelHandler.equals(other.uttRelHandler)) {
			return false;
		}
		return true;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (appCtx == null ? 0 : appCtx.hashCode());
		result = prime * result + (backgroundJobExecutor == null ? 0 : backgroundJobExecutor.hashCode());
		result = prime * result + (diagTransformer == null ? 0 : diagTransformer.hashCode());
		result = prime * result + (uttRelHandler == null ? 0 : uttRelHandler.hashCode());
		return result;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder();
		builder.append("TrainingContext [appCtx=");
		builder.append(appCtx);
		builder.append(", backgroundJobExecutor=");
		builder.append(backgroundJobExecutor);
		builder.append(", diagTransformer=");
		builder.append(diagTransformer);
		builder.append(", uttRelHandler=");
		builder.append(uttRelHandler);
		builder.append("]");
		return builder.toString();
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

	/**
	 * @return the uttRelHandler
	 */
	Consumer<? super List<UtteranceRelation>> getUttRelHandler() {
		return uttRelHandler;
	}

}