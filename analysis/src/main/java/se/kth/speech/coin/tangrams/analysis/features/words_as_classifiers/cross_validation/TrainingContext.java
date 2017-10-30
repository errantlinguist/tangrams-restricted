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
import java.util.Map;
import java.util.function.BiConsumer;

import org.springframework.context.ApplicationContext;

import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues.EventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues.UtteranceRelation;

final class TrainingContext {

	private final ApplicationContext appCtx;

	private final EventDialogueTransformer diagTransformer;

	private final Map<WordClassifierTrainingParameter, Object> trainingParams;

	private final BiConsumer<? super EventDialogue, ? super List<UtteranceRelation>> uttRelHandler;

	TrainingContext(final EventDialogueTransformer diagTransformer, final ApplicationContext appCtx,
			final BiConsumer<? super EventDialogue, ? super List<UtteranceRelation>> uttRelHandler,
			final Map<WordClassifierTrainingParameter, Object> trainingParams) {
		this.diagTransformer = diagTransformer;
		this.appCtx = appCtx;
		this.uttRelHandler = uttRelHandler;
		this.trainingParams = trainingParams;
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
		if (diagTransformer == null) {
			if (other.diagTransformer != null) {
				return false;
			}
		} else if (!diagTransformer.equals(other.diagTransformer)) {
			return false;
		}
		if (trainingParams == null) {
			if (other.trainingParams != null) {
				return false;
			}
		} else if (!trainingParams.equals(other.trainingParams)) {
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
		result = prime * result + (diagTransformer == null ? 0 : diagTransformer.hashCode());
		result = prime * result + (trainingParams == null ? 0 : trainingParams.hashCode());
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
		final StringBuilder builder = new StringBuilder(512);
		builder.append("TrainingContext [appCtx=");
		builder.append(appCtx);
		builder.append(", diagTransformer=");
		builder.append(diagTransformer);
		builder.append(", trainingParams=");
		builder.append(trainingParams);
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
	 * @return the diagTransformer
	 */
	EventDialogueTransformer getDiagTransformer() {
		return diagTransformer;
	}

	/**
	 * @return the trainingParams
	 */
	Map<WordClassifierTrainingParameter, Object> getTrainingParams() {
		return trainingParams;
	}

	/**
	 * @return the uttRelHandler
	 */
	BiConsumer<? super EventDialogue, ? super List<UtteranceRelation>> getUttRelHandler() {
		return uttRelHandler;
	}

}