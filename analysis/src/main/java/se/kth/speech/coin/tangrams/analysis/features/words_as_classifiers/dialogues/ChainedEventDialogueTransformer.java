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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues;

import java.util.List;

import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since May 25, 2017
 *
 */
public class ChainedEventDialogueTransformer implements EventDialogueTransformer {

	private final List<? extends EventDialogueTransformer> diagTransformers;

	public ChainedEventDialogueTransformer(final List<? extends EventDialogueTransformer> diagTransformers) {
		this.diagTransformers = diagTransformers;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Function#apply(java.lang.Object)
	 */
	@Override
	public EventDialogue apply(final EventDialogue diag) {
		EventDialogue result = diag;
		for (final EventDialogueTransformer diagTransformer : diagTransformers) {
			result = diagTransformer.apply(result);
		}
		return result;
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
		if (!(obj instanceof ChainedEventDialogueTransformer)) {
			return false;
		}
		final ChainedEventDialogueTransformer other = (ChainedEventDialogueTransformer) obj;
		if (diagTransformers == null) {
			if (other.diagTransformers != null) {
				return false;
			}
		} else if (!diagTransformers.equals(other.diagTransformers)) {
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
		result = prime * result + (diagTransformers == null ? 0 : diagTransformers.hashCode());
		return result;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder(128);
		builder.append("ChainedEventDialogueTransformer [diagTransformers=");
		builder.append(diagTransformers);
		builder.append(']');
		return builder.toString();
	}

}
