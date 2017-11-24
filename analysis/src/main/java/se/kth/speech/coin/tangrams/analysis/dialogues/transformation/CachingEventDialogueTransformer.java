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
package se.kth.speech.coin.tangrams.analysis.dialogues.transformation;

import com.google.common.cache.LoadingCache;

import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since May 25, 2017
 *
 */
public final class CachingEventDialogueTransformer implements EventDialogueTransformer {

	private final LoadingCache<EventDialogue, EventDialogue> transformedDiags;

	public CachingEventDialogueTransformer(final LoadingCache<EventDialogue, EventDialogue> transformedDiags) {
		this.transformedDiags = transformedDiags;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Function#apply(java.lang.Object)
	 */
	@Override
	public EventDialogue apply(final EventDialogue diag) {
		return transformedDiags.getUnchecked(diag);
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
		if (!(obj instanceof CachingEventDialogueTransformer)) {
			return false;
		}
		final CachingEventDialogueTransformer other = (CachingEventDialogueTransformer) obj;
		if (transformedDiags == null) {
			if (other.transformedDiags != null) {
				return false;
			}
		} else if (!transformedDiags.equals(other.transformedDiags)) {
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
		result = prime * result + (transformedDiags == null ? 0 : transformedDiags.hashCode());
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
		builder.append("CachingEventDialogueTransformer [transformedDiags=");
		builder.append(transformedDiags);
		builder.append(']');
		return builder.toString();
	}

}
