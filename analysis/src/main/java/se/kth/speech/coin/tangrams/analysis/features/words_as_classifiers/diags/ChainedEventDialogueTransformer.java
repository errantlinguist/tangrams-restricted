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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags;

import java.util.Iterator;
import java.util.List;
import java.util.function.Function;

import se.kth.speech.coin.tangrams.analysis.EventDialogue;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since May 25, 2017
 *
 */
public class ChainedEventDialogueTransformer implements Function<EventDialogue,EventDialogue> {

	private static Function<? super EventDialogue, EventDialogue> createChainedDialogueTransformer(
			final Iterable<? extends Function<? super EventDialogue, EventDialogue>> diagTransformers) {
		final Iterator<? extends Function<? super EventDialogue, EventDialogue>> transformerIter = diagTransformers
				.iterator();
		Function<? super EventDialogue, EventDialogue> result = transformerIter.next();
		while (transformerIter.hasNext()) {
			result = result.andThen(transformerIter.next());
		}
		return result;
	}

	private final Function<? super EventDialogue, EventDialogue> decorated;

	public ChainedEventDialogueTransformer(
			final List<? extends Function<? super EventDialogue, EventDialogue>> diagTransformers) {
		this(createChainedDialogueTransformer(diagTransformers));
	}

	private ChainedEventDialogueTransformer(final Function<? super EventDialogue, EventDialogue> decorated) {
		this.decorated = decorated;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Function#apply(java.lang.Object)
	 */
	@Override
	public EventDialogue apply(final EventDialogue diag) {
		return decorated.apply(diag);
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
		if (decorated == null) {
			if (other.decorated != null) {
				return false;
			}
		} else if (!decorated.equals(other.decorated)) {
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
		result = prime * result + (decorated == null ? 0 : decorated.hashCode());
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
		builder.append("ChainedEventDialogueTransformer [decorated=");
		builder.append(decorated);
		builder.append("]");
		return builder.toString();
	}

}
