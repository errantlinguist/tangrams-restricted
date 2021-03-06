/*
 *  This file is part of Tangrams-restricted.
 *
 *  Tangrams-restricted is free software: you can redistribute it and/or modify
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
package se.kth.speech.coin.tangrams.analysis.tokenization;

import java.util.function.Supplier;

import se.kth.speech.coin.tangrams.analysis.dialogues.transformation.EventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.dialogues.transformation.TokenFilteringEventDialogueTransformer;
import se.kth.speech.nlp.SnowballPorter2EnglishStopwords;

enum TokenFiltering implements Supplier<EventDialogueTransformer> {
	NONE(EventDialogueTransformer.IDENTITY_TRANSFORMER), STOPWORDS(
			new TokenFilteringEventDialogueTransformer(SnowballPorter2EnglishStopwords.Variant.CANONICAL.get()));

	private final EventDialogueTransformer held;

	private TokenFiltering(final EventDialogueTransformer held) {
		this.held = held;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Supplier#get()
	 */
	@Override
	public EventDialogueTransformer get() {
		return held;
	}

}