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

import java.util.Collection;
import java.util.EnumSet;
import java.util.Set;
import java.util.function.Supplier;

import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.DummyEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.EventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.TokenFilteringEventDialogueTransformer;
import se.kth.speech.nlp.SnowballPorter2EnglishStopwordSetFactory;
import se.kth.speech.nlp.SnowballPorter2EnglishStopwords;

enum TokenFiltering implements Supplier<EventDialogueTransformer>, HasAbbreviation {
	NO_FILTER(new DummyEventDialogueTransformer(), "noFilter"), STOPWORDS_FILLERS(
			createStopwordFilteringTransformer(EnumSet.of(SnowballPorter2EnglishStopwords.Variant.CANONICAL,
					SnowballPorter2EnglishStopwords.Variant.FILLERS)),
			"stopsFillers");

	private static TokenFilteringEventDialogueTransformer createStopwordFilteringTransformer(
			final Collection<SnowballPorter2EnglishStopwords.Variant> variantsToUnify) {
		return new TokenFilteringEventDialogueTransformer(createStopwordSet(variantsToUnify));
	}

	private static Set<String> createStopwordSet(
			final Collection<SnowballPorter2EnglishStopwords.Variant> variantsToUnify) {
		final SnowballPorter2EnglishStopwordSetFactory factory = new SnowballPorter2EnglishStopwordSetFactory();
		factory.setVariantsToUnify(variantsToUnify);
		return factory.getObject();
	}

	private final EventDialogueTransformer held;

	private final String keyName;

	private TokenFiltering(final EventDialogueTransformer held, final String keyName) {
		this.held = held;
		this.keyName = keyName;
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

	/**
	 * @return the keyName
	 */
	@Override
	public String getAbbreviation() {
		return keyName;
	}

}