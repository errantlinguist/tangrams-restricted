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

enum UtteranceProcessingOption implements HasAbbreviation {
	DEDUPLICATE_TOKENS("dedupTokens"),
	/**
	 * Train and test only on utterances from the dialogue instructor.
	 */
	INSTRUCTOR_ONLY("instrUtts"),
	/**
	 * Uses token lemmas rather than the observed token forms.
	 */
	LEMMATIZE("lemmatized"),
	/**
	 * Parses NPs and extract them.
	 */
	NPS_ONLY("onlyNPs"),
	/**
	 * Creates a parse tree and use the leaf node labels as tokens.
	 */
	PARSE_TOKENIZED("parseTok"),

	/**
	 * Parses PPs and remove them.
	 */
	PP_REMOVAL("prunedPPs"), REMOVE_DISFLUENCIES("noDisfl"), REMOVE_FILLERS("noFillers"), REMOVE_STOPWORDS("noStops"),
	/**
	 * (Re-)tokenizes the utterance using Stanford CoreNLP's
	 * {@link edu.stanford.nlp.process.PTBTokenizer<T>}.
	 *
	 * @see <a href="https://nlp.stanford.edu/software/tokenizer.html">Stanford
	 *      CoreNLP website</a>
	 */
	RETOKENIZE("retokenized");

	private final String abbrev;

	private UtteranceProcessingOption(final String abbrev) {
		this.abbrev = abbrev;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.
	 * cross_validation.HasAbbreviation#getAbbreviation()
	 */
	@Override
	public String getAbbreviation() {
		return abbrev;
	}
}