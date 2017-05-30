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

import java.util.Collections;
import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

import edu.stanford.nlp.ling.Label;
import edu.stanford.nlp.trees.CollinsHeadFinder;
import edu.stanford.nlp.trees.Tree;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.FallbackTokenizingEventDialogueTransformer;
import se.kth.speech.nlp.EnglishLocationalPrepositions;
import se.kth.speech.nlp.PatternTokenizer;
import se.kth.speech.nlp.stanford.PhrasalHeadFilteringPredicate;
import se.kth.speech.nlp.stanford.PhraseExtractingParsingTokenizer;
import se.kth.speech.nlp.stanford.StanfordCoreNLPConfigurationVariant;

enum ParsingTokenization implements Supplier<FallbackTokenizingEventDialogueTransformer>, HasAbbreviation {
	NP_WHITELISTING {

		@Override
		public FallbackTokenizingEventDialogueTransformer get() {
			return new FallbackTokenizingEventDialogueTransformer(new PhraseExtractingParsingTokenizer(
					StanfordCoreNLPConfigurationVariant.TOKENIZING_LEMMATIZING_PARSING.get(),
					NP_WHITELISTING_PHRASE_MATCHER), FALLBACK_TOKENIZER);
		}

		@Override
		public String getAbbreviation() {
			return "onlyNPs";
		}

	},
	NP_WHITELISTING_PP_PRUNING {

		@Override
		public FallbackTokenizingEventDialogueTransformer get() {
			return new FallbackTokenizingEventDialogueTransformer(new PhraseExtractingParsingTokenizer(
					StanfordCoreNLPConfigurationVariant.TOKENIZING_LEMMATIZING_PARSING.get(),
					NP_WHITELISTING_PHRASE_MATCHER), FALLBACK_TOKENIZER);
		}

		@Override
		public String getAbbreviation() {
			return "onlyNPs" + TokenizationAbbreviations.DELIMITER + "prunedPPs";
		}

	},
	PP_BLACKLISTING {

		@Override
		public FallbackTokenizingEventDialogueTransformer get() {
			return new FallbackTokenizingEventDialogueTransformer(new se.kth.speech.nlp.stanford.ParsingTokenizer(
					StanfordCoreNLPConfigurationVariant.TOKENIZING_LEMMATIZING_PARSING.get(),
					LOCATIONAL_PP_PRUNING_MATCHER), FALLBACK_TOKENIZER);
		}

		@Override
		public String getAbbreviation() {
			return "noPPs";
		}

	};

	private static final Function<String, List<String>> FALLBACK_TOKENIZER = new PatternTokenizer();

	private static final PhrasalHeadFilteringPredicate LOCATIONAL_PP_PRUNING_MATCHER = new PhrasalHeadFilteringPredicate(
			Collections.singletonMap("PP", EnglishLocationalPrepositions.get()), new CollinsHeadFinder());

	private static final Predicate<Tree> NP_WHITELISTING_PHRASE_MATCHER = subTree -> {
		final Label label = subTree.label();
		return label == null ? false : "NP".equals(label.value());
	};
}