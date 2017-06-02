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
import java.util.EnumMap;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Predicate;

import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.ling.Label;
import edu.stanford.nlp.trees.CollinsHeadFinder;
import edu.stanford.nlp.trees.Tree;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.TokenizingEventDialogueTransformer;
import se.kth.speech.nlp.EnglishLocationalPrepositions;
import se.kth.speech.nlp.stanford.ParsingTokenizer;
import se.kth.speech.nlp.stanford.PhrasalHeadFilteringPredicate;
import se.kth.speech.nlp.stanford.PhraseExtractingParsingTokenizer;
import se.kth.speech.nlp.stanford.StanfordCoreNLPConfigurationVariant;

enum ParsingTokenization implements Function<TokenizationContext, TokenizingEventDialogueTransformer> {
	NP_WHITELISTING {

		@Override
		public TokenizingEventDialogueTransformer apply(final TokenizationContext context) {
			final Function<CoreLabel, String> tokenExtractor = TOKEN_TYPE_EXTRACTORS.get(context.getTokenType());
			return new TokenizingEventDialogueTransformer(new PhraseExtractingParsingTokenizer(
					StanfordCoreNLPConfigurationVariant.TOKENIZING_PARSING.apply(context.getExecutor()), tokenExtractor,
					NP_WHITELISTING_PHRASE_MATCHER));
		}

	},
	NP_WHITELISTING_PP_PRUNING {

		@Override
		public TokenizingEventDialogueTransformer apply(final TokenizationContext context) {
			final Function<CoreLabel, String> tokenExtractor = TOKEN_TYPE_EXTRACTORS.get(context.getTokenType());
			return new TokenizingEventDialogueTransformer(new PhraseExtractingParsingTokenizer(
					StanfordCoreNLPConfigurationVariant.TOKENIZING_PARSING.apply(context.getExecutor()), tokenExtractor,
					NP_WHITELISTING_PHRASE_MATCHER, LOCATIONAL_PP_PRUNING_MATCHER));
		}
	},
	PP_BLACKLISTING {

		@Override
		public TokenizingEventDialogueTransformer apply(final TokenizationContext context) {
			final Function<CoreLabel, String> tokenExtractor = TOKEN_TYPE_EXTRACTORS.get(context.getTokenType());
			return new TokenizingEventDialogueTransformer(new ParsingTokenizer(
					StanfordCoreNLPConfigurationVariant.TOKENIZING_PARSING.apply(context.getExecutor()),
					LOCATIONAL_PP_PRUNING_MATCHER, tokenExtractor));
		}
	};

	private static final PhrasalHeadFilteringPredicate LOCATIONAL_PP_PRUNING_MATCHER = new PhrasalHeadFilteringPredicate(
			Collections.singletonMap("PP", EnglishLocationalPrepositions.get()), new CollinsHeadFinder());

	private static final Predicate<Tree> NP_WHITELISTING_PHRASE_MATCHER = subTree -> {
		final Label label = subTree.label();
		return label == null ? false : "NP".equals(label.value());
	};

	private static final Map<TokenType, Function<CoreLabel, String>> TOKEN_TYPE_EXTRACTORS = createTokenTypeExtractorMap();

	private static Map<TokenType, Function<CoreLabel, String>> createTokenTypeExtractorMap() {
		final Map<TokenType, Function<CoreLabel, String>> result = new EnumMap<>(TokenType.class);
		result.put(TokenType.INFLECTED, CoreLabel::word);
		result.put(TokenType.LEMMA, CoreLabel::lemma);
		assert result.size() == TokenType.values().length;
		return result;
	}
}