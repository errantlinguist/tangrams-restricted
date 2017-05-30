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

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Stream;

import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.ChainedEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.DuplicateSubsequenceFilteringEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.EventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.TokenFilteringEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.TokenizingEventDialogueTransformer;
import se.kth.speech.nlp.Disfluencies;
import se.kth.speech.nlp.SnowballPorter2EnglishStopwords;
import se.kth.speech.nlp.stanford.Lemmatizer;
import se.kth.speech.nlp.stanford.StanfordCoreNLPConfigurationVariant;
import se.kth.speech.nlp.stanford.Tokenizer;

enum Tokenization implements Supplier<EventDialogueTransformer>, HasAbbreviation {
	BASIC_TOKENIZER {

		@Override
		public TokenizingEventDialogueTransformer get() {
			return new TokenizingEventDialogueTransformer(
					new Tokenizer(StanfordCoreNLPConfigurationVariant.TOKENIZING.get()));
		}

		@Override
		public String getAbbreviation() {
			return "tokenized";
		}
	},
	LEMMATIZER {

		@Override
		public TokenizingEventDialogueTransformer get() {
			return new TokenizingEventDialogueTransformer(
					new Lemmatizer(StanfordCoreNLPConfigurationVariant.TOKENIZING_LEMMATIZING.get()));
		}

		@Override
		public String getAbbreviation() {
			return "lemmatized";
		}
	},
	NPS_ONLY {

		@Override
		public ChainedEventDialogueTransformer get() {
			final List<EventDialogueTransformer> chain = new ArrayList<>(
					PARSING_GARBAGE_TOKEN_REMOVING_DIAG_TRANSFORMERS.size() + 1);
			PARSING_GARBAGE_TOKEN_REMOVING_DIAG_TRANSFORMERS.stream().map(Entry::getValue).forEach(chain::add);
			chain.add(ParsingTokenization.NP_WHITELISTING.get());
			return new ChainedEventDialogueTransformer(chain);
		}

		@Override
		public String getAbbreviation() {
			final Stream.Builder<String> resultBuilder = Stream.builder();
			PARSING_GARBAGE_TOKEN_REMOVING_DIAG_TRANSFORMERS.stream().map(Entry::getKey).forEachOrdered(resultBuilder);
			resultBuilder.add(ParsingTokenization.NP_WHITELISTING.getAbbreviation());
			return resultBuilder.build().collect(TokenizationAbbreviations.JOINER);
		}
	},
	NPS_WITHOUT_PPS {

		@Override
		public ChainedEventDialogueTransformer get() {
			final List<EventDialogueTransformer> chain = new ArrayList<>(
					PARSING_GARBAGE_TOKEN_REMOVING_DIAG_TRANSFORMERS.size() + 1);
			PARSING_GARBAGE_TOKEN_REMOVING_DIAG_TRANSFORMERS.stream().map(Entry::getValue).forEach(chain::add);
			chain.add(ParsingTokenization.NP_WHITELISTING_PP_PRUNING.get());
			return new ChainedEventDialogueTransformer(chain);
		}

		@Override
		public String getAbbreviation() {
			final Stream.Builder<String> resultBuilder = Stream.builder();
			PARSING_GARBAGE_TOKEN_REMOVING_DIAG_TRANSFORMERS.stream().map(Entry::getKey).forEachOrdered(resultBuilder);
			resultBuilder.add(ParsingTokenization.NP_WHITELISTING_PP_PRUNING.getAbbreviation());
			return resultBuilder.build().collect(TokenizationAbbreviations.JOINER);
		}
	},
	PP_REMOVER {

		@Override
		public ChainedEventDialogueTransformer get() {
			final List<EventDialogueTransformer> chain = new ArrayList<>(
					PARSING_GARBAGE_TOKEN_REMOVING_DIAG_TRANSFORMERS.size() + 1);
			PARSING_GARBAGE_TOKEN_REMOVING_DIAG_TRANSFORMERS.stream().map(Entry::getValue).forEach(chain::add);
			chain.add(ParsingTokenization.PP_BLACKLISTING.get());
			return new ChainedEventDialogueTransformer(chain);
		}

		@Override
		public String getAbbreviation() {
			final Stream.Builder<String> resultBuilder = Stream.builder();
			PARSING_GARBAGE_TOKEN_REMOVING_DIAG_TRANSFORMERS.stream().map(Entry::getKey).forEachOrdered(resultBuilder);
			resultBuilder.add(ParsingTokenization.PP_BLACKLISTING.getAbbreviation());
			return resultBuilder.build().collect(TokenizationAbbreviations.JOINER);
		}
	};

	private static final List<Entry<String, EventDialogueTransformer>> PARSING_GARBAGE_TOKEN_REMOVING_DIAG_TRANSFORMERS;

	static {
		final Set<String> fillerWords = SnowballPorter2EnglishStopwords.Variant.FILLERS.get();
		final LinkedHashMap<String, EventDialogueTransformer> garbageRemovingTransformers = new LinkedHashMap<>();
		garbageRemovingTransformers.put("dedupSubseqs", new DuplicateSubsequenceFilteringEventDialogueTransformer());
		final Predicate<String> parsingGarbageTokenMatcher = token -> {
			return fillerWords.contains(token) || Disfluencies.TOKEN_MATCHER.test(token);
		};
		garbageRemovingTransformers.put("noFillers" + TokenizationAbbreviations.DELIMITER + "noDisfl",
				new TokenFilteringEventDialogueTransformer(token -> !parsingGarbageTokenMatcher.test(token)));
		PARSING_GARBAGE_TOKEN_REMOVING_DIAG_TRANSFORMERS = new ArrayList<>(garbageRemovingTransformers.entrySet());
	}

}