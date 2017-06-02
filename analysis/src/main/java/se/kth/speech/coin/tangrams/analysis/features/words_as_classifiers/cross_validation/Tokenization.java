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
import java.util.Arrays;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Supplier;

import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.ChainedEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.DuplicateTokenFilteringEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.EventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.TokenFilteringEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.TokenizingEventDialogueTransformer;
import se.kth.speech.nlp.Disfluencies;
import se.kth.speech.nlp.SnowballPorter2EnglishStopwords;
import se.kth.speech.nlp.stanford.Lemmatizer;
import se.kth.speech.nlp.stanford.StanfordCoreNLPConfigurationVariant;
import se.kth.speech.nlp.stanford.Tokenizer;

enum Tokenization implements Function<TokenizationContext, EventDialogueTransformer> {
	BASIC_TOKENIZER {

		@Override
		public TokenizingEventDialogueTransformer apply(final TokenizationContext context) {
			final Function<String, List<String>> tokenizer;
			switch (context.getTokenType()) {
			case INFLECTED:
				tokenizer = new Tokenizer(StanfordCoreNLPConfigurationVariant.TOKENIZING.apply(context.getExecutor()));
				break;
			case LEMMA:
				tokenizer = new Lemmatizer(
						StanfordCoreNLPConfigurationVariant.TOKENIZING_LEMMATIZING.apply(context.getExecutor()));
				break;
			default:
				throw new AssertionError("No logic for handling enum.");
			}
			return new TokenizingEventDialogueTransformer(tokenizer);
		}
	},
	NPS_ONLY {

		@Override
		public ChainedEventDialogueTransformer apply(final TokenizationContext context) {
			final List<EventDialogueTransformer> chain = new ArrayList<>(CLEANING_ORDERING.size() + 1);
			CLEANING_ORDERING.stream().map(CLEANING_TRANSFORMERS::get).map(Supplier::get).forEachOrdered(chain::add);
			chain.add(ParsingTokenization.NP_WHITELISTING.apply(context));
			return new ChainedEventDialogueTransformer(chain);
		}
	},
	NPS_WITHOUT_PPS {

		@Override
		public ChainedEventDialogueTransformer apply(final TokenizationContext context) {
			final List<EventDialogueTransformer> chain = new ArrayList<>(CLEANING_ORDERING.size() + 1);
			CLEANING_ORDERING.stream().map(CLEANING_TRANSFORMERS::get).map(Supplier::get).forEachOrdered(chain::add);
			chain.add(ParsingTokenization.NP_WHITELISTING_PP_PRUNING.apply(context));
			return new ChainedEventDialogueTransformer(chain);
		}
	},
	PP_REMOVER {

		@Override
		public ChainedEventDialogueTransformer apply(final TokenizationContext context) {
			final List<EventDialogueTransformer> chain = new ArrayList<>(CLEANING_ORDERING.size() + 1);
			CLEANING_ORDERING.stream().map(CLEANING_TRANSFORMERS::get).map(Supplier::get).forEachOrdered(chain::add);
			chain.add(ParsingTokenization.PP_BLACKLISTING.apply(context));
			return new ChainedEventDialogueTransformer(chain);
		}
	};

	private static final List<Cleaning> CLEANING_ORDERING = createCleaningOrderingList();

	private static final Map<Cleaning, Supplier<EventDialogueTransformer>> CLEANING_TRANSFORMERS = createCleaningTransformerSupplierMap();

	private static List<Cleaning> createCleaningOrderingList() {
		final List<Cleaning> result = Arrays.asList(Cleaning.DISFLUENCIES, Cleaning.FILLERS, Cleaning.DUPLICATES);
		assert result.size() == Cleaning.values().length;
		return result;
	}

	private static Map<Cleaning, Supplier<EventDialogueTransformer>> createCleaningTransformerSupplierMap() {
		final Map<Cleaning, Supplier<EventDialogueTransformer>> result = new EnumMap<>(Cleaning.class);
		result.put(Cleaning.DISFLUENCIES,
				() -> new TokenFilteringEventDialogueTransformer(token -> !Disfluencies.TOKEN_MATCHER.test(token)));
		result.put(Cleaning.DUPLICATES, () -> new DuplicateTokenFilteringEventDialogueTransformer());
		result.put(Cleaning.FILLERS, () -> {
			final Set<String> fillerWords = SnowballPorter2EnglishStopwords.Variant.FILLERS.get();
			return new TokenFilteringEventDialogueTransformer(token -> fillerWords.contains(token));
		});
		assert result.size() == Cleaning.values().length;
		return result;
	}

}