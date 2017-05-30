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
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import edu.stanford.nlp.ling.Label;
import edu.stanford.nlp.trees.CollinsHeadFinder;
import edu.stanford.nlp.trees.HeadFinder;
import edu.stanford.nlp.trees.Tree;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.ChainedEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.DuplicateTokenFilteringEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.EventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.FallbackTokenizingEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.TokenFilteringEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.TokenizingEventDialogueTransformer;
import se.kth.speech.nlp.Disfluencies;
import se.kth.speech.nlp.EnglishLocationalPrepositions;
import se.kth.speech.nlp.PatternTokenizer;
import se.kth.speech.nlp.SnowballPorter2EnglishStopwords;
import se.kth.speech.nlp.stanford.Lemmatizer;
import se.kth.speech.nlp.stanford.PhrasalHeadFilteringPredicate;
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
	NPS_WITHOUT_PPS {

		@Override
		public ChainedEventDialogueTransformer get() {
			final FallbackTokenizingEventDialogueTransformer npWhitelistingTokenizer = ParsingTokenizer.NP_WHITELISTING
					.get();
			final FallbackTokenizingEventDialogueTransformer ppBlacklistingTokenizer = ParsingTokenizer.PP_BLACKLISTING
					.get();
			return new ChainedEventDialogueTransformer(
					Arrays.asList(FILLER_REMOVING_DIAG_TRANSFORMER, npWhitelistingTokenizer, ppBlacklistingTokenizer));
		}

		@Override
		public String getAbbreviation() {
			final Stream.Builder<String> resultBuilder = Stream.builder();
			PARSING_GARBAGE_TOKEN_REMOVING_DIAG_TRANSFORMERS.stream().map(Entry::getKey).forEachOrdered(resultBuilder);
			resultBuilder.add(ParsingTokenizer.NP_WHITELISTING.getAbbreviation());
			resultBuilder.add(ParsingTokenizer.PP_BLACKLISTING.getAbbreviation());
			return resultBuilder.build().collect(TOKENIZER_ABBREV_JOINER);
		}
	},
	PP_REMOVER {

		@Override
		public ChainedEventDialogueTransformer get() {
			final FallbackTokenizingEventDialogueTransformer tokenizer = ParsingTokenizer.PP_BLACKLISTING.get();
			return new ChainedEventDialogueTransformer(Arrays.asList(FILLER_REMOVING_DIAG_TRANSFORMER, tokenizer));
		}

		@Override
		public String getAbbreviation() {
			final Stream.Builder<String> resultBuilder = Stream.builder();
			PARSING_GARBAGE_TOKEN_REMOVING_DIAG_TRANSFORMERS.stream().map(Entry::getKey).forEachOrdered(resultBuilder);
			resultBuilder.add(ParsingTokenizer.PP_BLACKLISTING.getAbbreviation());
			return resultBuilder.build().collect(TOKENIZER_ABBREV_JOINER);
		}
	};

	private enum ParsingTokenizer implements Supplier<FallbackTokenizingEventDialogueTransformer>, HasAbbreviation {
		NP_WHITELISTING {

			@Override
			public FallbackTokenizingEventDialogueTransformer get() {
				final Predicate<Tree> npWhitelistingPred = tree -> {
					// TODO: Fix this: This prunes away the entire root node and
					// so deleted the entire parse tree!
					final boolean result;
					if (tree.isLeaf()) {
						result = true;
					} else if (tree.isPreTerminal()) {
						result = true;
					} else {
						final Label label = tree.label();
						result = label == null ? false : "NP".equals(label.value());
					}
					return result;
				};
				return new FallbackTokenizingEventDialogueTransformer(new se.kth.speech.nlp.stanford.ParsingTokenizer(
						StanfordCoreNLPConfigurationVariant.TOKENIZING_LEMMATIZING_PARSING.get(), npWhitelistingPred),
						FALLBACK_TOKENIZER);
			}

			@Override
			public String getAbbreviation() {
				return "onlyNPs";
			}

		},
		PP_BLACKLISTING {

			@Override
			public FallbackTokenizingEventDialogueTransformer get() {
				final Map<String, Set<List<String>>> labelHeadBlacklists = Collections.singletonMap("PP",
						EnglishLocationalPrepositions.get());
				final PhrasalHeadFilteringPredicate pred = new PhrasalHeadFilteringPredicate(labelHeadBlacklists,
						HEAD_FINDER);
				return new FallbackTokenizingEventDialogueTransformer(
						new se.kth.speech.nlp.stanford.ParsingTokenizer(
								StanfordCoreNLPConfigurationVariant.TOKENIZING_LEMMATIZING_PARSING.get(), pred),
						FALLBACK_TOKENIZER);
			}

			@Override
			public String getAbbreviation() {
				return "noPPs";
			}

		};

	}

	private static final Function<String, List<String>> FALLBACK_TOKENIZER = new PatternTokenizer();

	private static final TokenFilteringEventDialogueTransformer FILLER_REMOVING_DIAG_TRANSFORMER;

	private static final HeadFinder HEAD_FINDER = new CollinsHeadFinder();

	private static final List<Entry<String, EventDialogueTransformer>> PARSING_GARBAGE_TOKEN_REMOVING_DIAG_TRANSFORMERS;

	private static final Collector<CharSequence, ?, String> TOKENIZER_ABBREV_JOINER = Collectors.joining(",");

	static {
		final Set<String> fillerWords = SnowballPorter2EnglishStopwords.Variant.FILLERS.get();
		FILLER_REMOVING_DIAG_TRANSFORMER = new TokenFilteringEventDialogueTransformer(fillerWords);

		final LinkedHashMap<String, EventDialogueTransformer> garbageRemovingTransformers = new LinkedHashMap<>();
		garbageRemovingTransformers.put("dedup", new DuplicateTokenFilteringEventDialogueTransformer());
		final Predicate<String> parsingGarbageTokenMatcher = token -> {
			return fillerWords.contains(token) || Disfluencies.TOKEN_MATCHER.test(token);
		};
		garbageRemovingTransformers.put("noFillers,noDisfl",
				new TokenFilteringEventDialogueTransformer(token -> !parsingGarbageTokenMatcher.test(token)));
		PARSING_GARBAGE_TOKEN_REMOVING_DIAG_TRANSFORMERS = new ArrayList<>(garbageRemovingTransformers.entrySet());
	}

}