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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

import org.apache.commons.lang3.tuple.Pair;

import com.google.common.cache.LoadingCache;

import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.ling.Label;
import edu.stanford.nlp.pipeline.Annotation;
import edu.stanford.nlp.trees.CollinsHeadFinder;
import edu.stanford.nlp.trees.Tree;
import edu.stanford.nlp.util.CoreMap;
import se.kth.speech.coin.tangrams.analysis.dialogues.transformation.ChainedEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.dialogues.transformation.DuplicateTokenFilteringEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.dialogues.transformation.EventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.dialogues.transformation.TokenFilteringEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.dialogues.transformation.TokenizingEventDialogueTransformer;
import se.kth.speech.nlp.Disfluencies;
import se.kth.speech.nlp.EnglishLocationalPrepositions;
import se.kth.speech.nlp.MetaLanguage;
import se.kth.speech.nlp.SnowballPorter2EnglishStopwords;
import se.kth.speech.nlp.stanford.Lemmatizer;
import se.kth.speech.nlp.stanford.ParsingTokenizer;
import se.kth.speech.nlp.stanford.PhrasalHeadFilteringPredicate;
import se.kth.speech.nlp.stanford.PhraseExtractingParsingTokenizer;
import se.kth.speech.nlp.stanford.StanfordCoreNLPConfigurationVariant;
import se.kth.speech.nlp.stanford.Tokenizer;

enum Tokenization implements Function<Tokenization.Context, EventDialogueTransformer> {
	STANFORD_BASIC {

		@Override
		protected TokenizingEventDialogueTransformer createMainTransformer(final Context context) {
			final Function<String, List<String>> tokenizer;
			switch (context.getTokenType()) {
			case INFLECTED:
				tokenizer = new Tokenizer(
						context.getAnnotationCacheFactory().apply(StanfordCoreNLPConfigurationVariant.TOKENIZING));
				break;
			case LEMMA:
				tokenizer = new Lemmatizer(context.getAnnotationCacheFactory()
						.apply(StanfordCoreNLPConfigurationVariant.TOKENIZING_LEMMATIZING));
				break;
			default:
				throw new AssertionError("No logic for handling enum.");
			}
			return new TokenizingEventDialogueTransformer(tokenizer);
		}
	},
	STANFORD_NPS_ONLY {

		@Override
		protected TokenizingEventDialogueTransformer createMainTransformer(final Context context) {
			final Entry<Function<CoreLabel, String>, StanfordCoreNLPConfigurationVariant> tokenExtractor = TOKEN_TYPE_EXTRACTORS
					.get(context.getTokenType());
			return new TokenizingEventDialogueTransformer(new PhraseExtractingParsingTokenizer(
					context.getAnnotationCacheFactory().apply(tokenExtractor.getValue()), tokenExtractor.getKey(),
					NP_WHITELISTING_PHRASE_MATCHER, context.getExtractionResultsHook()));
		}
	},
	STANFORD_NPS_WITHOUT_PPS {

		@Override
		protected TokenizingEventDialogueTransformer createMainTransformer(final Context context) {
			final Entry<Function<CoreLabel, String>, StanfordCoreNLPConfigurationVariant> tokenExtractor = TOKEN_TYPE_EXTRACTORS
					.get(context.getTokenType());
			return new TokenizingEventDialogueTransformer(new PhraseExtractingParsingTokenizer(
					context.getAnnotationCacheFactory().apply(tokenExtractor.getValue()), tokenExtractor.getKey(),
					NP_WHITELISTING_PHRASE_MATCHER, LOCATIONAL_PP_PRUNING_MATCHER, context.getExtractionResultsHook()));
		}
	},
	STANFORD_PP_REMOVER {

		@Override
		protected TokenizingEventDialogueTransformer createMainTransformer(final Context context) {
			final Entry<Function<CoreLabel, String>, StanfordCoreNLPConfigurationVariant> tokenExtractor = TOKEN_TYPE_EXTRACTORS
					.get(context.getTokenType());
			return new TokenizingEventDialogueTransformer(
					new ParsingTokenizer(context.getAnnotationCacheFactory().apply(tokenExtractor.getValue()),
							LOCATIONAL_PP_PRUNING_MATCHER, tokenExtractor.getKey()));
		}
	};

	static final class Context {

		private final Function<? super StanfordCoreNLPConfigurationVariant, LoadingCache<String, Annotation>> annotationCacheFactory;

		private final Set<Cleaning> cleaning;

		private final BiConsumer<? super CoreMap, ? super List<Tree>> extractionResultsHook;

		private final TokenType tokenType;

		Context(final Set<Cleaning> cleaning, final TokenType tokenType,
				final BiConsumer<? super CoreMap, ? super List<Tree>> extractionResultsHook,
				final Function<? super StanfordCoreNLPConfigurationVariant, LoadingCache<String, Annotation>> annotationCacheFactory) {
			this.cleaning = cleaning;
			this.tokenType = tokenType;
			this.extractionResultsHook = extractionResultsHook;
			this.annotationCacheFactory = annotationCacheFactory;
		}

		/**
		 * @return the annotationCacheFactory
		 */
		private Function<? super StanfordCoreNLPConfigurationVariant, LoadingCache<String, Annotation>> getAnnotationCacheFactory() {
			return annotationCacheFactory;
		}

		/**
		 * @return the cleaning
		 */
		private Set<Cleaning> getCleaning() {
			return cleaning;
		}

		/**
		 * @return the extractionResultsHook
		 */
		private BiConsumer<? super CoreMap, ? super List<Tree>> getExtractionResultsHook() {
			return extractionResultsHook;
		}

		/**
		 * @return the tokenType
		 */
		private TokenType getTokenType() {
			return tokenType;
		}
	}

	private static final List<Cleaning> CLEANING_ORDERING = createCleaningOrderingList();

	private static final Map<Cleaning, Supplier<EventDialogueTransformer>> CLEANING_TRANSFORMERS = createCleaningTransformerSupplierMap();

	private static final PhrasalHeadFilteringPredicate LOCATIONAL_PP_PRUNING_MATCHER = new PhrasalHeadFilteringPredicate(
			Collections.singletonMap("PP", EnglishLocationalPrepositions.get()), new CollinsHeadFinder());

	private static final Predicate<Tree> NP_WHITELISTING_PHRASE_MATCHER = subTree -> {
		final Label label = subTree.label();
		return label == null ? false : "NP".equals(label.value());
	};

	private static final Map<TokenType, Entry<Function<CoreLabel, String>, StanfordCoreNLPConfigurationVariant>> TOKEN_TYPE_EXTRACTORS = createTokenTypeExtractorMap();

	private static List<Cleaning> createCleaningOrderingList() {
		final List<Cleaning> result = Arrays.asList(Cleaning.METALANGUAGE, Cleaning.DISFLUENCIES, Cleaning.FILLERS,
				Cleaning.DUPLICATES);
		assert result.size() == Cleaning.values().length;
		return result;
	}

	private static Map<Cleaning, Supplier<EventDialogueTransformer>> createCleaningTransformerSupplierMap() {
		final Map<Cleaning, Supplier<EventDialogueTransformer>> result = new EnumMap<>(Cleaning.class);
		result.put(Cleaning.DISFLUENCIES,
				() -> new TokenFilteringEventDialogueTransformer(token -> !Disfluencies.isDisfluency(token)));
		result.put(Cleaning.DUPLICATES, () -> new DuplicateTokenFilteringEventDialogueTransformer());
		result.put(Cleaning.FILLERS, () -> {
			final Set<String> fillerWords = SnowballPorter2EnglishStopwords.Variant.FILLERS.get();
			return new TokenFilteringEventDialogueTransformer(fillerWords);
		});
		result.put(Cleaning.METALANGUAGE,
				() -> new TokenFilteringEventDialogueTransformer(token -> !MetaLanguage.isMetaLanguageToken(token)));
		assert result.size() == Cleaning.values().length;
		return result;
	}

	private static Map<TokenType, Entry<Function<CoreLabel, String>, StanfordCoreNLPConfigurationVariant>> createTokenTypeExtractorMap() {
		final Map<TokenType, Entry<Function<CoreLabel, String>, StanfordCoreNLPConfigurationVariant>> result = new EnumMap<>(
				TokenType.class);
		result.put(TokenType.INFLECTED,
				Pair.of(CoreLabel::word, StanfordCoreNLPConfigurationVariant.TOKENIZING_PARSING));
		result.put(TokenType.LEMMA,
				Pair.of(CoreLabel::lemma, StanfordCoreNLPConfigurationVariant.TOKENIZING_LEMMATIZING_PARSING));
		assert result.size() == TokenType.values().length;
		return result;
	}

	@Override
	public EventDialogueTransformer apply(final Context context) {
		final List<EventDialogueTransformer> chain = new ArrayList<>(CLEANING_ORDERING.size() + 1);
		final Set<Cleaning> cleaning = context.getCleaning();
		CLEANING_ORDERING.stream().filter(cleaning::contains).map(CLEANING_TRANSFORMERS::get).map(Supplier::get)
				.forEachOrdered(chain::add);
		chain.add(createMainTransformer(context));
		return new ChainedEventDialogueTransformer(chain);
	}

	protected abstract EventDialogueTransformer createMainTransformer(final Context context);

}