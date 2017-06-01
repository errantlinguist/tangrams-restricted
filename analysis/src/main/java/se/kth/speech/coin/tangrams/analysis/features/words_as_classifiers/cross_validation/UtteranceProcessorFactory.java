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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.Executor;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.ling.Label;
import edu.stanford.nlp.pipeline.StanfordCoreNLP;
import edu.stanford.nlp.trees.CollinsHeadFinder;
import edu.stanford.nlp.trees.Tree;
import se.kth.speech.MutablePair;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.CachingEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.ChainedEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.DummyEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.DuplicateTokenFilteringEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.EventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.InstructorUtteranceFilteringEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.TokenFilteringEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.TokenizingEventDialogueTransformer;
import se.kth.speech.nlp.Disfluencies;
import se.kth.speech.nlp.EnglishLocationalPrepositions;
import se.kth.speech.nlp.SnowballPorter2EnglishStopwords;
import se.kth.speech.nlp.stanford.Lemmatizer;
import se.kth.speech.nlp.stanford.ParsingTokenizer;
import se.kth.speech.nlp.stanford.PhrasalHeadFilteringPredicate;
import se.kth.speech.nlp.stanford.PhraseExtractingParsingTokenizer;
import se.kth.speech.nlp.stanford.StanfordCoreNLPConfigurationFactory;
import se.kth.speech.nlp.stanford.Tokenizer;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 1 Jun 2017
 *
 */
public final class UtteranceProcessorFactory implements Function<Executor, EventDialogueTransformer>, HasAbbreviation {

	private static final Predicate<Tree> ANY_SUBTREE_MATCHER = tree -> true;

	private static final InstructorUtteranceFilteringEventDialogueTransformer INST_UTT_DIAG_TRANSFORMER = new InstructorUtteranceFilteringEventDialogueTransformer();

	private static final PhrasalHeadFilteringPredicate LOCATIONAL_PP_PRUNING_MATCHER = new PhrasalHeadFilteringPredicate(
			Collections.singletonMap("PP", EnglishLocationalPrepositions.get()), new CollinsHeadFinder());

	private static final Predicate<Tree> NP_WHITELISTING_PHRASE_MATCHER = subTree -> {
		final Label label = subTree.label();
		return label == null ? false : "NP".equals(label.value());
	};

	private static final List<UtteranceProcessingOption> PROCESSING_STEP_ORDERING = createProcessingStepOrdering();

	private static final List<Entry<UtteranceProcessingOption, EventDialogueTransformer>> UNIFIABLE_PRE_PARSING_CLEANERS = createUnifiablePreParsingOptCleanerList();

	private static final Entry<DummyEventDialogueTransformer, String> NAIVE_PROCESSING_METHOD = new MutablePair<>(
			new DummyEventDialogueTransformer(), "none");

	private static List<UtteranceProcessingOption> createProcessingStepOrdering() {
		final List<UtteranceProcessingOption> result = Arrays.asList(UtteranceProcessingOption.INSTRUCTOR_ONLY,
				UtteranceProcessingOption.REMOVE_DISFLUENCIES, UtteranceProcessingOption.REMOVE_FILLERS,
				UtteranceProcessingOption.DEDUPLICATE_TOKENS, UtteranceProcessingOption.NPS_ONLY,
				UtteranceProcessingOption.PP_REMOVAL, UtteranceProcessingOption.PARSE_TOKENIZED,
				UtteranceProcessingOption.LEMMATIZE, UtteranceProcessingOption.RETOKENIZE,
				UtteranceProcessingOption.REMOVE_STOPWORDS);
		assert result.size() == UtteranceProcessingOption.values().length;
		return result;
	}

	private static List<Entry<UtteranceProcessingOption, EventDialogueTransformer>> createUnifiablePreParsingOptCleanerList() {
		final List<Entry<UtteranceProcessingOption, EventDialogueTransformer>> result = new ArrayList<>();
		result.add(new MutablePair<>(UtteranceProcessingOption.REMOVE_DISFLUENCIES,
				new TokenFilteringEventDialogueTransformer(token -> !Disfluencies.TOKEN_MATCHER.test(token))));
		final Set<String> fillerWords = SnowballPorter2EnglishStopwords.Variant.FILLERS.get();
		result.add(new MutablePair<>(UtteranceProcessingOption.REMOVE_FILLERS,
				new TokenFilteringEventDialogueTransformer(token -> !fillerWords.contains(token))));
		result.add(new MutablePair<>(UtteranceProcessingOption.DEDUPLICATE_TOKENS,
				new DuplicateTokenFilteringEventDialogueTransformer()));
		return result;
	}

	private final Set<? super UtteranceProcessingOption> uttProcessingOptions;

	/**
	 *
	 */
	public UtteranceProcessorFactory(final Set<? super UtteranceProcessingOption> uttProcessingOptions) {
		this.uttProcessingOptions = uttProcessingOptions;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Function#apply(java.lang.Object)
	 */
	@Override
	public EventDialogueTransformer apply(final Executor executor) {
		final EventDialogueTransformer result;
		if (uttProcessingOptions.isEmpty()) {
			result = NAIVE_PROCESSING_METHOD.getKey();
		} else {
			final List<EventDialogueTransformer> chain = new ArrayList<>(uttProcessingOptions.size());
			if (uttProcessingOptions.contains(UtteranceProcessingOption.INSTRUCTOR_ONLY)) {
				chain.add(INST_UTT_DIAG_TRANSFORMER);
			}

			for (final Entry<UtteranceProcessingOption, EventDialogueTransformer> preParsingOptCleaner : UNIFIABLE_PRE_PARSING_CLEANERS) {
				if (uttProcessingOptions.contains(preParsingOptCleaner.getKey())) {
					chain.add(preParsingOptCleaner.getValue());
				}
			}

			final Optional<TokenizingEventDialogueTransformer> parsingTokenizer = createTokenizingTranformer(executor);
			parsingTokenizer.ifPresent(chain::add);

			// Add stopword filter to the chain after parsing in order to
			// prevent it from negatively affecting parsing accuracy
			if (uttProcessingOptions.contains(UtteranceProcessingOption.REMOVE_STOPWORDS)) {
				final Set<String> stopWords = SnowballPorter2EnglishStopwords.Variant.CANONICAL.get();
				chain.add(new TokenFilteringEventDialogueTransformer(token -> !stopWords.contains(token)));
			}
			result = new CachingEventDialogueTransformer(new ChainedEventDialogueTransformer(chain));
		}
		return result;
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
		if (!(obj instanceof UtteranceProcessorFactory)) {
			return false;
		}
		final UtteranceProcessorFactory other = (UtteranceProcessorFactory) obj;
		if (uttProcessingOptions == null) {
			if (other.uttProcessingOptions != null) {
				return false;
			}
		} else if (!uttProcessingOptions.equals(other.uttProcessingOptions)) {
			return false;
		}
		return true;
	}

	@Override
	public String getAbbreviation() {
		return uttProcessingOptions.isEmpty() ? NAIVE_PROCESSING_METHOD.getValue()
				: PROCESSING_STEP_ORDERING.stream().filter(uttProcessingOptions::contains)
						.map(UtteranceProcessingOption::getAbbreviation).collect(TokenizationAbbreviations.JOINER);
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
		result = prime * result + (uttProcessingOptions == null ? 0 : uttProcessingOptions.hashCode());
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
		builder.append("UtteranceProcessorFactory [uttProcessingOptions=");
		builder.append(uttProcessingOptions);
		builder.append("]");
		return builder.toString();
	}

	private Optional<TokenizingEventDialogueTransformer> createTokenizingTranformer(final Executor executor) {
		final Optional<TokenizingEventDialogueTransformer> result;
		final boolean lemmatize = uttProcessingOptions.contains(UtteranceProcessingOption.LEMMATIZE);
		final Function<CoreLabel, String> parserTokenizer = lemmatize ? CoreLabel::lemma : CoreLabel::word;
		final boolean removePps = uttProcessingOptions.contains(UtteranceProcessingOption.PP_REMOVAL);

		if (uttProcessingOptions.contains(UtteranceProcessingOption.NPS_ONLY)) {
			// Parse NPs and extract them
			// Add secondary PP-removing filter if option is present
			final Predicate<Tree> ppFilter = removePps ? LOCATIONAL_PP_PRUNING_MATCHER : ANY_SUBTREE_MATCHER;
			final Supplier<StanfordCoreNLP> annotatorSupplier = new StanfordCoreNLPConfigurationFactory()
					.apply(EnumSet.of(StanfordCoreNLPConfigurationFactory.Option.PARSE), executor);
			result = Optional
					.of(new TokenizingEventDialogueTransformer(new PhraseExtractingParsingTokenizer(annotatorSupplier,
							parserTokenizer, NP_WHITELISTING_PHRASE_MATCHER, ppFilter)));
		} else if (removePps) {
			// Parse PPs and remove them
			final Supplier<StanfordCoreNLP> annotatorSupplier = new StanfordCoreNLPConfigurationFactory()
					.apply(EnumSet.of(StanfordCoreNLPConfigurationFactory.Option.PARSE), executor);
			result = Optional.of(new TokenizingEventDialogueTransformer(
					new ParsingTokenizer(annotatorSupplier, LOCATIONAL_PP_PRUNING_MATCHER, parserTokenizer)));
		} else if (uttProcessingOptions.contains(UtteranceProcessingOption.PARSE_TOKENIZED)) {
			// Create a parse tree and use the leaf node labels as tokens
			final Supplier<StanfordCoreNLP> annotatorSupplier = new StanfordCoreNLPConfigurationFactory()
					.apply(EnumSet.of(StanfordCoreNLPConfigurationFactory.Option.PARSE), executor);
			result = Optional.of(new TokenizingEventDialogueTransformer(
					new ParsingTokenizer(annotatorSupplier, ANY_SUBTREE_MATCHER, parserTokenizer)));
		} else if (lemmatize) {
			// Use token lemmas rather than the observed token forms
			final Supplier<StanfordCoreNLP> annotatorSupplier = new StanfordCoreNLPConfigurationFactory()
					.apply(EnumSet.of(StanfordCoreNLPConfigurationFactory.Option.LEMMATIZE), executor);
			result = Optional.of(new TokenizingEventDialogueTransformer(new Lemmatizer(annotatorSupplier)));
		} else if (uttProcessingOptions.contains(UtteranceProcessingOption.RETOKENIZE)) {
			// (Re-)tokenize utterances using Stanford CoreNLP
			final Supplier<StanfordCoreNLP> annotatorSupplier = new StanfordCoreNLPConfigurationFactory()
					.apply(EnumSet.noneOf(StanfordCoreNLPConfigurationFactory.Option.class), executor);
			result = Optional.of(new TokenizingEventDialogueTransformer(new Tokenizer(annotatorSupplier)));
		} else {
			result = Optional.empty();
		}
		return result;
	}

}
