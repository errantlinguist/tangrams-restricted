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
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.DuplicateTokenFilteringEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.EventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.InstructorUtteranceFilteringEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.TokenFilteringEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.TokenizingEventDialogueTransformer;
import se.kth.speech.nlp.Disfluencies;
import se.kth.speech.nlp.EnglishLocationalPrepositions;
import se.kth.speech.nlp.SnowballPorter2EnglishStopwords;
import se.kth.speech.nlp.stanford.PhrasalHeadFilteringPredicate;
import se.kth.speech.nlp.stanford.PhraseExtractingParsingTokenizer;
import se.kth.speech.nlp.stanford.StanfordCoreNLPConfigurationVariant;

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

	private static final List<UtteranceProcessingOption> PARSING_OPTS = Arrays
			.asList(UtteranceProcessingOption.NPS_ONLY, UtteranceProcessingOption.PP_REMOVAL);

	private static final List<Entry<UtteranceProcessingOption, EventDialogueTransformer>> UNIFIABLE_PRE_PARSING_CLEANERS = createUnifiablePreParsingOptCleanerList();

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

	private final Set<UtteranceProcessingOption> uttProcessingOptions;

	/**
	 *
	 */
	public UtteranceProcessorFactory(final Set<UtteranceProcessingOption> uttProcessingOptions,
			final Tokenization tokenization) {
		this.uttProcessingOptions = uttProcessingOptions;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Function#apply(java.lang.Object)
	 */
	@Override
	public EventDialogueTransformer apply(final Executor executor) {
		final List<EventDialogueTransformer> chain = new ArrayList<>(uttProcessingOptions.size());
		if (uttProcessingOptions.contains(UtteranceProcessingOption.INSTRUCTOR_ONLY)) {
			chain.add(INST_UTT_DIAG_TRANSFORMER);
		}

		for (final Entry<UtteranceProcessingOption, EventDialogueTransformer> preParsingOptCleaner : UNIFIABLE_PRE_PARSING_CLEANERS) {
			if (uttProcessingOptions.contains(preParsingOptCleaner.getKey())) {
				chain.add(preParsingOptCleaner.getValue());
			}
		}

		final boolean lemmatize = uttProcessingOptions.contains(UtteranceProcessingOption.LEMMATIZE);
		final Optional<ParsingTokenization> tokenization = createParsingTokenizer(executor, lemmatize);
		tokenization.ifPresent(tok -> tok.apply(executor));

		// Add stopword filter to the chain after parsing in order to
		// prevent it from negatively affecting parsing accuracy
		if (uttProcessingOptions.contains(UtteranceProcessingOption.REMOVE_STOPWORDS)) {
			final Set<String> fillerWords = SnowballPorter2EnglishStopwords.Variant.CANONICAL.get();
			chain.add(new TokenFilteringEventDialogueTransformer(token -> !fillerWords.contains(token)));
		}
		return new CachingEventDialogueTransformer(new ChainedEventDialogueTransformer(chain));
	}

	@Override
	public String getAbbreviation() {
		uttProcessingOptions.stream().map(UtteranceProcessingOption::getAbbreviation)
				.collect(TokenizationAbbreviations.JOINER);
		// TODO Auto-generated method stub
		return null;
	}

	private Optional<TokenizingEventDialogueTransformer> createParsingTokenizer(final Executor executor,
			final boolean lemmatize) {
		final Optional<TokenizingEventDialogueTransformer> result;
		final Supplier<StanfordCoreNLP> annotatorInst = StanfordCoreNLPConfigurationVariant.TOKENIZING_PARSING
				.apply(executor);
		final Function<CoreLabel, String> parserTokenizer = lemmatize ? CoreLabel::lemma : CoreLabel::word;
		final boolean removePps = uttProcessingOptions.contains(UtteranceProcessingOption.PP_REMOVAL);
		if (uttProcessingOptions.contains(UtteranceProcessingOption.NPS_ONLY)) {
			final Predicate<Tree> ppFilter = removePps ? LOCATIONAL_PP_PRUNING_MATCHER : ANY_SUBTREE_MATCHER;
			final TokenizingEventDialogueTransformer transformer = new TokenizingEventDialogueTransformer(
					new PhraseExtractingParsingTokenizer(annotatorInst, parserTokenizer, NP_WHITELISTING_PHRASE_MATCHER,
							ppFilter));
			result = Optional.of(transformer);
		} else if (removePps) {
			final TokenizingEventDialogueTransformer transformer = new TokenizingEventDialogueTransformer(
					new se.kth.speech.nlp.stanford.ParsingTokenizer(annotatorInst, LOCATIONAL_PP_PRUNING_MATCHER,
							parserTokenizer));
			result = Optional.of(transformer);
		} else {
			result = Optional.empty();
		}
		return result;
	}

}
