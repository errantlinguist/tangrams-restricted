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
package se.kth.speech.nlp.stanford;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Deque;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.function.Predicate;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.cache.LoadingCache;

import edu.stanford.nlp.ling.CoreAnnotations.SentencesAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.TokensAnnotation;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.pipeline.Annotation;
import edu.stanford.nlp.trees.Tree;
import edu.stanford.nlp.trees.TreeCoreAnnotations.TreeAnnotation;
import edu.stanford.nlp.util.CoreMap;

/**
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since Apr 14, 2017
 *
 */
public final class PhraseExtractingParsingTokenizer extends AbstractTokenizer {

	private static final Logger LOGGER = LoggerFactory.getLogger(PhraseExtractingParsingTokenizer.class);

	private static void addTokens(final CoreMap sent, final ArrayList<CoreLabel> resultWords) {
		// a CoreLabel is a CoreMap with additional token-specific methods
		final List<CoreLabel> tokens = sent.get(TokensAnnotation.class);
		resultWords.ensureCapacity(resultWords.size() + tokens.size());
		for (final CoreLabel token : tokens) {
			// this is the text of the token
			resultWords.add(token);
		}
	}

	private final AtomicInteger failedExtractionCount = new AtomicInteger(0);

	private final Function<? super CoreLabel, String> labelTokenExtractor;

	private final Predicate<Tree> subTreeBranchPruningPositiveFilter;

	private final Predicate<? super Tree> subTreeMatcher;

	private final BiConsumer<? super CoreMap, ? super List<Tree>> extractionResultsHook;

	public PhraseExtractingParsingTokenizer(final LoadingCache<String, Annotation> cache,
			final Function<? super CoreLabel, String> labelTokenExtractor, final Predicate<? super Tree> subTreeMatcher,
			final BiConsumer<? super CoreMap, ? super List<Tree>> extractionResultsHook) {
		this(cache, labelTokenExtractor, subTreeMatcher, subTreeBranch -> true, extractionResultsHook);
	}

	public PhraseExtractingParsingTokenizer(final LoadingCache<String, Annotation> cache,
			final Function<? super CoreLabel, String> labelTokenExtractor, final Predicate<? super Tree> subTreeMatcher,
			final Predicate<Tree> subTreeBranchPruningPositiveFilter,
			final BiConsumer<? super CoreMap, ? super List<Tree>> extractionResultsHook) {
		super(cache);
		this.labelTokenExtractor = labelTokenExtractor;
		this.subTreeMatcher = subTreeMatcher;
		this.subTreeBranchPruningPositiveFilter = subTreeBranchPruningPositiveFilter;
		this.extractionResultsHook = extractionResultsHook;
	}

	private void handleExtractedPhrases(final CoreMap sent, final List<Tree> extractedPhrases,
			final ArrayList<CoreLabel> resultWords) {
		extractionResultsHook.accept(sent, extractedPhrases);
		if (extractedPhrases.isEmpty()) {
			final int newFailedExtractionCount = failedExtractionCount.incrementAndGet();
			LOGGER.debug("No phrases extracted ({} total failures); Falling back to using entire sentence \"{}\".",
					newFailedExtractionCount, sent.toString());
			addTokens(sent, resultWords);
		} else {
			for (final Tree extractedPhrase : extractedPhrases) {
				final List<CoreLabel> phraseLabels = extractedPhrase.taggedLabeledYield();
				resultWords.addAll(phraseLabels);
			}
		}
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * se.kth.speech.nlp.AbstractStanfordCoreNLPTokenizer#tokenize(edu.stanford.
	 * nlp.pipeline.Annotation)
	 */
	@Override
	protected List<String> tokenize(final Annotation annot) {
		final List<CoreMap> sents = annot.get(SentencesAnnotation.class);
		final ArrayList<CoreLabel> resultWords = new ArrayList<>(16 * sents.size());
		// traversing the words in the current sentence
		for (final CoreMap sent : sents) {
			// this is the parse tree of the current sentence
			final Tree tree = sent.get(TreeAnnotation.class);
			final List<Tree> children = tree.getChildrenAsList();
			final Deque<Tree> subTreesToProcess = new ArrayDeque<>(Math.max(16, children.size()));
			children.forEach(subTreesToProcess::addLast);
			final List<Tree> extractedPhrases = new ArrayList<>();

			while (!subTreesToProcess.isEmpty()) {
				final Tree subTreeToProcess = subTreesToProcess.removeFirst();
				if (subTreeMatcher.test(subTreeToProcess)) {
					final Tree prunedSubTree = subTreeToProcess.prune(subTreeBranchPruningPositiveFilter);
					extractedPhrases.add(prunedSubTree);
				} else {
					subTreeToProcess.getChildrenAsList().forEach(subTreesToProcess::addLast);
				}
			}

			handleExtractedPhrases(sent, extractedPhrases, resultWords);
		}
		final List<String> result = Arrays.asList(resultWords.stream().map(labelTokenExtractor).toArray(String[]::new));
		assert !result.isEmpty();
		return result;
	}

}
