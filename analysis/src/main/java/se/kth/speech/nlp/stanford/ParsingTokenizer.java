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
package se.kth.speech.nlp.stanford;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

import edu.stanford.nlp.ling.CoreAnnotations.SentencesAnnotation;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.pipeline.Annotation;
import edu.stanford.nlp.pipeline.Annotator;
import edu.stanford.nlp.trees.Tree;
import edu.stanford.nlp.trees.TreeCoreAnnotations.TreeAnnotation;
import edu.stanford.nlp.util.CoreMap;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Apr 14, 2017
 *
 */
public final class ParsingTokenizer extends AbstractTokenizer {

	private final Function<? super CoreLabel, String> labelTokenExtractor;

	private final Predicate<Tree> treePruningPositiveFilter;

	public ParsingTokenizer(final Supplier<? extends Annotator> annotatorSupplier,
			final Predicate<Tree> treePruningPositiveFilter,
			final Function<? super CoreLabel, String> labelTokenExtractor) {
		super(annotatorSupplier);
		this.treePruningPositiveFilter = treePruningPositiveFilter;
		this.labelTokenExtractor = labelTokenExtractor;
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
		final ArrayList<String> result = new ArrayList<>(16 * sents.size());
		// traversing the words in the current sentence
		for (final CoreMap sent : sents) {
			// this is the parse tree of the current sentence
			final Tree tree = sent.get(TreeAnnotation.class);
			final Tree prunedTree = tree.prune(treePruningPositiveFilter);
			if (prunedTree != null) {
				final List<CoreLabel> coreLabels = prunedTree.taggedLabeledYield();
				result.ensureCapacity(result.size() + coreLabels.size());
				for (final CoreLabel coreLabel : coreLabels) {
					final String token = labelTokenExtractor.apply(coreLabel);
					result.add(token);
				}
			}
		}
		return result;
	}

}
