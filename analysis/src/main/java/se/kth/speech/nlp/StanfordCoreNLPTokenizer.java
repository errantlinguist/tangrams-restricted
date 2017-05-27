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
package se.kth.speech.nlp;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;

import edu.stanford.nlp.ling.CoreAnnotations.SentencesAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.TextAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.TokensAnnotation;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.pipeline.Annotation;
import edu.stanford.nlp.pipeline.Annotator;
import edu.stanford.nlp.util.CoreMap;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Apr 14, 2017
 *
 */
public final class StanfordCoreNLPTokenizer implements Function<String, List<String>> {

	private final Annotator annotator;

	private final Predicate<? super String> tokenFilter;

	public StanfordCoreNLPTokenizer(final Annotator annotator) {
		this(annotator, token -> true);
	}

	public StanfordCoreNLPTokenizer(final Annotator annotator, final Predicate<? super String> tokenFilter) {
		this.tokenFilter = tokenFilter;
		this.annotator = annotator;
	}

	@Override
	public List<String> apply(final String input) {
		final Annotation annot = new Annotation(input);
		annotator.annotate(annot);
		final List<CoreMap> sents = annot.get(SentencesAnnotation.class);
		// traversing the words in the current sentence
		final ArrayList<String> result = new ArrayList<>(16 * sents.size());
		for (final CoreMap sent : sents) {
			// a CoreLabel is a CoreMap with additional token-specific methods
			final List<CoreLabel> tokens = sent.get(TokensAnnotation.class);
			result.ensureCapacity(result.size() + tokens.size());
			for (final CoreLabel token : tokens) {
				// this is the text of the token
				final String word = token.get(TextAnnotation.class);
				if (tokenFilter.test(word)) {
					result.add(word);
				}
			}
		}
		return result;
	}

}
