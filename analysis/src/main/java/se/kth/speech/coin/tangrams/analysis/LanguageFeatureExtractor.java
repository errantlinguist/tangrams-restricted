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
package se.kth.speech.coin.tangrams.analysis;

import java.util.List;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.DoubleStream.Builder;
import java.util.stream.Stream;

import edu.stanford.nlp.ling.CoreAnnotations.PartOfSpeechAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.SentencesAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.TextAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.TokensAnnotation;
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
final class LanguageFeatureExtractor implements UtteranceFeatureExtractor {

	private static final Collector<CharSequence, ?, String> TOKEN_FORM_JOINER = Collectors.joining(" ");

	private final Annotator annotator;

	public LanguageFeatureExtractor(final Annotator annotator) {
		this.annotator = annotator;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.BiConsumer#accept(java.lang.Object,
	 * java.lang.Object)
	 */
	@Override
	public void accept(final Utterance utt, final Builder vals) {
		final Annotation annot = new Annotation(utt.getTokens().stream().collect(TOKEN_FORM_JOINER));
		annotator.annotate(annot);
		final List<CoreMap> sents = annot.get(SentencesAnnotation.class);
		// traversing the words in the current sentence
		// a CoreLabel is a CoreMap with additional token-specific methods
		for (final CoreMap sent : sents) {
			for (final CoreLabel token : sent.get(TokensAnnotation.class)) {
				// this is the text of the token
				final String word = token.get(TextAnnotation.class);
				// System.out.println(word);
				// this is the POS tag of the token
				final String pos = token.get(PartOfSpeechAnnotation.class);
				// System.out.println(pos);
			}
			// this is the parse tree of the current sentence
			final Tree tree = sent.get(TreeAnnotation.class);
			System.err.println(tree);
		}
		// TODO Finish
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see se.kth.speech.coin.tangrams.analysis.UtteranceFeatureExtractor#
	 * createFeatureDescriptions()
	 */
	@Override
	public Stream<String> createFeatureDescriptions() {
		// TODO Auto-generated method stub
		return Stream.empty();
	}

}
