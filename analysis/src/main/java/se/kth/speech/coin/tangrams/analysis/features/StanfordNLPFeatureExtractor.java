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
package se.kth.speech.coin.tangrams.analysis.features;

import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.util.List;
import java.util.Properties;
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
import edu.stanford.nlp.pipeline.StanfordCoreNLP;
import edu.stanford.nlp.trees.Tree;
import edu.stanford.nlp.trees.TreeCoreAnnotations.TreeAnnotation;
import edu.stanford.nlp.util.CoreMap;
import se.kth.speech.coin.tangrams.analysis.Utterance;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Apr 14, 2017
 *
 */
final class StanfordNLPFeatureExtractor implements UtteranceFeatureExtractor {

	/**
	 * {@link DefaultAnnotPipelineHolder} is loaded on the first execution of
	 * {@link StanfordNLPFeatureExtractor#getDefaultAnnotPipeline()} or the first access to
	 * {@link DefaultAnnotPipelineHolder#INSTANCE}, not before.
	 *
	 * @author <a href="http://www.cs.umd.edu/~pugh/">Bill Pugh</a>
	 * @see <a href=
	 *      "https://en.wikipedia.org/wiki/Singleton_pattern#The_solution_of_Bill_Pugh">https://en.wikipedia.org/wiki/Singleton_pattern#The_solution_of_Bill_Pugh</a>
	 */
	private static final class DefaultAnnotPipelineHolder {
		/**
		 * A singleton instance of {@link StanfordCoreNLP}.
		 */
		private static final StanfordCoreNLP INSTANCE = createDefaultAnnotPipeline();
		
		private static StanfordCoreNLP createDefaultAnnotPipeline() {
			final Properties props = new Properties();
			try (final InputStream inStream = DefaultAnnotPipelineHolder.class
					.getResourceAsStream("stanford-corenlp-feature_extraction.properties")) {
				props.load(inStream);
			} catch (final IOException e) {
				throw new UncheckedIOException(e);
			}
			return new StanfordCoreNLP(props);
		}
	}

	private static final Collector<CharSequence, ?, String> TOKEN_FORM_JOINER = Collectors.joining(" ");

	/**
	 * Gets a singleton instance of {@link StanfordCoreNLP}.
	 *
	 * @return The singleton instance.
	 */
	private static StanfordCoreNLP getDefaultAnnotPipeline() {
		return DefaultAnnotPipelineHolder.INSTANCE;
	}

	private final Annotator annotator;

	public StanfordNLPFeatureExtractor() {
		this(getDefaultAnnotPipeline());
	}

	private StanfordNLPFeatureExtractor(final Annotator annotator) {
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
