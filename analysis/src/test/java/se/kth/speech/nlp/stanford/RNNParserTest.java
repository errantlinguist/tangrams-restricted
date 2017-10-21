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
package se.kth.speech.nlp.stanford;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import edu.stanford.nlp.ling.CoreAnnotations.SentencesAnnotation;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.pipeline.Annotation;
import edu.stanford.nlp.pipeline.Annotator;
import edu.stanford.nlp.pipeline.StanfordCoreNLP;
import edu.stanford.nlp.trees.Tree;
import edu.stanford.nlp.trees.TreeCoreAnnotations.TreeAnnotation;
import edu.stanford.nlp.util.CoreMap;
import edu.stanford.nlp.util.PropertiesUtils;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 20 Oct 2017
 *
 */

@RunWith(Parameterized.class)
public final class RNNParserTest {

	private static StanfordCoreNLP pipeline;

	@Parameters
	public static List<Object[]> data() {
		return Arrays.asList(new Object[][] {
				{ "I think that's a good idea", Arrays.asList("I", "think", "that", "'s", "a", "good", "idea") },
				{ "", Collections.emptyList() },

		});
	}

	@BeforeClass
	public static void initPipeline() {
		pipeline = new StanfordCoreNLP(
				PropertiesUtils.asProperties("annotators", "tokenize,ssplit,pos,parse", "parse.binaryTrees", "true",
						"parse.model", "edu/stanford/nlp/models/lexparser/englishRNN.ser.gz", "pos.model",
						"edu/stanford/nlp/models/pos-tagger/english-bidirectional/english-bidirectional-distsim.tagger",
						"tokenize.language", "en"));
	}

	private final String input;

	private final List<String> expected;

	public RNNParserTest(final String input, final List<String> expected) {
		this.input = input;
		this.expected = expected;
	}
	
	private static List<String> createParseTreeLeafNodeList(String input, Annotator annotator){
		final edu.stanford.nlp.pipeline.Annotation annot = new edu.stanford.nlp.pipeline.Annotation(input);
		annotator.annotate(annot);
		final List<CoreMap> sents = annot.get(SentencesAnnotation.class);
		final List<String> result = new ArrayList<>(sents.size() * 16);
		for (final CoreMap sent : sents) {
			// this is the parse tree of the current sentence
			final Tree tree = sent.get(TreeAnnotation.class);
			final List<CoreLabel> phaseLabels = tree.taggedLabeledYield();
			for (final CoreLabel phraseLabel : phaseLabels) {
				result.add(phraseLabel.word());
			}
		}
		return result;
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.nlp.stanford.AbstractTokenizer#apply(java.lang.String)}.
	 */
	@Test
	public void testAnnotate() {
		final Annotation annot = new Annotation(input);
		pipeline.annotate(annot);
		final List<CoreMap> sents = annot.get(SentencesAnnotation.class);
		final List<String> actual = createParseTreeLeafNodeList(input, pipeline);
		Assert.assertEquals(expected, actual);
	}

}
