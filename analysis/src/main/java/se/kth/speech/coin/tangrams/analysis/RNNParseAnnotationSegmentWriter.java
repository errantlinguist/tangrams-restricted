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

import java.io.IOException;
import java.nio.file.FileVisitOption;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.xml.bind.JAXBException;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;

import edu.stanford.nlp.ling.CoreAnnotations.SentencesAnnotation;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.pipeline.Annotator;
import edu.stanford.nlp.pipeline.StanfordCoreNLP;
import edu.stanford.nlp.trees.Tree;
import edu.stanford.nlp.trees.TreeCoreAnnotations.TreeAnnotation;
import edu.stanford.nlp.util.CoreMap;
import edu.stanford.nlp.util.PropertiesUtils;
import se.kth.speech.higgins._2005.annotation.Annotation;
import se.kth.speech.higgins._2005.annotation.Annotation.Segments.Segment;
import se.kth.speech.higgins._2005.annotation.Transcription.T;
import se.kth.speech.higgins.io.HatIO;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Oct 21, 2017
 *
 */
public final class RNNParseAnnotationSegmentWriter { // NO_UCD (unused code)

	private static final Collector<CharSequence, ?, String> LIST_VALUE_JOINER = Collectors.joining(",");

	private static final Collector<CharSequence, ?, String> ROW_CELL_JOINER = Collectors.joining("\t");

	private static final Collector<CharSequence, ?, String> TOKEN_JOINER = Collectors.joining(" ");

	public static void main(final String[] args) throws IOException, JAXBException {
		final Path[] inpaths = Arrays.stream(args).map(String::trim).filter(path -> !path.isEmpty()).map(Paths::get)
				.toArray(Path[]::new);
		if (inpaths.length < 1) {
			throw new IllegalArgumentException(
					String.format("Usage: %s INPATHS...", RNNParseAnnotationSegmentWriter.class.getName()));
		} else {
			final StanfordCoreNLP pipeline = createPipeline();
			System.out.println(Stream.of("INPUT", "LEAF_NODES").collect(ROW_CELL_JOINER));
			final LoadingCache<String, List<String>> parses = CacheBuilder.newBuilder().initialCapacity(500)
					.maximumSize(500).build(CacheLoader.from(input -> createParseTreeLeafNodeList(input, pipeline)));
			for (final Path inpath : inpaths) {
				final Iterable<Path> infiles = Files.walk(inpath, FileVisitOption.FOLLOW_LINKS)
						.filter(Files::isRegularFile)
						.filter(filePath -> filePath.getFileName().toString().endsWith(".xml"))::iterator;
				for (final Path infile : infiles) {
					final Annotation annot = HatIO.readAnnotation(infile);
					for (final Segment seg : annot.getSegments().getSegment()) {
						final String segStr = createSegmentString(seg);
						final List<String> parseLeafNodes = parses.getUnchecked(segStr);
						System.out.println(Stream.of(segStr, parseLeafNodes.stream().collect(LIST_VALUE_JOINER))
								.collect(ROW_CELL_JOINER));
					}
				}
			}
		}
	}

	private static List<String> createParseTreeLeafNodeList(final String input, final Annotator annotator) {
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

	private static StanfordCoreNLP createPipeline() {
		return new StanfordCoreNLP(
				PropertiesUtils.asProperties("annotators", "tokenize,ssplit,pos,parse", "parse.binaryTrees", "true",
						"parse.model", "edu/stanford/nlp/models/lexparser/englishRNN.ser.gz", "pos.model",
						"edu/stanford/nlp/models/pos-tagger/english-bidirectional/english-bidirectional-distsim.tagger",
						"tokenize.language", "en"));
	}

	private static String createSegmentString(final Segment seg) {
		final List<Object> segsOrT = seg.getTranscription().getSegmentOrT();
		final List<String> tokenStrs = new ArrayList<>(segsOrT.size());
		for (final Object segOrT : segsOrT) {
			final T token = (T) segOrT;
			tokenStrs.add(token.getContent());
		}
		return tokenStrs.stream().collect(TOKEN_JOINER);
	}

	private RNNParseAnnotationSegmentWriter() {
	}

}
