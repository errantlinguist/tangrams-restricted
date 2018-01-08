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
package se.kth.speech.coin.tangrams.analysis.tokenization;

import java.util.Arrays;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;
import java.util.function.BiConsumer;

import javax.annotation.concurrent.NotThreadSafe;

import org.junit.Assert;
import org.junit.Test;

import edu.stanford.nlp.trees.Tree;
import edu.stanford.nlp.util.CoreMap;
import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.coin.tangrams.analysis.dialogues.transformation.EventDialogueTransformer;
import se.kth.speech.nlp.stanford.AnnotationCacheFactory;

/**
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since Dec 4, 2017
 *
 */
@NotThreadSafe
public final class TokenizationTest {

	private static final BiConsumer<CoreMap, List<Tree>> EXTRACTED_PHRASE_HANDLER = (sent, extractedPhrases) -> {
		// Do nothing
	};

	/**
	 * Parsing can take up huge amounts of memory, so it's single-threaded and
	 * thus the caches are created designed for single-threaded operation.
	 */
	private static final AnnotationCacheFactory ANNOTATION_CACHE_FACTORY = new AnnotationCacheFactory(1);

	@Test
	public void testStanfordBasicLemmatizingNoCleaning() {
		final Tokenization.Context ctx = new Tokenization.Context(EnumSet.noneOf(Cleaning.class), TokenType.LEMMA,
				EXTRACTED_PHRASE_HANDLER, ANNOTATION_CACHE_FACTORY);
		final EventDialogueTransformer diagTransformer = Tokenization.STANFORD_BASIC.apply(ctx);

		final List<String> tokSeq = Arrays.asList("eh", "so", "it", "has", "one", "two", "three", "four", "five", "eh",
				"sh-", "CLICK", "eh");
		final Utterance uttToTransform = new Utterance("segment23", "A", tokSeq, 54.7623f, 59.7255f);
		final EventDialogue diagToTransform = new EventDialogue(Collections.emptyList(), Arrays.asList(uttToTransform));
		final EventDialogue transformedDiag = diagTransformer.apply(diagToTransform);
		final Utterance transformedUtt = transformedDiag.getFirstUtterance().get();
		final List<String> transformedTokSeq = transformedUtt.getTokens();
		final List<String> expectedTokSeq = Arrays.asList("eh", "so", "it", "have", "one", "two", "three", "four",
				"five", "eh", "sh", "-", "click", "eh");
		Assert.assertEquals(expectedTokSeq, transformedTokSeq);
	}
	
	@Test
	public void testStanfordBasicLemmatizingAllCleaning() {
		final Tokenization.Context ctx = new Tokenization.Context(EnumSet.allOf(Cleaning.class), TokenType.LEMMA,
				EXTRACTED_PHRASE_HANDLER, ANNOTATION_CACHE_FACTORY);
		final EventDialogueTransformer diagTransformer = Tokenization.STANFORD_BASIC.apply(ctx);

		final List<String> tokSeq = Arrays.asList("eh", "so", "it", "has", "one", "two", "three", "four", "five", "eh",
				"sh-", "CLICK", "eh");
		final Utterance uttToTransform = new Utterance("segment23", "A", tokSeq, 54.7623f, 59.7255f);
		final EventDialogue diagToTransform = new EventDialogue(Collections.emptyList(), Arrays.asList(uttToTransform));
		final EventDialogue transformedDiag = diagTransformer.apply(diagToTransform);
		final Utterance transformedUtt = transformedDiag.getFirstUtterance().get();
		final List<String> transformedTokSeq = transformedUtt.getTokens();
		final List<String> expectedTokSeq = Arrays.asList("so", "it", "have", "one", "two", "three", "four",
				"five");
		Assert.assertEquals(expectedTokSeq, transformedTokSeq);
	}

}
