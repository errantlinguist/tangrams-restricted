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

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.annotation.concurrent.NotThreadSafe;

import org.junit.Assert;
import org.junit.experimental.theories.DataPoints;
import org.junit.experimental.theories.Theories;
import org.junit.experimental.theories.Theory;
import org.junit.runner.RunWith;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.cache.LoadingCache;

import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.ling.Label;
import edu.stanford.nlp.pipeline.Annotation;

/**
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since 30 May 2017
 *
 */
@RunWith(Theories.class)
@NotThreadSafe
public final class PhraseExtractingParsingTokenizerTest {
	
	private static final Logger LOGGER = LoggerFactory.getLogger(PhraseExtractingParsingTokenizerTest.class);


	@DataPoints
	public static final Collection<Entry<String, List<String>>> INPUT_EXPECTED_OUTPUTS = createInputExpectedOutputMap()
			.entrySet();

	private static final PhraseExtractingParsingTokenizer TEST_INST;

	static {
		final LoadingCache<String, Annotation> cache = new AnnotationCacheFactory(1)
				.apply(StanfordCoreNLPConfigurationVariant.TOKENIZING_PARSING);
		TEST_INST = new PhraseExtractingParsingTokenizer(cache, CoreLabel::word, subTree -> {
			final Label label = subTree.label();
			return label == null ? false : "NP".equals(label.value());
		}, (sent, extractedPhrases) -> {
			// Do nothing
		});
	}

	private static Map<String, List<String>> createInputExpectedOutputMap() {
		final Map<String, List<String>> result = new HashMap<>();
		result.put("the man bites the dog", Arrays.asList("the", "man", "the", "dog"));
		result.put("I think that's a good idea", Arrays.asList("I", "that", "a", "good", "idea"));
		result.put("it looks like a bird shape", Arrays.asList("it", "a", "bird", "shape"));
		result.put("a small chicken bird", Arrays.asList("a", "small", "chicken", "bird"));
		return result;
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.nlp.stanford.AbstractTokenizer#apply(java.lang.String)}.
	 */
	@Theory
	public void testApply(final Entry<String, List<String>> inputExpectedOutput) {
		final String input = inputExpectedOutput.getKey();
		final List<String> expectedResults = inputExpectedOutput.getValue();
		
		Runtime runtime = Runtime.getRuntime();
		final long bytesPerMegabyte = 1048576;
		LOGGER.info("Free memory: {} MB; Total memory: {} MB; Max memory: {} MB", runtime.freeMemory() / bytesPerMegabyte, runtime.totalMemory() / bytesPerMegabyte, runtime.maxMemory() / bytesPerMegabyte); 
		final List<String> actualResults = TEST_INST.apply(input);
		LOGGER.info("Parsed \"{}\" to get {}, expecting {}.", input, actualResults, expectedResults);
		Assert.assertEquals(String.format("For input \"%s\" expected output %s but got %s instead.", input,
				expectedResults, actualResults), expectedResults, actualResults);
	}

}
