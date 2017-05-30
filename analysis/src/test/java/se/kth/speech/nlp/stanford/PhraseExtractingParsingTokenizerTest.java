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

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import org.junit.Assert;
import org.junit.experimental.theories.DataPoints;
import org.junit.experimental.theories.Theories;
import org.junit.experimental.theories.Theory;
import org.junit.runner.RunWith;

import edu.stanford.nlp.ling.Label;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 30 May 2017
 *
 */
@RunWith(Theories.class)
public final class PhraseExtractingParsingTokenizerTest {

	@DataPoints
	public static final Collection<Entry<String, List<String>>> INPUT_EXPECTED_OUTPUTS = createInputExpectedOutputMap()
			.entrySet();

	private static final PhraseExtractingParsingTokenizer TEST_INST;

	static {
		final ExecutorService executor = Executors.newSingleThreadExecutor();
		TEST_INST = new PhraseExtractingParsingTokenizer(
				StanfordCoreNLPConfigurationVariant.TOKENIZING_PARSING.apply(executor), subTree -> {
					final Label label = subTree.label();
					return label == null ? false : "NP".equals(label.value());
				});
		executor.shutdown();
	}

	private static Map<String, List<String>> createInputExpectedOutputMap() {
		final Map<String, List<String>> result = new HashMap<>();
		result.put("the man bites the dog", Arrays.asList("the", "man", "the", "dog"));
		result.put("I think that's a good idea", Arrays.asList("I", "that", "a", "good", "idea"));
		result.put("it's looks like a bird shape", Arrays.asList("it", "a", "bird", "shape"));
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
		final List<String> actualResults = TEST_INST.apply(input);
		Assert.assertEquals(expectedResults, actualResults);
	}

}
