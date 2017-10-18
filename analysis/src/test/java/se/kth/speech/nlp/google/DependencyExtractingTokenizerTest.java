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
package se.kth.speech.nlp.google;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collector;
import java.util.stream.Collectors;

import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.cloud.language.v1.LanguageServiceClient;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 18 Oct 2017
 *
 */
@RunWith(Parameterized.class)
public final class DependencyExtractingTokenizerTest {

	private static LanguageServiceClient client;

	private static final Logger LOGGER = LoggerFactory.getLogger(DependencyExtractingTokenizerTest.class);

	private static final Collector<CharSequence, ?, String> TOKEN_JOINER = Collectors.joining(" ");

	@Parameters
	public static List<Object[]> data() {
		return Arrays
				.asList(new Object[][] { { "the blue piece that I saw before", Arrays.asList("the", "blue", "piece", "I") },
						{ "so the piece that you picked last time same but it's bigger and blue",
								Arrays.asList("the", "piece", "you", "last", "time", "bigger", "blue") },
						{ "it's in the upper right hand corner",
								Arrays.asList("it", "the", "upper", "right", "hand", "corner") },

		});
	}

	@BeforeClass
	public static void initService() throws IOException {
		client = LanguageServiceClient.create();
	}

	@AfterClass
	public static void shutdownService() throws Exception {
		client.close();
	}

	private final List<String> expected;

	private final String input;

	public DependencyExtractingTokenizerTest(final String input, final List<String> expected) {
		this.input = input;
		this.expected = expected;
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.nlp.google.DependencyExtractingTokenizer#apply(java.lang.String)}.
	 */
	@Test
	public void testApply() {
		final DependencyExtractingTokenizer tokenizer = new DependencyExtractingTokenizer(client);
		LOGGER.info("Parsing test input \"{}\".", input);
		final List<String> actualResult = tokenizer.apply(input);
		final String actualResultStr = actualResult.stream().collect(TOKEN_JOINER);
		LOGGER.info("Tokenization results: \"{}\"", actualResultStr);
		Assert.assertEquals(
				String.format("Expected tokenization result of \"%s\" was \"%s\" but actual result was \"%s\".", input,
						expected.stream().collect(TOKEN_JOINER), actualResultStr),
				expected, actualResult);
	}

}
