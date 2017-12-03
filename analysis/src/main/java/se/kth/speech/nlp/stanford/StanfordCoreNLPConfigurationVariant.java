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

import java.lang.ref.Reference;
import java.lang.ref.SoftReference;
import java.util.Properties;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.function.Supplier;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import edu.stanford.nlp.pipeline.Annotator;
import edu.stanford.nlp.pipeline.StanfordCoreNLP;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 29 May 2017
 *
 */
public enum StanfordCoreNLPConfigurationVariant implements Supplier<StanfordCoreNLP> {
	TOKENIZING {

		@Override
		protected Properties createProps() {
			final Properties result = new Properties(DEFAULT_PROPS);
			// https://stanfordnlp.github.io/CoreNLP/annotators.html
			final Stream<String> annotatorNames = Stream.of(Annotator.STANFORD_TOKENIZE, Annotator.STANFORD_SSPLIT);
			result.setProperty("annotators", annotatorNames.collect(OPTION_MULTIVALUE_DELIMITER));
			return result;
		}
	},
	TOKENIZING_LEMMATIZING {

		@Override
		protected Properties createProps() {
			final Properties result = new Properties(DEFAULT_PROPS);
			// https://stanfordnlp.github.io/CoreNLP/annotators.html
			final Stream<String> annotatorNames = Stream.of(Annotator.STANFORD_TOKENIZE, Annotator.STANFORD_SSPLIT,
					Annotator.STANFORD_POS, Annotator.STANFORD_LEMMA);
			result.setProperty("annotators", annotatorNames.collect(OPTION_MULTIVALUE_DELIMITER));
			return result;
		}
	},
	TOKENIZING_LEMMATIZING_PARSING {

		@Override
		protected Properties createProps() {
			final Properties result = new Properties(DEFAULT_PROPS);
			// https://stanfordnlp.github.io/CoreNLP/annotators.html
			final Stream<String> annotatorNames = Stream.of(Annotator.STANFORD_TOKENIZE, Annotator.STANFORD_SSPLIT,
					Annotator.STANFORD_POS, Annotator.STANFORD_LEMMA, Annotator.STANFORD_PARSE);
			result.setProperty("annotators", annotatorNames.collect(OPTION_MULTIVALUE_DELIMITER));
			return result;
		}
	},
	TOKENIZING_PARSING {

		@Override
		protected Properties createProps() {
			final Properties result = new Properties(DEFAULT_PROPS);
			// https://stanfordnlp.github.io/CoreNLP/annotators.html
			final Stream<String> annotatorNames = Stream.of(Annotator.STANFORD_TOKENIZE, Annotator.STANFORD_SSPLIT,
					Annotator.STANFORD_POS, Annotator.STANFORD_PARSE);
			result.setProperty("annotators", annotatorNames.collect(OPTION_MULTIVALUE_DELIMITER));
			return result;
		}
	},
	TOKENIZING_PARSING_SENTIMENT {

		@Override
		protected Properties createProps() {
			final Properties result = new Properties(DEFAULT_PROPS);
			// https://stanfordnlp.github.io/CoreNLP/annotators.html
			final Stream<String> annotatorNames = Stream.of(Annotator.STANFORD_TOKENIZE, Annotator.STANFORD_SSPLIT,
					Annotator.STANFORD_POS, Annotator.STANFORD_PARSE, Annotator.STANFORD_SENTIMENT);
			result.setProperty("annotators", annotatorNames.collect(OPTION_MULTIVALUE_DELIMITER));
			return result;
		}
	};

	private static final Properties DEFAULT_PROPS = createDefaultPipelineProperties();

	private static final ConcurrentMap<StanfordCoreNLPConfigurationVariant, Reference<StanfordCoreNLP>> INSTANCES = new ConcurrentHashMap<>(
			StanfordCoreNLPConfigurationVariant.values().length);

	private static final Collector<CharSequence, ?, String> OPTION_MULTIVALUE_DELIMITER = Collectors.joining(",");

	// private static Properties loadDefaultPipelineProperties() {
	// final Properties result = new Properties();
	// try (InputStream is = StanfordCoreNLPConfigurationVariant.class
	// .getResourceAsStream("corenlp-defaults.properties")) {
	// result.load(is);
	// } catch (final IOException e) {
	// throw new UncheckedIOException(e);
	// }
	// return result;
	// }

	/**
	 * <strong>NOTE:</strong> This method is used because of problems reading a
	 * properties file for doing so when running unit tests with Maven.
	 *
	 * @return A new {@link Properties} instance creating default CoreNLP
	 *         configuration properties.
	 */
	private static Properties createDefaultPipelineProperties() {
		// https://stanfordnlp.github.io/CoreNLP/api.html
		final Properties result = new Properties();
		// https://stanfordnlp.github.io/CoreNLP/parse.html
		// Sentiment analyzer requires parser to output binary trees
		result.put("parse.binaryTrees", "true");
		// This is the greedy SR model used in Todd Shore and Gabriel Skantze
		// (2017) "Enhancing reference resolution in dialogue using participant
		// feedback". First International Workshop on Grounding Language
		// Understanding (GLU 2017)
		// parse.model = edu/stanford/nlp/models/srparser/englishSR.ser.gz
		// This SR model is supposedly much more accurate than the greedy SR
		// version while still being very fast
		result.put("parse.model", "edu/stanford/nlp/models/srparser/englishSR.beam.ser.gz");
		// Supposedly slightly more accurate than SR beam-search model but is
		// very slow and has huge memory footprint
		// parse.model = edu/stanford/nlp/models/lexparser/englishRNN.ser.gz

		// https://stanfordnlp.github.io/CoreNLP/pos.html
		// pos.model =
		// edu/stanford/nlp/models/pos-tagger/english-left3words/english-left3words-distsim.tagger
		// pos.model =
		// edu/stanford/nlp/models/pos-tagger/english-caseless-left3words-distsim.tagger
		result.put("pos.model",
				"edu/stanford/nlp/models/pos-tagger/english-bidirectional/english-bidirectional-distsim.tagger");

		// https://stanfordnlp.github.io/CoreNLP/ssplit.html
		// ssplit.isOneSentence = true

		// https://stanfordnlp.github.io/CoreNLP/tokenize.html
		result.put("tokenize.language", "en");
		return result;
	}

	@Override
	public StanfordCoreNLP get() {
		final Reference<StanfordCoreNLP> ref = INSTANCES.compute(this, (key, oldValue) -> {
			final Reference<StanfordCoreNLP> newValue;
			if (oldValue == null) {
				// No instance has yet been created; Create one
				newValue = new SoftReference<>(new StanfordCoreNLP(key.createProps()));
			} else if (oldValue.get() == null) {
				// The old instance has already been deleted; Replace it with a
				// new reference to a new instance
				newValue = new SoftReference<>(new StanfordCoreNLP(key.createProps()));
			} else {
				// The existing instance has not yet been deleted; Re-use it
				newValue = oldValue;
			}
			return newValue;
		});
		return ref.get();
	}

	protected abstract Properties createProps();
}
