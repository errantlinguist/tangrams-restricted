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
			final Properties result = createDefaultProps();
			// https://stanfordnlp.github.io/CoreNLP/annotators.html
			result.setProperty("annotators", "tokenize,ssplit");
			return result;
		}
	},
	TOKENIZING_LEMMATIZING {

		@Override
		protected Properties createProps() {
			final Properties result = createDefaultProps();
			// https://stanfordnlp.github.io/CoreNLP/annotators.html
			result.setProperty("annotators", "tokenize,ssplit,pos,lemma");
			return result;
		}
	},
	TOKENIZING_LEMMATIZING_PARSING {

		@Override
		protected Properties createProps() {
			final Properties result = createDefaultProps();
			// https://stanfordnlp.github.io/CoreNLP/annotators.html
			result.setProperty("annotators", "tokenize,ssplit,pos,lemma,parse");
			return result;
		}
	};

	private static final ConcurrentMap<StanfordCoreNLPConfigurationVariant, Reference<StanfordCoreNLP>> INSTANCES = new ConcurrentHashMap<>(
			StanfordCoreNLPConfigurationVariant.values().length);

	private static Properties createDefaultProps() {
		final Properties result = new Properties();
		// https://stanfordnlp.github.io/CoreNLP/api.html
		// https://stanfordnlp.github.io/CoreNLP/parse.html
		// result.setProperty("parse.model",
		// "edu/stanford/nlp/models/lexparser/englishPCFG.ser.gz");
//		 result.setProperty("parse.model",
//		 "edu/stanford/nlp/models/lexparser/englishPCFG.caseless.ser.gz");
//		result.setProperty("parse.model", "edu/stanford/nlp/models/lexparser/englishFactored.ser.gz");
//		result.setProperty("parse.model", "edu/stanford/nlp/models/lexparser/englishRNN.ser.gz");
		result.setProperty("parse.model", "edu/stanford/nlp/models/srparser/englishSR.ser.gz");
		// https://stanfordnlp.github.io/CoreNLP/pos.html
		result.setProperty("pos.model",
				"edu/stanford/nlp/models/pos-tagger/english-left3words/english-left3words-distsim.tagger");
		// result.setProperty("pos.model",
		// "edu/stanford/nlp/models/pos-tagger/english-caseless-left3words-distsim.tagger");
		// https://stanfordnlp.github.io/CoreNLP/ssplit.html
		result.setProperty("ssplit.isOneSentence", "true");
		// https://stanfordnlp.github.io/CoreNLP/tokenize.html
		result.setProperty("tokenize.language", "en");
		return result;
	}

	@Override
	public StanfordCoreNLP get() {
		final Reference<StanfordCoreNLP> ref = INSTANCES.compute(this, (key, oldValue) -> {
			final Reference<StanfordCoreNLP> newValue;
			if (oldValue == null) {
				// No instance has yet been created; Create one
				newValue = new SoftReference<>(new StanfordCoreNLP(createProps()));
			} else if (oldValue.get() == null) {
				// The old instance has already been deleted; Replace it with a
				// new reference to a new instance
				newValue = new SoftReference<>(new StanfordCoreNLP(createProps()));
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
