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
package se.kth.speech.nlp;

import java.util.Properties;
import java.util.function.Supplier;

import edu.stanford.nlp.pipeline.StanfordCoreNLP;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 29 May 2017
 *
 */
public enum StanfordCoreNLPFactory implements Supplier<StanfordCoreNLP> {
	TOKENIZING_LEMMATIZING_PARSING {

		@Override
		protected Properties createProps() {
			final Properties result = new Properties();
			// https://stanfordnlp.github.io/CoreNLP/api.html
			// https://stanfordnlp.github.io/CoreNLP/annotators.html
			result.setProperty("annotators", "tokenize,ssplit,pos,lemma,parse");
			// https://stanfordnlp.github.io/CoreNLP/parse.html
			result.setProperty("parse.model", "edu/stanford/nlp/models/lexparser/englishPCFG.caseless.ser.gz");
			// https://stanfordnlp.github.io/CoreNLP/pos.html
			result.setProperty("pos.model",
					"edu/stanford/nlp/models/pos-tagger/english-caseless-left3words-distsim.tagger");
			// https://stanfordnlp.github.io/CoreNLP/ssplit.html
			result.setProperty("ssplit.isOneSentence", "true");
			// https://stanfordnlp.github.io/CoreNLP/tokenize.html
			result.setProperty("tokenize.language", "en");
			return result;
		}

	};

	protected StanfordCoreNLP instance;

	@Override
	public StanfordCoreNLP get() {
		if (instance == null) {
			synchronized (this) {
				if (instance == null) {
					final Properties props = createProps();
					instance = new StanfordCoreNLP(props);
				}
			}
		}
		return instance;
	}

	protected abstract Properties createProps();
}
