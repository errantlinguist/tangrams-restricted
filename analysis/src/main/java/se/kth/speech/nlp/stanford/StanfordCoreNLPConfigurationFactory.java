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
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executor;
import java.util.concurrent.Future;
import java.util.function.BiFunction;
import java.util.function.Supplier;
import java.util.stream.Collector;
import java.util.stream.Collectors;

import com.google.common.collect.Sets;

import edu.stanford.nlp.pipeline.Annotator;
import edu.stanford.nlp.pipeline.StanfordCoreNLP;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 29 May 2017
 *
 */
public final class StanfordCoreNLPConfigurationFactory
		implements BiFunction<Set<StanfordCoreNLPConfigurationFactory.Option>, Executor, Supplier<StanfordCoreNLP>> {

	public enum Option {
		LEMMATIZE, PARSE, SENTIMENT;
	}

	private static final ConcurrentMap<Set<Option>, Reference<Supplier<StanfordCoreNLP>>> INSTANCES = new ConcurrentHashMap<>(
			Option.values().length * Option.values().length);

	private static final Collector<CharSequence, ?, String> OPTION_MULTIVALUE_DELIMITER = Collectors.joining(",");

	private static Properties createDefaultProps() {
		final Properties result = new Properties();
		// https://stanfordnlp.github.io/CoreNLP/api.html
		// https://stanfordnlp.github.io/CoreNLP/parse.html
		// result.setProperty("parse.model",
		// "edu/stanford/nlp/models/lexparser/englishPCFG.ser.gz");
		// result.setProperty("parse.model",
		// "edu/stanford/nlp/models/lexparser/englishPCFG.caseless.ser.gz");
		// result.setProperty("parse.model",
		// "edu/stanford/nlp/models/lexparser/englishFactored.ser.gz");
		result.setProperty("parse.model", "edu/stanford/nlp/models/srparser/englishSR.ser.gz");
		// https://stanfordnlp.github.io/CoreNLP/pos.html
		// result.setProperty("pos.model",
		// "edu/stanford/nlp/models/pos-tagger/english-left3words/english-left3words-distsim.tagger");
		// result.setProperty("pos.model",
		// "edu/stanford/nlp/models/pos-tagger/english-caseless-left3words-distsim.tagger");
		result.setProperty("pos.model",
				"edu/stanford/nlp/models/pos-tagger/english-bidirectional/english-bidirectional-distsim.tagger");
		// https://stanfordnlp.github.io/CoreNLP/sentiment.html
		result.setProperty("sentiment.model", "edu/stanford/nlp/models/sentiment/sentiment.binary.ser.gz");
		// https://stanfordnlp.github.io/CoreNLP/ssplit.html
		result.setProperty("ssplit.isOneSentence", "true");
		// https://stanfordnlp.github.io/CoreNLP/tokenize.html
		result.setProperty("tokenize.language", "en");
		return result;
	}

	private static <T> Supplier<T> wrapFuture(final Future<? extends T> fut) {
		return () -> {
			try {
				return fut.get();
			} catch (InterruptedException | ExecutionException e) {
				throw new RuntimeException(e);
			}
		};
	}

	@Override
	public Supplier<StanfordCoreNLP> apply(final Set<Option> opts, final Executor executor) {
		final Reference<Supplier<StanfordCoreNLP>> ref = INSTANCES.compute(opts, (key, oldValue) -> {
			final Reference<Supplier<StanfordCoreNLP>> newValue;
			if (oldValue == null) {
				// No instance has yet been created; Create one
				final CompletableFuture<StanfordCoreNLP> futureInst = CompletableFuture
						.supplyAsync(() -> new StanfordCoreNLP(createProps(opts)), executor);
				newValue = new SoftReference<>(wrapFuture(futureInst));
			} else if (oldValue.get() == null) {
				// The old instance has already been deleted; Replace it with a
				// new reference to a new instance
				final CompletableFuture<StanfordCoreNLP> futureInst = CompletableFuture
						.supplyAsync(() -> new StanfordCoreNLP(createProps(opts)), executor);
				newValue = new SoftReference<>(wrapFuture(futureInst));
			} else {
				// The existing instance has not yet been deleted; Re-use it
				newValue = oldValue;
			}
			return newValue;
		});
		return ref.get();
	}

	/**
	 * @param opts
	 * @return
	 */
	private Properties createProps(final Set<Option> opts) {
		final Properties result = createDefaultProps();
		final Set<String> annotatorNames = Sets.newLinkedHashSetWithExpectedSize(6);
		annotatorNames.add(Annotator.STANFORD_TOKENIZE);
		annotatorNames.add(Annotator.STANFORD_SSPLIT);
		if (opts.contains(Option.LEMMATIZE)) {
			annotatorNames.add(Annotator.STANFORD_POS);
			annotatorNames.add(Annotator.STANFORD_LEMMA);
		}
		if (opts.contains(Option.PARSE)) {
			annotatorNames.add(Annotator.STANFORD_POS);
			annotatorNames.add(Annotator.STANFORD_PARSE);
		}
		if (opts.contains(Option.SENTIMENT)) {
			annotatorNames.add(Annotator.STANFORD_POS);
			annotatorNames.add(Annotator.STANFORD_PARSE);
			annotatorNames.add(Annotator.STANFORD_SENTIMENT);
		}
		result.setProperty("annotators", annotatorNames.stream().collect(OPTION_MULTIVALUE_DELIMITER));
		return result;
	}

}
