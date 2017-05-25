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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags;

import java.util.Arrays;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Stream;

import org.apache.lucene.analysis.Analyzer;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;

import se.kth.speech.coin.tangrams.analysis.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.Utterance;
import se.kth.speech.nlp.lucene.StringTokenizer;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 24 May 2017
 *
 */
public final class LuceneAnalyzingEventDialogueTransformer implements EventDialogueTransformer {

	private static Function<String, Stream<String>> createTokenTransformer(final Analyzer analyzer) {
		final StringTokenizer ssTokenizer = new StringTokenizer(analyzer);
		return tokens -> ssTokenizer.apply(null, tokens);
	}

	private final Function<? super String, Stream<String>> tokenTransformer;

	private final LoadingCache<Utterance, Stream<Utterance>> transformedUtts = CacheBuilder.newBuilder().weakKeys()
			.softValues().build(CacheLoader.from(this::transformUtt));

	public LuceneAnalyzingEventDialogueTransformer(final Analyzer analyzer) {
		this(createTokenTransformer(analyzer));
	}

	public LuceneAnalyzingEventDialogueTransformer(final Function<? super String, Stream<String>> tokenTransformer) {
		this.tokenTransformer = tokenTransformer;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Function#apply(java.lang.Object)
	 */
	@Override
	public EventDialogue apply(final EventDialogue diag) {
		final List<Utterance> newUtts = Arrays
				.asList(diag.getUtts().stream().flatMap(transformedUtts::getUnchecked).toArray(Utterance[]::new));
		return new EventDialogue(diag.getLastEvent(), newUtts);
	}

	private Stream<Utterance> transformUtt(final Utterance utt) {
		final Stream<String> newTokens = tokenTransformer.apply(utt.getTokenStr()).map(String::trim)
				.filter(str -> !str.isEmpty());
		final String[] newTokenArr = newTokens.toArray(String[]::new);
		return newTokenArr.length < 1 ? Stream.empty()
				: Stream.of(new Utterance(utt.getSegmentId(), utt.getSpeakerId(), Arrays.asList(newTokenArr),
						utt.getStartTime(), utt.getEndTime()));
	}

}
