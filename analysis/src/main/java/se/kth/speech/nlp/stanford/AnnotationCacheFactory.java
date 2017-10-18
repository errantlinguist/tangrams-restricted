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
package se.kth.speech.nlp.stanford;

import java.lang.ref.Reference;
import java.lang.ref.SoftReference;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.function.Function;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;

import edu.stanford.nlp.pipeline.Annotation;
import edu.stanford.nlp.pipeline.Annotator;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Apr 14, 2017
 *
 */
public final class AnnotationCacheFactory
		implements Function<StanfordCoreNLPConfigurationVariant, LoadingCache<String, Annotation>> {

	private static final ConcurrentMap<StanfordCoreNLPConfigurationVariant, Reference<LoadingCache<String, Annotation>>> CONFIG_CACHES = new ConcurrentHashMap<>(
			StanfordCoreNLPConfigurationVariant.values().length);

	private static final int ESTIMATED_MAX_UNIQUE_INPUT_COUNT = 2000;

	private static Annotation annotate(final String input, final StanfordCoreNLPConfigurationVariant config) {
		final Annotator annotator = config.get();
		final Annotation result = new Annotation(input);
		annotator.annotate(result);
		result.compact();
		return result;
	}

	private final int concurrencyLevel;

	public AnnotationCacheFactory(final int concurrencyLevel) {
		this.concurrencyLevel = concurrencyLevel;
	}

	@Override
	public LoadingCache<String, Annotation> apply(final StanfordCoreNLPConfigurationVariant annotConfig) {
		final Reference<LoadingCache<String, Annotation>> ref = CONFIG_CACHES.compute(annotConfig, (key, oldValue) -> {
			final Reference<LoadingCache<String, Annotation>> newValue;
			if (oldValue == null) {
				// No instance has yet been created; Create one
				newValue = new SoftReference<>(createCache(key));
			} else if (oldValue.get() == null) {
				// The old instance has already been deleted; Replace it
				// with a new reference to a new instance
				newValue = new SoftReference<>(createCache(key));
			} else {
				// The existing instance has not yet been deleted;
				// Re-use it
				newValue = oldValue;
			}
			return newValue;
		});
		return ref.get();
	}

	private LoadingCache<String, Annotation> createCache(final StanfordCoreNLPConfigurationVariant annotConfig) {
		return CacheBuilder.newBuilder().softValues().initialCapacity(ESTIMATED_MAX_UNIQUE_INPUT_COUNT)
				.maximumSize(ESTIMATED_MAX_UNIQUE_INPUT_COUNT).concurrencyLevel(concurrencyLevel)
				.build(CacheLoader.from(str -> annotate(str, annotConfig)));
	}

}
