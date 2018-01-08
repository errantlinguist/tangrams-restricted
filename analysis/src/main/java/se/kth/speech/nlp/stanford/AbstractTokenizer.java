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

import java.util.List;
import java.util.function.Function;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.cache.LoadingCache;

import edu.stanford.nlp.pipeline.Annotation;

/**
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since Apr 14, 2017
 *
 */
abstract class AbstractTokenizer implements Function<String, List<String>> {

	private static final Logger LOGGER = LoggerFactory.getLogger(AbstractTokenizer.class);

	private final LoadingCache<String, Annotation> cache;

	AbstractTokenizer(final LoadingCache<String, Annotation> cache) {
		this.cache = cache;
	}

	@Override
	public final List<String> apply(final String input) {
		final Annotation annot = cache.getUnchecked(input);
		final List<String> result = tokenize(annot);
		LOGGER.debug("Tokenized \"{}\" to {}.", input, result);
		return result;
	}

	protected abstract List<String> tokenize(Annotation annot);

}
