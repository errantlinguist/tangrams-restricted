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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UncheckedIOException;
import java.lang.ref.Reference;
import java.lang.ref.SoftReference;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.function.Supplier;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Sets;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 24 May 2017
 * @see <a href="http://snowballstem.org/algorithms/english/stop.txt">Original
 *      stopword list location</a>
 * @see <a href="http://snowballstem.org/">Snowball stemmer</a>
 *
 */
public final class SnowballPorter2EnglishStopwords {

	public enum Variant implements Supplier<Set<String>> {
		CANONICAL("stop.txt"), FILLERS("fillers.txt");

		protected final String resLoc;

		private Variant(final String resLoc) {
			this.resLoc = resLoc;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.util.function.Supplier#get()
		 */
		@Override
		public Set<String> get() {
			return fetchStopwordSet(getResLoc());
		}

		/**
		 * @return the resLoc
		 */
		protected String getResLoc() {
			return resLoc;
		}
	}

	private static final char COMMENT_DELIM = '|';

	private static final ConcurrentMap<String, Reference<Set<String>>> INSTANCES = new ConcurrentHashMap<>(
			Variant.values().length);

	private static final Logger LOGGER = LoggerFactory.getLogger(SnowballPorter2EnglishStopwords.class);

	/**
	 * <strong>NOTE:</strong> All words in the stoplist are lowercase (or at
	 * least they should be).
	 *
	 * @return A new {@List} of (lowercase) strings representing the stoplist.
	 * @throws IOException
	 *             If an error occurs while reading the stoplist file.
	 */
	public static List<String> loadStopwordList(final String[] resLocs) throws IOException {
		final List<String> result = new ArrayList<>(16 * resLocs.length);
		for (final String resLoc : resLocs) {
			load(resLoc, result);
		}
		LOGGER.debug("Read stopword list of size {} from {}.", result.size(), Arrays.toString(resLocs));
		return result;
	}

	/**
	 * <strong>NOTE:</strong> All words in the stoplist are lowercase (or at
	 * least they should be).
	 *
	 * @return A new {@Set} of (lowercase) strings representing the stoplist.
	 */
	public static Set<String> loadStopwordSet(final Collection<Variant> variantsToUnify) {
		final String[] resLocs = variantsToUnify.stream().map(Variant::getResLoc).toArray(String[]::new);
		try {
			final List<String> words = loadStopwordList(resLocs);
			final Set<String> result = Sets.newHashSetWithExpectedSize(words.size());
			result.addAll(words);
			LOGGER.info("Read stopword set of size {} from {}.", result.size(), Arrays.asList(resLocs));
			return result;
		} catch (final IOException e) {
			throw new UncheckedIOException(e);
		}
	}

	public static Stream<String> parse(final String line) {
		final int commentStartIdx = line.indexOf(COMMENT_DELIM);
		String content;
		if (commentStartIdx < 1) {
			content = line;
		} else {
			content = line.substring(0, commentStartIdx);
		}
		content = content.trim();
		return content.isEmpty() ? Stream.empty() : Stream.of(content);
	}

	/**
	 * <strong>NOTE:</strong> All words in the stoplist are lowercase (or at
	 * least they should be).
	 *
	 * @return A new {@Set} of (lowercase) strings representing the stoplist.
	 * @throws UncheckedIOException
	 *             If an error occurs while reading the stoplist file.
	 */
	private static Set<String> fetchStopwordSet(final String resLoc) {
		final Reference<Set<String>> ref = INSTANCES.compute(resLoc, (key, oldValue) -> {
			final Reference<Set<String>> newValue;
			try {
				if (oldValue == null) {
					// No instance has yet been created; Create one
					newValue = new SoftReference<>(loadStopwordSet(resLoc));
				} else if (oldValue.get() == null) {
					// The old instance has already been deleted; Replace it
					// with a new reference to a new instance
					newValue = new SoftReference<>(loadStopwordSet(resLoc));
				} else {
					// The existing instance has not yet been deleted;
					// Re-use it
					newValue = oldValue;
				}
			} catch (final IOException e) {
				throw new UncheckedIOException(e);
			}
			return newValue;
		});
		return ref.get();
	}

	private static void load(final String resLoc, final Collection<? super String> coll) throws IOException {
		try (final BufferedReader reader = new BufferedReader(
				new InputStreamReader(SnowballPorter2EnglishStopwords.class.getResourceAsStream(resLoc)))) {
			reader.lines().flatMap(SnowballPorter2EnglishStopwords::parse).forEach(coll::add);
		}
	}

	/**
	 * <strong>NOTE:</strong> All words in the stoplist are lowercase (or at
	 * least they should be).
	 *
	 * @return A new {@Set} of (lowercase) strings representing the stoplist.
	 * @throws IOException
	 *             If an error occurs while reading the stoplist file.
	 */
	private static Set<String> loadStopwordSet(final String resLoc) throws IOException {
		final Set<String> result = new HashSet<>();
		load(resLoc, result);
		LOGGER.info("Read stopword set of size {} from \"{}\".", result.size(), resLoc);
		return result;
	}

	private SnowballPorter2EnglishStopwords() {
	}

}
