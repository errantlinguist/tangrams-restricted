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
package se.kth.speech.nlp;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UncheckedIOException;
import java.lang.ref.Reference;
import java.lang.ref.SoftReference;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Sets;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since May 28, 2017
 *
 */
public final class EnglishLocationalPrepositions {

	private static final ConcurrentMap<Class<EnglishLocationalPrepositions>, Reference<Set<List<String>>>> INSTANCES = new ConcurrentHashMap<>(
			1);

	private static final Logger LOGGER = LoggerFactory.getLogger(EnglishLocationalPrepositions.class);

	private static final Pattern WHITESPACE_PATTERN = Pattern.compile("\\s+");

	public static Set<List<String>> get() {
		final Reference<Set<List<String>>> ref = INSTANCES.compute(EnglishLocationalPrepositions.class,
				(key, oldValue) -> {
					final Reference<Set<List<String>>> newValue;
					if (oldValue == null) {
						// No instance has yet been created; Create one
						newValue = new SoftReference<>(loadSet());
					} else if (oldValue.get() == null) {
						// The old instance has already been deleted; Replace it
						// with a
						// new reference to a new instance
						newValue = new SoftReference<>(loadSet());
					} else {
						// The existing instance has not yet been deleted;
						// Re-use it
						newValue = oldValue;
					}
					return newValue;
				});
		return ref.get();
	}

	private static List<List<String>> loadList() {
		final List<List<String>> result = new ArrayList<>();
		try (final BufferedReader reader = new BufferedReader(new InputStreamReader(
				EnglishLocationalPrepositions.class.getResourceAsStream("english-locational-prepositions.txt")))) {
			for (String line = reader.readLine(); line != null; line = reader.readLine()) {
				final String[] tokens = WHITESPACE_PATTERN.split(line);
				result.add(Arrays.asList(tokens));
			}
		} catch (final IOException e) {
			throw new UncheckedIOException(e);
		}
		return result;
	}

	private static Set<List<String>> loadSet() {
		LOGGER.info("Loading English locational preposition set.");
		final List<List<String>> list = loadList();
		final Set<List<String>> result = Sets.newHashSetWithExpectedSize(list.size());
		result.addAll(list);
		return result;
	}

	private EnglishLocationalPrepositions() {

	}

}
