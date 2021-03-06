/*
 *  This file is part of Tangrams-restricted.
 *
 *  Tangrams-restricted is free software: you can redistribute it and/or modify
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
import java.lang.ref.SoftReference;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Sets;

/**
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since May 28, 2017
 *
 */
public final class EnglishLocationalPrepositions {

	private static final String COMMENT_LINE_PREFIX = "#";

	private static volatile SoftReference<Set<List<String>>> list = new SoftReference<>(null);

	private static final Logger LOGGER = LoggerFactory.getLogger(EnglishLocationalPrepositions.class);

	private static final Pattern WHITESPACE_PATTERN = Pattern.compile("\\s+");

	public static Set<List<String>> get() {
		Set<List<String>> result = list.get();
		if (result == null) {
			synchronized (EnglishLocationalPrepositions.class) {
				result = list.get();
				if (result == null) {
					result = loadSet();
					list = new SoftReference<>(result);
				}
			}
		}
		return result;
	}

	private static List<List<String>> loadList() {
		final List<List<String>> result = new ArrayList<>();
		try (final BufferedReader reader = new BufferedReader(new InputStreamReader(
				EnglishLocationalPrepositions.class.getResourceAsStream("english-locational-prepositions.txt")))) {
			for (String line = reader.readLine(); line != null; line = reader.readLine()) {
				final String trimmedLine = line.trim();
				if (!trimmedLine.startsWith(COMMENT_LINE_PREFIX)) {
					final String[] tokens = WHITESPACE_PATTERN.split(trimmedLine);
					result.add(Arrays.asList(tokens));
				}
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
