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
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import it.unimi.dsi.fastutil.objects.ObjectOpenHashSet;
import it.unimi.dsi.fastutil.objects.ObjectSet;
import it.unimi.dsi.fastutil.objects.ObjectSets;

/**
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since Nov 18, 2017
 *
 */
public final class MetaLanguage {

	private static final String COMMENT_LINE_PREFIX = "#";

	private static final ObjectSet<String> META_LANGUAGE_TOKENS;

	private static final Pattern TOKEN_DELIMITER_PATTERN = Pattern.compile("\\s*,\\\\s*");

	private static final Logger LOGGER = LoggerFactory.getLogger(MetaLanguage.class);

	static {
		try {
			META_LANGUAGE_TOKENS = ObjectSets.unmodifiable(readMetaLanguageTokenSet());
			LOGGER.info("Read {} metalanguage token(s).", META_LANGUAGE_TOKENS.size());
		} catch (final IOException e) {
			throw new UncheckedIOException(e);
		}
	}

	/**
	 * @return the metaLanguageTokens
	 */
	public static ObjectSet<String> getMetaLanguageTokens() {
		return META_LANGUAGE_TOKENS;
	}

	public static boolean isMetaLanguageToken(final String token) {
		return META_LANGUAGE_TOKENS.contains(token);
	}

	private static ObjectSet<String> readMetaLanguageTokenSet() throws IOException {
		final ObjectOpenHashSet<String> result = new ObjectOpenHashSet<>();
		try (BufferedReader reader = new BufferedReader(
				new InputStreamReader(MetaLanguage.class.getResourceAsStream("metalanguage-tokens.txt")))) {
			for (String line = reader.readLine(); line != null; line = reader.readLine()) {
				final String trimmedLine = line.trim();
				if (!trimmedLine.startsWith(COMMENT_LINE_PREFIX)) {
					TOKEN_DELIMITER_PATTERN.splitAsStream(trimmedLine).forEach(result::add);
				}
			}
		}
		result.trim();
		return result;
	}

	private MetaLanguage() {
	}

}
