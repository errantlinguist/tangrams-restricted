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
package se.kth.speech.coin.tangrams.analysis;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.UncheckedIOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.FileTime;
import java.text.Collator;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.NavigableSet;
import java.util.Optional;
import java.util.Properties;
import java.util.TreeSet;
import java.util.function.Function;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.github.errantlinguist.ClassProperties;

import se.kth.speech.io.FileDataAge;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Apr 13, 2017
 *
 */
public final class VocabularyFactory {

	private static final Path CACHE_DIR;

	private static final Logger LOGGER = LoggerFactory.getLogger(VocabularyFactory.class);

	private static final Properties PROPS;

	static {
		try {
			PROPS = ClassProperties.load(VocabularyFactory.class);
			final String cacheDirResLoc = PROPS.getProperty("cacheDir");
			CACHE_DIR = Paths.get(cacheDirResLoc);
			LOGGER.debug("Using cache directory \"{}\".", CACHE_DIR);
		} catch (final IOException e) {
			throw new UncheckedIOException(e);
		}
	}

	private static NavigableSet<String> createWordList(final URL vocabResourceLoc, final Collator collator,
			final Function<? super String, String> normalizer) throws IOException {
		// Use a TreeSet so that iteration order is stable across invocations,
		// meaning that a feature with a given index will always have the
		// same meaning
		final NavigableSet<String> result = new TreeSet<>(collator);
		LOGGER.info("Reading word list at \"{}\".", vocabResourceLoc);
		try (final BufferedReader reader = new BufferedReader(new InputStreamReader(vocabResourceLoc.openStream()))) {
			for (String line = reader.readLine(); line != null; line = reader.readLine()) {
				// Remove any extra whitespace
				line = line.trim();
				if (!line.isEmpty()) {
					line = normalizer.apply(line);
					result.add(line);
				}
			}
		}
		return result;
	}

	private static List<String> deserializeWordList(final Path infile) throws IOException, ClassNotFoundException {
		try (ObjectInputStream instream = new ObjectInputStream(Files.newInputStream(infile))) {
			@SuppressWarnings("unchecked")
			final List<String> deserialized = (ArrayList<String>) instream.readObject();
			return deserialized;
		}
	}

	private static NavigableSet<String> deserializeWordList(final Path infile, final Collator collator)
			throws IOException, ClassNotFoundException {
		final List<String> words = deserializeWordList(infile);
		final NavigableSet<String> result = new TreeSet<>(collator);
		result.addAll(words);
		return result;
	}

	private static NavigableSet<String> fetchWordList(final Locale locale) throws IOException {
		final Collator collator = Collator.getInstance(locale);
		final Function<String, String> normalizer = str -> str.toLowerCase(locale);

		final String cachedVocabFileName = "words_" + locale.toLanguageTag() + ".ser";
		final Path cachedVocabPath = CACHE_DIR.resolve(cachedVocabFileName);

		final String vocabResLocStr = PROPS.getProperty("vocabFilePath");
		final URL vocabResourceLoc = ModelFeatureExtractor.class.getResource(vocabResLocStr);

		NavigableSet<String> result;
		try {
			final Path vocabFilePath = Paths.get(vocabResourceLoc.toURI());
			Optional<FileTime> optVocabFilePathAge = Optional.empty();
			try {
				optVocabFilePathAge = FileDataAge.getFileDataAge(vocabFilePath);
			} catch (final IOException e) {
				LOGGER.warn(String.format(
						"An I/O error occured while trying to get file attributes for vocab input file \"%s\".",
						vocabFilePath), e);
			}
			if (optVocabFilePathAge.isPresent()) {
				Optional<FileTime> optCachedVocabPathAge = Optional.empty();
				try {
					optCachedVocabPathAge = FileDataAge.getFileDataAge(cachedVocabPath);
				} catch (final IOException e) {
					LOGGER.info(String.format(
							"An I/O error occured while trying to get file attributes for cached vocab data file \"%s\".",
							cachedVocabPath), e);
				}
				if (optCachedVocabPathAge.isPresent()) {
					// If the input file is older than the cached file, use
					// the cached data
					if (optVocabFilePathAge.get().compareTo(optCachedVocabPathAge.get()) < 0) {
						try {
							result = deserializeWordList(cachedVocabPath, collator);
						} catch (ClassCastException | ClassNotFoundException e) {
							LOGGER.warn(String.format(
									"An error occurred while casting the deserialized data at \"%s\"; Creating a new word list from scratch.",
									cachedVocabPath), e);
							result = createWordList(vocabResourceLoc, collator, normalizer);
							serializeWordList(result, cachedVocabPath);
						}
					} else {
						LOGGER.info(
								"The input vocab file at \"{}\" is newer than the cached data; Creating a new word list from scratch.",
								vocabFilePath);
						result = createWordList(vocabResourceLoc, collator, normalizer);
						serializeWordList(result, cachedVocabPath);
					}
				} else {
					LOGGER.info(
							"Could not get file attributes for cached vocab data file \"{}\"; Creating a new word list from scratch.",
							cachedVocabPath);
					result = createWordList(vocabResourceLoc, collator, normalizer);
					serializeWordList(result, cachedVocabPath);
				}
			} else {
				LOGGER.warn(
						"Could not get file attributes for vocab input file \"{}\"; Creating a new word list from scratch.",
						vocabFilePath);
				result = createWordList(vocabResourceLoc, collator, normalizer);
				serializeWordList(result, cachedVocabPath);
			}

		} catch (final URISyntaxException e) {
			throw new AssertionError(e);
		}

		return result;
	}

	private static void serializeWordList(final List<String> words, final Path outfile) throws IOException {
		Files.createDirectories(CACHE_DIR);
		try (ObjectOutputStream outstream = new ObjectOutputStream(
				Files.newOutputStream(outfile, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING))) {
			outstream.writeObject(words);
		}
	}

	private static void serializeWordList(final NavigableSet<String> words, final Path outfile) throws IOException {
		serializeWordList(new ArrayList<>(words), outfile);
	}

	private final Locale locale;

	public VocabularyFactory() {
		this(Locale.forLanguageTag(PROPS.getProperty("lang")));
	}

	public VocabularyFactory(final Locale locale) {
		LOGGER.info("Using locale \"{}\".", locale);
		this.locale = locale;
	}

	public Vocabulary get() throws IOException {
		final NavigableSet<String> words = fetchWordList(locale);
		return new Vocabulary(words);
	}

}
