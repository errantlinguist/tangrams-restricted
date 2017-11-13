/**
 *
 */
package se.kth.speech;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.stream.Stream;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 12 Nov 2017
 *
 */
public final class CommonPaths {

	private static final char CANONICAL_PATH_SEPARATOR;

	/**
	 * This is a hack for use with {@link String#split(String)} because
	 * single-character regex strings have an optimized implementation but,
	 * strangely, there is no method which takes a <code>char</code> instead.
	 */
	private static final String CANONICAL_PATH_SEPARATOR_PATTERN;

	static {
		CANONICAL_PATH_SEPARATOR = '/';
		CANONICAL_PATH_SEPARATOR_PATTERN = "" + CANONICAL_PATH_SEPARATOR;
	}

	public static Path findCommonPrefixPath(final Stream<? extends Path> paths) {
		// Rewrite Windows-style paths
		final String[] normalizedPathStrs = paths.map(path -> path.toString().replace('\\', CANONICAL_PATH_SEPARATOR))
				.toArray(String[]::new);
		final String commonPrefix = findCommonPrefix(normalizedPathStrs);
		return Paths.get(commonPrefix);
	}

	/**
	 *
	 * @param paths
	 * @return
	 * @see <a href=
	 *      "https://www.rosettacode.org/wiki/Find_common_directory_path#Java">Rosetta
	 *      Code</a>
	 *
	 */
	private static String findCommonPrefix(final String... paths) {
		final int maxPrefixLength = Arrays.stream(paths).mapToInt(String::length).min().orElse(0);
		final StringBuilder resultBuilder = new StringBuilder(maxPrefixLength);
		final String[][] folders = new String[paths.length][];
		for (int i = 0; i < paths.length; i++) {
			folders[i] = paths[i].split(CANONICAL_PATH_SEPARATOR_PATTERN); // split
																			// on
																			// file
																			// separator
		}
		for (int j = 0; j < folders[0].length; j++) {
			final String thisFolder = folders[0][j]; // grab the next folder
														// name in the first
														// path
			boolean allMatched = true; // assume all have matched in case there
										// are no more paths
			for (int i = 1; i < folders.length && allMatched; i++) { // look at
																		// the
																		// other
																		// paths
				if (folders[i].length < j) { // if there is no folder here
					allMatched = false; // no match
					break; // stop looking because we've gone as far as we can
				}
				// otherwise
				allMatched &= folders[i][j].equals(thisFolder); // check if it
																// matched
			}
			if (allMatched) { // if they all matched this folder name
				// add it to the answer
				resultBuilder.append(thisFolder);
				resultBuilder.append(CANONICAL_PATH_SEPARATOR);
			} else {// otherwise
				break;// stop looking
			}
		}
		return resultBuilder.toString();
	}

	private CommonPaths() {
	}

}
