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
package se.kth.speech.coin.tangrams;

import java.io.File;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileVisitOption;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.HashSet;
import java.util.OptionalInt;
import java.util.Set;
import java.util.stream.Stream;

import org.apache.commons.cli.ParseException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 1 Jun 2017
 *
 */
public final class CLIParameters {

	private static final Charset DEFAULT_OUTPUT_ENCODING = StandardCharsets.UTF_8;

	private static final Logger LOGGER = LoggerFactory.getLogger(CLIParameters.class);

	public static Set<String> parseAppCtxDefPaths(final String[] appCtxLocs) throws IOException {
		final Set<String> result = new HashSet<>();
		for (final String appCtxLoc : appCtxLocs) {
			final Path appCtxPath = Paths.get(appCtxLoc);
			try (Stream<Path> childPaths = Files.walk(appCtxPath, FileVisitOption.FOLLOW_LINKS)) {
				final Stream<Path> xmlFilePaths = childPaths.filter(inpath -> {
					boolean shouldBeParsed = false;
					try {
						final String contentType = Files.probeContentType(inpath);
						shouldBeParsed = contentType != null && contentType.endsWith("/xml");
					} catch (final IOException e) {
						LOGGER.warn("A(n) {} occurred while probing the content type of \"{}\"; Skipping file.",
								new Object[] { e.getClass().getSimpleName(), inpath }, e);
						shouldBeParsed = true;
					}
					return shouldBeParsed;
				});
				xmlFilePaths.map(Path::toAbsolutePath).map(Path::toString).forEach(result::add);
			}
		}
		return result;
	}

	public static OptionalInt parseIterCount(final Number optVal) throws ParseException {
		final OptionalInt result;
		if (optVal == null) {
			result = OptionalInt.empty();
		} else {
			final int val = optVal.intValue();
			LOGGER.info("Will run {} training/testing iteration(s).", val);
			result = OptionalInt.of(val);
		}
		return result;
	}

	public static PrintWriter parseOutpath(final File outfile) throws IOException {
		return parseOutpath(outfile, DEFAULT_OUTPUT_ENCODING);
	}

	public static PrintWriter parseOutpath(final File outfile, final Charset outputEncoding) throws IOException {
		final PrintWriter result;
		if (outfile == null) {
			LOGGER.info("No output file path specified; Writing to standard output.");
			result = new PrintWriter(new OutputStreamWriter(System.out, outputEncoding));
		} else {
			LOGGER.info("Output file path is \"{}\".", outfile);
			result = new PrintWriter(Files.newBufferedWriter(outfile.toPath(), outputEncoding,
					StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING));
		}
		return result;
	}

	public static String parseOutputType(final String outputFileExtension) {
		final String result;
		final char extPrefix = '.';
		if (outputFileExtension.charAt(0) == extPrefix) {
			result = outputFileExtension;
		} else {
			result = extPrefix + outputFileExtension;
		}
		return result;
	}

	private CLIParameters() {
	}

}
