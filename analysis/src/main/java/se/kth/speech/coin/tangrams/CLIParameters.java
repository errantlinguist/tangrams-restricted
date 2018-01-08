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
package se.kth.speech.coin.tangrams;

import java.io.File;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.StandardOpenOption;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since 1 Jun 2017
 *
 */
public final class CLIParameters {

	private static final Charset DEFAULT_OUTPUT_ENCODING = StandardCharsets.UTF_8;

	private static final Logger LOGGER = LoggerFactory.getLogger(CLIParameters.class);

	public static PrintWriter parseOutpath(final File outfile) throws IOException {
		return parseOutpath(outfile, DEFAULT_OUTPUT_ENCODING);
	}

	public static PrintWriter parseOutpath(final File outfile, final Charset outputEncoding) throws IOException { // NO_UCD (use private)
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

	private CLIParameters() {
	}

}
