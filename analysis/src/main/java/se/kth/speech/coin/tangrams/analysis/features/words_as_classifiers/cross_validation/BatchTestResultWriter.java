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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.cross_validation;

import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.nio.file.OpenOption;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 24 May 2017
 *
 */
final class BatchTestResultWriter {

	private static final StandardOpenOption[] DEFAULT_FILE_OPEN_OPTS = new StandardOpenOption[] {
			StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING };

	private static final Logger LOGGER = LoggerFactory.getLogger(BatchTestResultWriter.class);

	private final OpenOption[] fileOpenOpts;

	private final Path outdir;

	private BatchTestResultWriter(final Path outdir, final OpenOption[] fileOpenOpts) {
		this.outdir = outdir;
		this.fileOpenOpts = fileOpenOpts;
	}

	BatchTestResultWriter(final Path outdir) {
		this(outdir, DEFAULT_FILE_OPEN_OPTS);
	}

	void accept(final Tester.Result result) throws IOException {
		final Path statsFilePath = outdir.resolve("stats.tsv");
		try (final PrintWriter out = new PrintWriter(Files.newBufferedWriter(statsFilePath, fileOpenOpts))) {
			final StatisticsWriter writer = new StatisticsWriter(out);
			writer.accept(result);
		}
		LOGGER.info("Wrote cross-validation statistics to \"{}\".", statsFilePath);

		final Path diagAnalysisPath = outdir.resolve("diag-analysis-firstiter.tsv");
		try (final PrintWriter out = new PrintWriter(Files.newBufferedWriter(diagAnalysisPath, fileOpenOpts))) {
			final DialogueAnalysisWriter writer = new DialogueAnalysisWriter(out, 1);
			writer.accept(result);
		}
		LOGGER.info("Wrote dialogue analysis to \"{}\".", diagAnalysisPath);
	}

}
