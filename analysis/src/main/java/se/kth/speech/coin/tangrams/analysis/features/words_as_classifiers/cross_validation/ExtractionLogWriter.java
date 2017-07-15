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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.cross_validation;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.io.Writer;
import java.util.List;
import java.util.function.BiConsumer;
import java.util.stream.Collector;
import java.util.stream.Collectors;

import edu.stanford.nlp.trees.Tree;
import edu.stanford.nlp.util.CoreMap;

final class ExtractionLogWriter implements BiConsumer<CoreMap, List<Tree>> {

	private static final String COL_DELIM = "\t";

	private static final Collector<CharSequence, ?, String> EXTR_PHRASE_JOINER = Collectors.joining(", ", "[", "]");

	private static final String ROW_DELIM = System.lineSeparator();

	private boolean hasStartedWriting;

	private final Writer writer;

	ExtractionLogWriter(final Writer writer) {
		this.writer = writer;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.BiConsumer#accept(java.lang.Object,
	 * java.lang.Object)
	 */
	@Override
	public void accept(final CoreMap sent, final List<Tree> extractedPhrases) {
		try {
			if (!hasStartedWriting) {
				writeHeader();
				hasStartedWriting = true;
			}

			writer.write(ROW_DELIM);
			final String sentRepr = sent.toString();
			writer.write(sentRepr);
			writer.write(COL_DELIM);
			writer.write(Integer.toString(extractedPhrases.size()));
			writer.write(COL_DELIM);
			writer.write(extractedPhrases.stream().map(Tree::toString).collect(EXTR_PHRASE_JOINER));
		} catch (final IOException e) {
			throw new UncheckedIOException(e);
		}
	}

	private void writeHeader() throws IOException {
		writer.write("SENTENCE");
		writer.write(COL_DELIM);
		writer.write("EXTR_PHRASE_COUNT");
		writer.write(COL_DELIM);
		writer.write("EXTR_PHRASES");
	}
}