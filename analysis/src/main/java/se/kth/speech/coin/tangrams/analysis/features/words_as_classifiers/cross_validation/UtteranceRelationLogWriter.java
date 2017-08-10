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
import java.util.Arrays;
import java.util.List;
import java.util.function.BiConsumer;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import iristk.system.Event;
import se.kth.speech.coin.tangrams.analysis.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.Utterance;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues.UtteranceRelation;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Jul 15, 2017
 *
 */
final class UtteranceRelationLogWriter implements BiConsumer<EventDialogue, Iterable<UtteranceRelation>> {

	private static final String COL_DELIM = "\t";

	private static final String ROW_DELIM = System.lineSeparator();

	private static final Collector<CharSequence, ?, String> UTT_REPR_JOINER = Collectors.joining("\", \"", "\"", "\"");

	private final Writer writer;

	UtteranceRelationLogWriter(final Writer writer) throws IOException {
		this.writer = writer;
		writeHeader();
	}

	@Override
	public void accept(final EventDialogue evtDiag, final Iterable<UtteranceRelation> uttRels) {
		try {
			final Event firstEvent = evtDiag.getFirstEvent().get();
			int relNo = 1;
			for (final UtteranceRelation uttRel : uttRels) {
				writeRow(firstEvent.getTime(), relNo++, uttRel);
			}
		} catch (final IOException e) {
			throw new UncheckedIOException(e);
		}

	}

	private void writeHeader() throws IOException {
		writer.write("EVENT_TIME");
		writer.write(COL_DELIM);
		writer.write("DIAG_REL_NO");
		writer.write(COL_DELIM);
		writer.write("INSTR_UTT");
		writer.write(COL_DELIM);
		writer.write("INSTR_UTT_TOKEN_COUNT");
		writer.write(COL_DELIM);
		writer.write("SENT_VALUE");
		writer.write(COL_DELIM);
		writer.write("PREV_UTT_COUNT");
		writer.write(COL_DELIM);
		writer.write("PREV_UTT_TOTAL_TOKENS");
		writer.write(COL_DELIM);
		writer.write("PREV_UTTS");
	}

	private void writeRow(final String eventTime, final int relNo, final UtteranceRelation uttRel) throws IOException {
		writer.write(ROW_DELIM);
		writer.write(eventTime);
		writer.write(COL_DELIM);
		writer.write(Integer.toString(relNo));
		writer.write(COL_DELIM);
		final Utterance instrUtt = uttRel.getAcceptanceUtt();
		writer.write(instrUtt.getTokenStr());
		writer.write(COL_DELIM);
		writer.write(Long.toString(instrUtt.getTokens().stream().count()));
		writer.write(COL_DELIM);
		writer.write(Double.toString(uttRel.getAcceptanceValue()));
		writer.write(COL_DELIM);
		final List<Utterance> prevUtts = uttRel.getPrevUtts();
		writer.write(Integer.toString(prevUtts.size()));
		writer.write(COL_DELIM);
		final Stream<String> prevUttWordClasses = prevUtts.stream().map(Utterance::getTokens).flatMap(List::stream);
		writer.write(Long.toString(prevUttWordClasses.count()));
		writer.write(COL_DELIM);
		final String[] prevUttReprs = prevUtts.stream().map(Utterance::getTokenStr).toArray(String[]::new);
		if (prevUttReprs.length > 0) {
			writer.write(Arrays.stream(prevUttReprs).collect(UTT_REPR_JOINER));
		}
	}

}
