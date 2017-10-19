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
import java.time.temporal.TemporalAccessor;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.coin.tangrams.analysis.dialogues.WeightedUtterance;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues.UtteranceRelation;
import se.kth.speech.coin.tangrams.iristk.EventTimes;
import se.kth.speech.coin.tangrams.iristk.GameEvent;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Jul 15, 2017
 *
 */
final class UtteranceRelationLogWriter implements BiConsumer<EventDialogue, Iterable<UtteranceRelation>> {

	private static final String COL_DELIM = "\t";

	private static final String ROW_DELIM = System.lineSeparator();

	private static final Function<TemporalAccessor, String> TIMESTAMP_FORMATTER = EventTimes.FORMATTER::format;

	private static final Collector<CharSequence, ?, String> UTT_REPR_JOINER = Collectors.joining("\", \"", "\"", "\"");

	private final String nullCellValueRepr;

	private final Writer writer;

	UtteranceRelationLogWriter(final Writer writer, final String nullCellValueRepr) throws IOException {
		this.writer = writer;
		this.nullCellValueRepr = nullCellValueRepr;
		writeHeader();
	}

	@Override
	public void accept(final EventDialogue evtDiag, final Iterable<UtteranceRelation> uttRels) {
		try {
			final GameEvent firstEvent = evtDiag.getFirstEvent().get();
			int relNo = 1;
			for (final UtteranceRelation uttRel : uttRels) {
				writeRow(TIMESTAMP_FORMATTER.apply(firstEvent.getTime()), relNo++, uttRel);
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
		final Optional<WeightedUtterance> optInstrUtt = uttRel.getAcceptanceUtt();
		writer.write(optInstrUtt.map(WeightedUtterance::getUtterance).map(Utterance::createTokenString)
				.orElse(nullCellValueRepr));
		writer.write(COL_DELIM);
		writer.write(optInstrUtt.map(WeightedUtterance::getUtterance).map(Utterance::getTokens).map(List::size)
				.map(Object::toString).orElse(nullCellValueRepr));
		writer.write(COL_DELIM);
		writer.write(optInstrUtt.map(WeightedUtterance::getWeight).map(Object::toString).orElse(nullCellValueRepr));
		writer.write(COL_DELIM);
		final List<Utterance> prevUtts = uttRel.getPrevUtts();
		writer.write(Integer.toString(prevUtts.size()));
		writer.write(COL_DELIM);
		final Stream<String> prevUttWordClasses = prevUtts.stream().map(Utterance::getTokens).flatMap(List::stream);
		writer.write(Long.toString(prevUttWordClasses.count()));
		writer.write(COL_DELIM);
		final String[] prevUttReprs = prevUtts.stream().map(Utterance::createTokenString).toArray(String[]::new);
		if (prevUttReprs.length > 0) {
			writer.write(Arrays.stream(prevUttReprs).collect(UTT_REPR_JOINER));
		}
	}

}
