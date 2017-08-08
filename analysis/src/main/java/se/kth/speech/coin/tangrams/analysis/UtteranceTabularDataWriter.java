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
package se.kth.speech.coin.tangrams.analysis;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.math.BigDecimal;
import java.math.MathContext;
import java.math.RoundingMode;
import java.time.Duration;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Optional;
import java.util.function.Supplier;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import iristk.system.Event;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeature;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeatureExtractionContextFactory;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfoTableRowWriter;
import se.kth.speech.coin.tangrams.iristk.EventTimes;
import se.kth.speech.coin.tangrams.iristk.EventTypeMatcher;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;
import se.kth.speech.coin.tangrams.iristk.ImageVisualizationInfoUnmarshaller;
import se.kth.speech.coin.tangrams.iristk.events.Move;

final class UtteranceTabularDataWriter {

	private enum EventDatum {
		LAST_RND_TIME, LAST_RND_TIME_DIFF, MOVE_SUBMITTER, TIME;
	}

	private enum LanguageDatum {
		DIALOGUE;
	}

	private static final Supplier<String> COL_HEADER_PADDING_SUPPLIER = () -> "";

	private static final EventDialogueFactory EVENT_DIAG_FACTORY = new EventDialogueFactory(
			new EventTypeMatcher(GameManagementEvent.NEXT_TURN_REQUEST));

	private static final MathContext EVT_TIME_DIFF_CTX = new MathContext(16, RoundingMode.HALF_UP);

	private static final ImageVisualizationInfoUnmarshaller IMG_VIZ_INFO_UNMARSHALLER = new ImageVisualizationInfoUnmarshaller();

	private static final Logger LOGGER = LoggerFactory.getLogger(UtteranceTabularDataWriter.class);

	private static final BigDecimal NANOS_TO_SECS_DIVISOR = new BigDecimal("1000000000");

	private static final String NULL_VALUE_REPR = "-";

	private static final Collector<CharSequence, ?, String> TABLE_ROW_CELL_JOINER;

	private static final Collector<CharSequence, ?, String> TABLE_ROW_JOINER;

	private static final String TABLE_STRING_REPR_COL_DELIMITER;

	private static final String TABLE_STRING_REPR_ROW_DELIMITER;

	private static final UtteranceDialogueRepresentationStringFactory UTT_DIAG_REPR_FACTORY = new UtteranceDialogueRepresentationStringFactory();

	static {
		TABLE_STRING_REPR_ROW_DELIMITER = System.lineSeparator();
		TABLE_ROW_JOINER = Collectors.joining(TABLE_STRING_REPR_ROW_DELIMITER);
	}

	static {
		TABLE_STRING_REPR_COL_DELIMITER = "\t";
		TABLE_ROW_CELL_JOINER = Collectors.joining(TABLE_STRING_REPR_COL_DELIMITER);
	}

	private static BigDecimal calculateTimeDiffSecs(final Event firstEvt, final Event nextEvt) {
		final LocalDateTime firstTime = EventTimes.parseEventTime(firstEvt.getTime());
		final LocalDateTime nextTime = EventTimes.parseEventTime(nextEvt.getTime());
		final long diffNanos = ChronoUnit.NANOS.between(firstTime, nextTime);
		return new BigDecimal(diffNanos).divide(NANOS_TO_SECS_DIVISOR, EVT_TIME_DIFF_CTX);
	}

	private static String createBlankEvtImgDesc(final List<String> evtImgFeatureCols) {
		final int colCount = evtImgFeatureCols.size();
		final String[] blankCells = new String[colCount];
		Arrays.fill(blankCells, NULL_VALUE_REPR);
		return Arrays.stream(blankCells).collect(TABLE_ROW_CELL_JOINER);
	}

	private static List<String> getEvtImgFeatureCols(final List<String> cols) {
		final int imgFeatureEndIdx = cols.size() - LanguageDatum.values().length;
		return cols.subList(0, imgFeatureEndIdx);
	}

	private final EntityFeatureExtractionContextFactory extractionContextFactory;

	private final EntityFeature.Extractor extractor;

	private final List<EntityFeature> featuresToDescribe;

	private final boolean strict;

	private final List<Utterance> utts;

	UtteranceTabularDataWriter(final List<Utterance> utts, final EntityFeature.Extractor extractor,
			final List<EntityFeature> featuresToDescribe,
			final EntityFeatureExtractionContextFactory extractionContextFactory, final boolean strict) {
		this.utts = utts;
		this.extractor = extractor;
		this.featuresToDescribe = featuresToDescribe;
		this.extractionContextFactory = extractionContextFactory;
		this.strict = strict;
	}

	private List<List<String>> createColHeaders() {
		final List<List<String>> imgViewDescColHeaders = ImageVisualizationInfoTableRowWriter.createColumnHeaders();
		final int resultColCount = imgViewDescColHeaders.stream().mapToInt(List::size).max().getAsInt()
				+ EventDatum.values().length + featuresToDescribe.size() + LanguageDatum.values().length;

		final Iterator<List<String>> imgDescHeaderIter = imgViewDescColHeaders.iterator();
		List<List<String>> result;
		if (imgDescHeaderIter.hasNext()) {
			result = new ArrayList<>(imgViewDescColHeaders.size());
			final List<String> firstHeader = new ArrayList<>(resultColCount);
			result.add(firstHeader);

			Arrays.stream(EventDatum.values()).map(EventDatum::toString).forEachOrdered(firstHeader::add);
			featuresToDescribe.stream().map(Object::toString).forEachOrdered(firstHeader::add);
			final List<String> firstImgDescHeader = imgDescHeaderIter.next();
			firstHeader.addAll(firstImgDescHeader);
			Arrays.stream(LanguageDatum.values()).map(LanguageDatum::toString).forEachOrdered(firstHeader::add);
			while (firstHeader.size() < resultColCount) {
				firstHeader.add(COL_HEADER_PADDING_SUPPLIER.get());
			}

			// Add subheader for image description-specific features,
			// e.g. color features
			while (imgDescHeaderIter.hasNext()) {
				final List<String> nextImgDescHeader = imgDescHeaderIter.next();
				final List<String> nextHeader = new ArrayList<>(resultColCount);
				result.add(nextHeader);

				// Add padding for evt cols
				Stream.generate(COL_HEADER_PADDING_SUPPLIER).limit(EventDatum.values().length).forEach(nextHeader::add);
				// Add padding for feature-derived descriptions
				Stream.generate(COL_HEADER_PADDING_SUPPLIER).limit(featuresToDescribe.size()).forEach(nextHeader::add);
				nextHeader.addAll(nextImgDescHeader);
				// Add padding for language cols
				Stream.generate(COL_HEADER_PADDING_SUPPLIER).limit(LanguageDatum.values().length)
						.forEach(nextHeader::add);
			}

		} else {
			result = Collections.emptyList();
		}
		assert result.stream().mapToInt(List::size).boxed().collect(Collectors.toSet()).size() <= 1;
		return result;
	}

	private String createNoEventUtterancesMsg(final Event event, final List<EventDialogue> eventDiags,
			final int eventIdx) {
		final StringBuilder sb = new StringBuilder(256);
		sb.append("No utterances for event index ");
		sb.append(eventIdx);
		sb.append(" \"");
		sb.append(event);
		sb.append("\".");
		{
			final ListIterator<EventDialogue> eventDiagIter = eventDiags.listIterator(eventIdx);
			EventDialogue prevEventDiag = null;
			while (eventDiagIter.hasPrevious()) {
				prevEventDiag = eventDiagIter.previous();
				final List<Utterance> prevUtts = prevEventDiag.getUtts();
				if (!prevUtts.isEmpty()) {
					break;
				}
			}
			if (prevEventDiag != null) {
				sb.append(System.lineSeparator());
				final Event prevEvent = prevEventDiag.getFirstEvent().orElse(null);
				final List<Utterance> prevUtts = prevEventDiag.getUtts();

				if (prevUtts.isEmpty()) {
					sb.append(String.format("Last utt before event: (NONE); event ID: \"%s\"; event time: \"%s\"",
							prevEvent.getId(), prevEvent.getTime()));
				} else {
					final Utterance prevUtt = prevUtts.get(prevUtts.size() - 1);
					final String speakingPlayerId = prevUtt.getSpeakerId();
					sb.append(String.format(
							"Last utt before event: \"%s\"; speaking player ID: \"%s\"; start: %f; end: %f; segment ID: \"%s\"; event ID: \"%s\"; event time: \"%s\"",
							prevUtt.getTokenStr(), speakingPlayerId, prevUtt.getStartTime(), prevUtt.getEndTime(),
							prevUtt.getSegmentId(), prevEvent.getId(), prevEvent.getTime()));
				}
			}
		}
		{
			final ListIterator<EventDialogue> eventDiagIter = eventDiags.listIterator(eventIdx + 1);
			EventDialogue nextEventDiag = null;
			while (eventDiagIter.hasNext()) {
				nextEventDiag = eventDiagIter.next();
				final List<Utterance> nextUtts = nextEventDiag.getUtts();
				if (!nextUtts.isEmpty()) {
					break;
				}
			}
			if (nextEventDiag != null) {
				sb.append(System.lineSeparator());
				final Event nextEvent = nextEventDiag.getFirstEvent().orElse(null);
				final List<Utterance> nextUtts = nextEventDiag.getUtts();
				if (nextUtts.isEmpty()) {
					// Do nothing
				} else {
					final Utterance nextUtt = nextUtts.get(0);
					final String speakingPlayerId = nextUtt.getSpeakerId();
					sb.append(String.format(
							"Next utt after event: \"%s\"; speaking player ID: \"%s\"; start: %f; end: %f; segment ID: \"%s\"; event ID: \"%s\"; event time: \"%s\"",
							nextUtt.getTokenStr(), speakingPlayerId, nextUtt.getStartTime(), nextUtt.getEndTime(),
							nextUtt.getSegmentId(), nextEvent.getId(), nextEvent.getTime()));
				}
			}
		}
		return sb.toString();
	}

	void write(final GameHistory history, final Writer writer) throws IOException {
		// The visualization info for the given game
		final ImageVisualizationInfo imgVizInfo = IMG_VIZ_INFO_UNMARSHALLER
				.apply(history.getInitialState().getImageVisualizationInfoDescription());
		final List<EventDialogue> eventDiags = Arrays
				.asList(EVENT_DIAG_FACTORY.apply(utts.listIterator(), history).toArray(EventDialogue[]::new));

		final List<List<String>> colHeaders = createColHeaders();
		final String colHeaderStr = colHeaders.stream().map(header -> header.stream().collect(TABLE_ROW_CELL_JOINER))
				.collect(TABLE_ROW_JOINER);
		writer.write(colHeaderStr);

		Event lastRoundEvent = null;
		for (final ListIterator<EventDialogue> eventDiagIter = eventDiags.listIterator(); eventDiagIter.hasNext();) {
			final EventDialogue eventDiag = eventDiagIter.next();
			writer.write(TABLE_STRING_REPR_ROW_DELIMITER);

			final String evtImgVizInfoDesc;
			final List<Event> diagEvts = eventDiag.getDialogueEvents();
			final List<Utterance> diagUtts = eventDiag.getUtts();
			if (diagEvts.isEmpty()) {
				final List<String> evtImgFeatureCols = getEvtImgFeatureCols(colHeaders.get(0));
				evtImgVizInfoDesc = createBlankEvtImgDesc(evtImgFeatureCols);
			} else {
				final Event firstDiagEvent = diagEvts.iterator().next();
				final float contextStartTime;
				final float contextEndTime;
				if (diagUtts.isEmpty()) {
					if (strict) {
						throw new IllegalArgumentException(
								String.format("No utterances for event \"%s\".", firstDiagEvent));
					} else {
						final String msg = createNoEventUtterancesMsg(firstDiagEvent, eventDiags,
								eventDiagIter.nextIndex() - 1);
						LOGGER.warn(msg);
						final LocalDateTime eventTime = EventTimes.parseEventTime(firstDiagEvent.getTime());
						final Duration gameDuration = Duration.between(history.getStartTime(), eventTime);
						final float offset = gameDuration.toMillis() / 1000.0f;
						contextStartTime = offset;
						contextEndTime = offset;
					}
				} else {
					// Just use the context of the first utterance
					final Utterance firstUtt = diagUtts.iterator().next();
					contextStartTime = firstUtt.getStartTime();
					contextEndTime = firstUtt.getEndTime();
				}

				if (lastRoundEvent == null) {
					writer.write(NULL_VALUE_REPR);
					writer.write(TABLE_STRING_REPR_COL_DELIMITER);
					writer.write(NULL_VALUE_REPR);
				} else {
					writer.write(lastRoundEvent.getTime());
					writer.write(TABLE_STRING_REPR_COL_DELIMITER);
					final BigDecimal lastRndEvtTimeDiff = calculateTimeDiffSecs(lastRoundEvent, firstDiagEvent);
					writer.write(lastRndEvtTimeDiff.toString());
				}
				writer.write(TABLE_STRING_REPR_COL_DELIMITER);
				writer.write(firstDiagEvent.getString(GameManagementEvent.Attribute.PLAYER_ID.toString()));
				writer.write(TABLE_STRING_REPR_COL_DELIMITER);
				writer.write(firstDiagEvent.getTime());
				writer.write(TABLE_STRING_REPR_COL_DELIMITER);
				{
					final GameContext context = TemporalGameContexts.create(history, contextStartTime, contextEndTime)
							.findFirst().get();
					final Optional<Integer> optSelectedEntityId = context.findLastSelectedEntityId();
					final String featureVectorRepr;
					if (optSelectedEntityId.isPresent()) {
						final EntityFeature.Extractor.Context extractionContext = extractionContextFactory
								.apply(context, optSelectedEntityId.get());
						final Stream<Optional<Object>> featureVals = featuresToDescribe.stream()
								.map(feature -> extractor.apply(feature, extractionContext));
						featureVectorRepr = featureVals.map(opt -> opt.map(Object::toString).orElse(NULL_VALUE_REPR))
								.collect(TABLE_ROW_CELL_JOINER);
					} else {
						final List<String> evtImgFeatureCols = getEvtImgFeatureCols(colHeaders.get(0));
						featureVectorRepr = createBlankEvtImgDesc(evtImgFeatureCols);
					}
					writer.write(featureVectorRepr);
				}
				writer.write(TABLE_STRING_REPR_COL_DELIMITER);
				final StringWriter strWriter = new StringWriter(256);
				{
					final ImageVisualizationInfoTableRowWriter imgInfoDescWriter = new ImageVisualizationInfoTableRowWriter(
							strWriter);
					final Move move = (Move) firstDiagEvent.get(GameManagementEvent.Attribute.MOVE.toString());
					final Integer selectedPieceId = move.getPieceId();
					final ImageVisualizationInfo.Datum selectedPieceImgVizInfo = imgVizInfo.getData()
							.get(selectedPieceId);
					LOGGER.debug("Writing selected piece (ID {}) viz info: {} ", selectedPieceId,
							selectedPieceImgVizInfo);
					imgInfoDescWriter.write(selectedPieceId, selectedPieceImgVizInfo);
				}
				evtImgVizInfoDesc = strWriter.toString();
				lastRoundEvent = diagEvts.get(diagEvts.size() - 1);
			}

			writer.write(evtImgVizInfoDesc);
			writer.write(TABLE_STRING_REPR_COL_DELIMITER);

			final String eventDialogStr = UTT_DIAG_REPR_FACTORY.apply(diagUtts.iterator());
			writer.write(eventDialogStr);
		}
	}

}