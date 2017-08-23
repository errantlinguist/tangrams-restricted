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
import java.io.UncheckedIOException;
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
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import iristk.system.Event;
import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeature;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeatureExtractionContextFactory;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfoTableRowWriter;
import se.kth.speech.coin.tangrams.iristk.EventTimes;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;
import se.kth.speech.coin.tangrams.iristk.ImageVisualizationInfoUnmarshaller;
import se.kth.speech.coin.tangrams.iristk.events.Move;

final class UtteranceTabularDataWriter {

	private enum EventDatum {
		LAST_RND_TIME {
			@Override
			String getValue(final Event firstDiagEvent, final Optional<Event> optLastRoundEvent) {
				return optLastRoundEvent.map(Event::getTime).orElse(NULL_VALUE_REPR);
			}
		},
		LAST_RND_TIME_DIFF {
			@Override
			String getValue(final Event firstDiagEvent, final Optional<Event> optLastRoundEvent) {
				return optLastRoundEvent.map(lastRoundEvent -> calculateTimeDiffSecs(lastRoundEvent, firstDiagEvent))
						.map(BigDecimal::toString).orElse(NULL_VALUE_REPR);
			}
		},
		MOVE_SUBMITTER {
			@Override
			String getValue(final Event firstDiagEvent, final Optional<Event> lastRoundEvent) {
				return firstDiagEvent.getString(GameManagementEvent.Attribute.PLAYER_ID.toString());
			}
		},
		NAME {
			@Override
			String getValue(final Event firstDiagEvent, final Optional<Event> optLastRoundEvent) {
				return firstDiagEvent.getName();
			}
		},
		TIME {
			@Override
			String getValue(final Event firstDiagEvent, final Optional<Event> optLastRoundEvent) {
				return firstDiagEvent.getTime();
			}
		};

		abstract String getValue(Event firstDiagEvent, Optional<Event> optLastRoundEvent);
	}

	private enum LanguageDatum {
		DIALOGUE {
			@Override
			String getValue(final List<Utterance> diagUtts,
					final Function<? super Iterator<Utterance>, String> uttDiagReprFactory) {
				return uttDiagReprFactory.apply(diagUtts.iterator());
			}
		};

		abstract String getValue(final List<Utterance> diagUtts,
				Function<? super Iterator<Utterance>, String> uttDiagReprFactory);
	}

	private static final Supplier<String> COL_HEADER_PADDING_SUPPLIER = () -> "";

	private static final MathContext EVT_TIME_DIFF_CTX = new MathContext(16, RoundingMode.HALF_UP);

	private static final ImageVisualizationInfoUnmarshaller IMG_VIZ_INFO_UNMARSHALLER = new ImageVisualizationInfoUnmarshaller();

	private static final Logger LOGGER = LoggerFactory.getLogger(UtteranceTabularDataWriter.class);

	private static final BigDecimal NANOS_TO_SECS_DIVISOR = new BigDecimal("1000000000");

	private static final String NULL_VALUE_REPR = "-";

	private static final Collector<CharSequence, ?, String> TABLE_ROW_CELL_JOINER;

	private static final Collector<CharSequence, ?, String> TABLE_ROW_JOINER;

	private static final String TABLE_STRING_REPR_COL_DELIMITER;

	private static final String TABLE_STRING_REPR_ROW_DELIMITER;

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

	private static String createBlankImgVizInfoDesc() {
		final StringWriter strWriter = new StringWriter(16);
		final ImageVisualizationInfoTableRowWriter imgInfoDescWriter = new ImageVisualizationInfoTableRowWriter(
				strWriter, NULL_VALUE_REPR);
		try {
			imgInfoDescWriter.write(null, null);
			return strWriter.toString();
		} catch (final IOException e) {
			// Should not happen when writing to a StringBuffer
			throw new UncheckedIOException(e);
		}
	}

	private static String createImgVizInfoDesc(final Move move, final LocalDateTime gameStartTime,
			final List<ImageVisualizationInfo.Datum> imgVizInfoData) {
		final StringWriter strWriter = new StringWriter(256);
		final ImageVisualizationInfoTableRowWriter imgInfoDescWriter = new ImageVisualizationInfoTableRowWriter(
				strWriter, NULL_VALUE_REPR);
		final Integer selectedPieceId = move.getPieceId();
		final ImageVisualizationInfo.Datum selectedPieceImgVizInfo = imgVizInfoData.get(selectedPieceId);
		LOGGER.debug("Writing selected piece (ID {}) viz info: {} ", selectedPieceId, selectedPieceImgVizInfo);
		try {
			imgInfoDescWriter.write(selectedPieceId, selectedPieceImgVizInfo);
			return strWriter.toString();
		} catch (final IOException e) {
			// Should not happen when writing to a StringBuffer
			throw new UncheckedIOException(e);
		}
	}

	private final String blankImgVizInfoDesc;

	private final List<List<String>> colHeaders;

	private final EntityFeature.Extractor entityFeatureExtractor;

	private final List<EntityFeature> entityFeaturesToDescribe;

	private final EventDatum[] eventDataToWrite;

	private final EntityFeatureExtractionContextFactory extractionContextFactory;

	private final LanguageDatum[] langDataToWrite;

	private final boolean strict;

	private final Function<? super Iterator<Utterance>, String> uttDiagReprFactory;

	private UtteranceTabularDataWriter(final EntityFeature.Extractor entityFeatureExtractor,
			final List<EntityFeature> entityFeaturesToDescribe,
			final EntityFeatureExtractionContextFactory extractionContextFactory,
			final Function<? super Iterator<Utterance>, String> uttDiagReprFactory, final boolean strict,
			final EventDatum[] eventDataToWrite, final LanguageDatum[] langDataToWrite) {
		this.entityFeatureExtractor = entityFeatureExtractor;
		this.entityFeaturesToDescribe = entityFeaturesToDescribe;
		this.extractionContextFactory = extractionContextFactory;
		this.uttDiagReprFactory = uttDiagReprFactory;
		this.strict = strict;
		this.eventDataToWrite = eventDataToWrite;
		this.langDataToWrite = langDataToWrite;

		colHeaders = createColHeaders();

		blankImgVizInfoDesc = createBlankImgVizInfoDesc();
	}

	UtteranceTabularDataWriter(final EntityFeature.Extractor extractor, final List<EntityFeature> featuresToDescribe,
			final EntityFeatureExtractionContextFactory extractionContextFactory,
			final Function<? super Iterator<Utterance>, String> uttDiagReprFactory, final boolean strict) {
		this(extractor, featuresToDescribe, extractionContextFactory, uttDiagReprFactory, strict, EventDatum.values(),
				LanguageDatum.values());
	}

	private List<List<String>> createColHeaders() {
		final List<List<String>> imgViewDescColHeaders = ImageVisualizationInfoTableRowWriter.createColumnHeaders();
		final int resultColCount = imgViewDescColHeaders.stream().mapToInt(List::size).max().getAsInt()
				+ eventDataToWrite.length + entityFeaturesToDescribe.size() + langDataToWrite.length;

		final Iterator<List<String>> imgDescHeaderIter = imgViewDescColHeaders.iterator();
		final List<List<String>> result;
		if (imgDescHeaderIter.hasNext()) {
			result = new ArrayList<>(imgViewDescColHeaders.size());
			final List<String> firstHeader = new ArrayList<>(resultColCount);
			result.add(firstHeader);

			Arrays.stream(eventDataToWrite).map(EventDatum::toString).forEachOrdered(firstHeader::add);
			entityFeaturesToDescribe.stream().map(Object::toString).forEachOrdered(firstHeader::add);
			final List<String> firstImgDescHeader = imgDescHeaderIter.next();
			firstHeader.addAll(firstImgDescHeader);
			final int paddingCellsToAdd = resultColCount - langDataToWrite.length - firstHeader.size();
			Stream.generate(COL_HEADER_PADDING_SUPPLIER).limit(paddingCellsToAdd).forEach(firstHeader::add);
			Arrays.stream(langDataToWrite).map(LanguageDatum::toString).forEachOrdered(firstHeader::add);

			// Add subheader for image description-specific features,
			// e.g. color features
			while (imgDescHeaderIter.hasNext()) {
				final List<String> nextImgDescHeader = imgDescHeaderIter.next();
				final List<String> nextHeader = new ArrayList<>(resultColCount);
				result.add(nextHeader);

				// Add padding for evt cols
				Stream.generate(COL_HEADER_PADDING_SUPPLIER).limit(eventDataToWrite.length).forEach(nextHeader::add);
				// Add padding for feature-derived descriptions
				Stream.generate(COL_HEADER_PADDING_SUPPLIER).limit(entityFeaturesToDescribe.size())
						.forEach(nextHeader::add);
				nextHeader.addAll(nextImgDescHeader);
				// Add padding for language cols
				Stream.generate(COL_HEADER_PADDING_SUPPLIER).limit(langDataToWrite.length).forEach(nextHeader::add);
			}

		} else {
			result = Collections.emptyList();
		}
		assert result.stream().mapToInt(List::size).boxed().collect(Collectors.toSet()).size() <= 1;
		return result;
	}

	private String createImgVizInfoDesc(final Event firstDiagEvent, final LocalDateTime gameStartTime,
			final List<ImageVisualizationInfo.Datum> imgVizInfoData) throws IOException {
		final String result;
		final Move move = (Move) firstDiagEvent.get(GameManagementEvent.Attribute.MOVE.toString());
		if (move == null) {
			result = blankImgVizInfoDesc;
		} else {
			result = createImgVizInfoDesc(move, gameStartTime, imgVizInfoData);
		}
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
				final List<Utterance> prevUtts = prevEventDiag.getUtterances();
				if (!prevUtts.isEmpty()) {
					break;
				}
			}
			if (prevEventDiag != null) {
				sb.append(System.lineSeparator());
				final Event prevEvent = prevEventDiag.getFirstEvent().orElse(null);
				final List<Utterance> prevUtts = prevEventDiag.getUtterances();

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
				final List<Utterance> nextUtts = nextEventDiag.getUtterances();
				if (!nextUtts.isEmpty()) {
					break;
				}
			}
			if (nextEventDiag != null) {
				sb.append(System.lineSeparator());
				final Event nextEvent = nextEventDiag.getFirstEvent().orElse(null);
				final List<Utterance> nextUtts = nextEventDiag.getUtterances();
				if (nextUtts.isEmpty()) {
					LOGGER.debug("No utterances for dialogue ID \"{}\".", nextEvent.getId());
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

	private Stream<String> getBlankImgFeatureValueReprs() {
		return entityFeaturesToDescribe.stream().map(feature -> NULL_VALUE_REPR);
	}

	private Stream<String> getImgFeatureValueReprs(final GameContext context) {
		final Optional<Integer> optSelectedEntityId = context.findLastSelectedEntityId();
		final Stream<String> result;
		if (optSelectedEntityId.isPresent()) {
			final EntityFeature.Extractor.Context extractionContext = extractionContextFactory.apply(context,
					optSelectedEntityId.get());
			final Stream<Optional<Object>> featureVals = entityFeaturesToDescribe.stream()
					.map(feature -> entityFeatureExtractor.apply(feature, extractionContext));
			result = featureVals.map(opt -> opt.map(Object::toString).orElse(NULL_VALUE_REPR));
		} else {
			result = getBlankImgFeatureValueReprs();
		}
		return result;
	}

	void accept(final GameHistory history, final List<EventDialogue> eventDiags, final Writer writer)
			throws IOException {
		// Write header
		writer.write(colHeaders.stream().map(header -> header.stream().collect(TABLE_ROW_CELL_JOINER))
				.collect(TABLE_ROW_JOINER));

		// The visualization info for the given game
		final LocalDateTime gameStartTime = history.getStartTime();
		final List<ImageVisualizationInfo.Datum> imgVizInfoData = IMG_VIZ_INFO_UNMARSHALLER
				.apply(history.getInitialState().getImageVisualizationInfoDescription()).getData();

		Optional<Event> optLastRoundEvent = Optional.empty();
		for (final ListIterator<EventDialogue> eventDiagIter = eventDiags.listIterator(); eventDiagIter.hasNext();) {
			final EventDialogue eventDiag = eventDiagIter.next();
			writer.write(TABLE_STRING_REPR_ROW_DELIMITER);

			final List<Event> diagEvts = eventDiag.getEvents();
			final List<Utterance> diagUtts = eventDiag.getUtterances();

			final Stream<String> eventDataReprs;
			final Stream<String> imgFeatureVectorReprs;
			final String imgVizInfoDesc;
			if (diagEvts.isEmpty()) {
				eventDataReprs = Arrays.stream(eventDataToWrite).map(eventDatum -> NULL_VALUE_REPR);
				imgFeatureVectorReprs = getBlankImgFeatureValueReprs();
				imgVizInfoDesc = blankImgVizInfoDesc;
			} else {
				final Event firstDiagEvent = diagEvts.iterator().next();
				{
					final Optional<Event> streamOptLastRoundEvent = optLastRoundEvent;
					eventDataReprs = Arrays.stream(eventDataToWrite)
							.map(eventDatum -> eventDatum.getValue(firstDiagEvent, streamOptLastRoundEvent));
				}

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
						final Duration gameDuration = Duration.between(gameStartTime, eventTime);
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

				final GameContext context = TemporalGameContexts.create(history, contextStartTime, contextEndTime)
						.findFirst().get();
				imgFeatureVectorReprs = getImgFeatureValueReprs(context);

				imgVizInfoDesc = createImgVizInfoDesc(firstDiagEvent, gameStartTime, imgVizInfoData);

				optLastRoundEvent = Optional.of(diagEvts.get(diagEvts.size() - 1));
			}
			writer.write(eventDataReprs.collect(TABLE_ROW_CELL_JOINER));
			writer.write(TABLE_STRING_REPR_COL_DELIMITER);
			writer.write(imgFeatureVectorReprs.collect(TABLE_ROW_CELL_JOINER));
			writer.write(TABLE_STRING_REPR_COL_DELIMITER);
			writer.write(imgVizInfoDesc);

			for (final LanguageDatum langDatum : langDataToWrite) {
				writer.write(TABLE_STRING_REPR_COL_DELIMITER);
				final String datumValue = langDatum.getValue(diagUtts, uttDiagReprFactory);
				writer.write(datumValue);
			}
		}
	}

	void accept(final SessionGame sessionGame, final Writer writer) throws IOException {
		final GameHistory history = sessionGame.getHistory();
		final List<EventDialogue> eventDiags = sessionGame.getEventDialogues();
		accept(history, eventDiags, writer);
	}

}