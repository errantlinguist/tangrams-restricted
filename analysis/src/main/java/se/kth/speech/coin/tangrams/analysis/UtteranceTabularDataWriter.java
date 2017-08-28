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
import java.io.Writer;
import java.math.BigDecimal;
import java.math.MathContext;
import java.math.RoundingMode;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import iristk.system.Event;
import se.kth.speech.TimestampArithmetic;
import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeature;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfoTableRowCellFactory;
import se.kth.speech.coin.tangrams.iristk.EventTimes;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;
import se.kth.speech.coin.tangrams.iristk.ImageVisualizationInfoUnmarshaller;
import se.kth.speech.coin.tangrams.iristk.events.Move;

final class UtteranceTabularDataWriter {

	private enum EventDatum implements Function<UtteranceTabularDataWriter.EventDatum.Context, String> {
		LAST_RND_TIME {
			@Override
			public String apply(final EventDatum.Context ctx) {
				return ctx.optLastRoundEvent
						.map(lastRoundEvent -> calculateDecimalSecondDifference(ctx.gameStartTime, lastRoundEvent))
						.map(TIME_OFFSET_FORMATTER).orElseGet(ctx.nullValueReprSupplier);
			}
		},
		LAST_RND_TIME_DIFF {
			@Override
			public String apply(final EventDatum.Context ctx) {
				return ctx.optLastRoundEvent
						.map(lastRoundEvent -> calculateDecimalSecondDifference(lastRoundEvent, ctx.firstDiagEvent))
						.map(TIME_OFFSET_FORMATTER).orElseGet(ctx.nullValueReprSupplier);
			}
		},
		MOVE_SUBMITTER {
			@Override
			public String apply(final EventDatum.Context ctx) {
				return ctx.firstDiagEvent.getString(GameManagementEvent.Attribute.PLAYER_ID.toString());
			}
		},
		NAME {
			@Override
			public String apply(final EventDatum.Context ctx) {
				return ctx.firstDiagEvent.getName();
			}
		},
		TIME {
			@Override
			public String apply(final EventDatum.Context ctx) {
				return TIME_OFFSET_FORMATTER
						.apply(calculateDecimalSecondDifference(ctx.gameStartTime, ctx.firstDiagEvent));
			}
		};

		private static class Context {

			private final Event firstDiagEvent;

			private final LocalDateTime gameStartTime;

			private final Supplier<String> nullValueReprSupplier;

			private final Optional<? extends Event> optLastRoundEvent;

			private Context(final Event firstDiagEvent, final Optional<? extends Event> optLastRoundEvent,
					final LocalDateTime gameStartTime, final Supplier<String> nullValueReprSupplier) {
				this.firstDiagEvent = firstDiagEvent;
				this.optLastRoundEvent = optLastRoundEvent;
				this.gameStartTime = gameStartTime;
				this.nullValueReprSupplier = nullValueReprSupplier;
			}

			/*
			 * (non-Javadoc)
			 *
			 * @see java.lang.Object#equals(java.lang.Object)
			 */
			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}
				if (obj == null) {
					return false;
				}
				if (!(obj instanceof Context)) {
					return false;
				}
				final Context other = (Context) obj;
				if (firstDiagEvent == null) {
					if (other.firstDiagEvent != null) {
						return false;
					}
				} else if (!firstDiagEvent.equals(other.firstDiagEvent)) {
					return false;
				}
				if (gameStartTime == null) {
					if (other.gameStartTime != null) {
						return false;
					}
				} else if (!gameStartTime.equals(other.gameStartTime)) {
					return false;
				}
				if (nullValueReprSupplier == null) {
					if (other.nullValueReprSupplier != null) {
						return false;
					}
				} else if (!nullValueReprSupplier.equals(other.nullValueReprSupplier)) {
					return false;
				}
				if (optLastRoundEvent == null) {
					if (other.optLastRoundEvent != null) {
						return false;
					}
				} else if (!optLastRoundEvent.equals(other.optLastRoundEvent)) {
					return false;
				}
				return true;
			}

			/*
			 * (non-Javadoc)
			 *
			 * @see java.lang.Object#hashCode()
			 */
			@Override
			public int hashCode() {
				final int prime = 31;
				int result = 1;
				result = prime * result + (firstDiagEvent == null ? 0 : firstDiagEvent.hashCode());
				result = prime * result + (gameStartTime == null ? 0 : gameStartTime.hashCode());
				result = prime * result + (nullValueReprSupplier == null ? 0 : nullValueReprSupplier.hashCode());
				result = prime * result + (optLastRoundEvent == null ? 0 : optLastRoundEvent.hashCode());
				return result;
			}

			/*
			 * (non-Javadoc)
			 *
			 * @see java.lang.Object#toString()
			 */
			@Override
			public String toString() {
				final StringBuilder builder = new StringBuilder();
				builder.append("Context [firstDiagEvent=");
				builder.append(firstDiagEvent);
				builder.append(", gameStartTime=");
				builder.append(gameStartTime);
				builder.append(", nullValueReprSupplier=");
				builder.append(nullValueReprSupplier);
				builder.append(", optLastRoundEvent=");
				builder.append(optLastRoundEvent);
				builder.append("]");
				return builder.toString();
			}
		}

	}

	private enum LanguageDatum
			implements BiFunction<List<Utterance>, Function<? super Iterator<Utterance>, String>, String> {
		DIALOGUE {
			@Override
			public String apply(final List<Utterance> diagUtts,
					final Function<? super Iterator<Utterance>, String> uttDiagReprFactory) {
				return uttDiagReprFactory.apply(diagUtts.iterator());
			}
		};

	}

	private static final Supplier<String> COL_HEADER_PADDING_SUPPLIER = () -> "";

	private static final Function<String, LocalDateTime> EVENT_TIME_PARSER = EventTimes::parseEventTime;

	private static final MathContext EVT_TIME_DIFF_CTX = new MathContext(16, RoundingMode.HALF_UP);

	private static final List<ImageVisualizationInfoTableRowCellFactory.Attribute> IMG_VIZ_INFO_ATTRS_TO_WRITE;

	private static final ImageVisualizationInfoUnmarshaller IMG_VIZ_INFO_UNMARSHALLER = new ImageVisualizationInfoUnmarshaller();

	private static final Logger LOGGER = LoggerFactory.getLogger(UtteranceTabularDataWriter.class);

	private static final Collector<CharSequence, ?, String> TABLE_ROW_CELL_JOINER;

	private static final Collector<CharSequence, ?, String> TABLE_ROW_JOINER;

	private static final String TABLE_STRING_REPR_COL_DELIMITER;

	private static final String TABLE_STRING_REPR_ROW_DELIMITER;

	private static final Function<BigDecimal, String> TIME_OFFSET_FORMATTER = BigDecimal::toString;

	static {
		TABLE_STRING_REPR_ROW_DELIMITER = System.lineSeparator();
		TABLE_ROW_JOINER = Collectors.joining(TABLE_STRING_REPR_ROW_DELIMITER);

		TABLE_STRING_REPR_COL_DELIMITER = "\t";
		TABLE_ROW_CELL_JOINER = Collectors.joining(TABLE_STRING_REPR_COL_DELIMITER);

		IMG_VIZ_INFO_ATTRS_TO_WRITE = ImageVisualizationInfoTableRowCellFactory.Attribute.getCanonicalOrdering();
	}

	private static BigDecimal calculateDecimalSecondDifference(final Event firstEvt, final Event nextEvt) {
		final LocalDateTime firstTime = EVENT_TIME_PARSER.apply(firstEvt.getTime());
		return calculateDecimalSecondDifference(firstTime, nextEvt);
	}

	private static BigDecimal calculateDecimalSecondDifference(final LocalDateTime firstTime, final Event nextEvt) {
		final LocalDateTime nextTime = EVENT_TIME_PARSER.apply(nextEvt.getTime());
		return TimestampArithmetic.calculateDecimalSecondDifference(firstTime, nextTime, EVT_TIME_DIFF_CTX);
	}

	private final List<List<String>> colHeaders;

	private final Collection<EntityFeature> entityFeaturesToDescribe;

	private final EntityFeatureVectorDescriptionFactory entityFeatureVectorDescFactory;

	private final EventDatum[] eventDataToWrite;

	private final ImageVisualizationInfoDescriptionFactory imgVizInfoDescFactory;

	private final LanguageDatum[] langDataToWrite;

	private final Supplier<String> nullValueReprSupplier;

	private final boolean strict;

	private final Function<? super Iterator<Utterance>, String> uttDiagReprFactory;

	UtteranceTabularDataWriter(final EntityFeatureVectorDescriptionFactory entityFeatureVectorDescFactory,
			final Function<? super Iterator<Utterance>, String> uttDiagReprFactory, final String nullValueRepr,
			final boolean strict) {
		this(entityFeatureVectorDescFactory, uttDiagReprFactory, nullValueRepr, strict, EventDatum.values(),
				LanguageDatum.values());
	}

	UtteranceTabularDataWriter(final EntityFeatureVectorDescriptionFactory entityFeatureVectorDescFactory,
			final Function<? super Iterator<Utterance>, String> uttDiagReprFactory, final String nullValueRepr,
			final boolean strict, final EventDatum[] eventDataToWrite, final LanguageDatum[] langDataToWrite) {
		this.entityFeatureVectorDescFactory = entityFeatureVectorDescFactory;
		entityFeaturesToDescribe = entityFeatureVectorDescFactory.getEntityFeaturesToDescribe();
		this.uttDiagReprFactory = uttDiagReprFactory;
		nullValueReprSupplier = () -> nullValueRepr;
		this.strict = strict;
		this.eventDataToWrite = eventDataToWrite;
		this.langDataToWrite = langDataToWrite;
		imgVizInfoDescFactory = new ImageVisualizationInfoDescriptionFactory(
				new ImageVisualizationInfoTableRowCellFactory(nullValueRepr, IMG_VIZ_INFO_ATTRS_TO_WRITE));

		colHeaders = createColHeaders();
	}

	private void accept(final GameHistory history, final List<EventDialogue> eventDiags, final Writer writer)
			throws IOException {
		// Write header
		writer.write(colHeaders.stream().map(header -> header.stream().collect(TABLE_ROW_CELL_JOINER))
				.collect(TABLE_ROW_JOINER));

		final LocalDateTime gameStartTime = history.getStartTime();
		// The visualization info for the given game
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
			final Stream<String> imgVizInfoDesc;
			if (diagEvts.isEmpty()) {
				eventDataReprs = Stream.generate(nullValueReprSupplier).limit(eventDataToWrite.length);
				imgFeatureVectorReprs = entityFeatureVectorDescFactory.createBlankFeatureValueReprs();
				imgVizInfoDesc = imgVizInfoDescFactory.getBlankDescription().stream();
			} else {
				final Event firstDiagEvent = diagEvts.iterator().next();
				{
					final Optional<Event> streamOptLastRoundEvent = optLastRoundEvent;
					eventDataReprs = Arrays.stream(eventDataToWrite)
							.map(eventDatum -> eventDatum.apply(new EventDatum.Context(firstDiagEvent,
									streamOptLastRoundEvent, gameStartTime, nullValueReprSupplier)));
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
						final float offset = calculateDecimalSecondDifference(gameStartTime, firstDiagEvent)
								.floatValue();
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
				imgFeatureVectorReprs = entityFeatureVectorDescFactory.createFeatureValueReprs(context);

				imgVizInfoDesc = createImgVizInfoDesc(firstDiagEvent, gameStartTime, imgVizInfoData);

				optLastRoundEvent = Optional.of(diagEvts.get(diagEvts.size() - 1));
			}
			writer.write(eventDataReprs.collect(TABLE_ROW_CELL_JOINER));
			writer.write(TABLE_STRING_REPR_COL_DELIMITER);
			writer.write(imgFeatureVectorReprs.collect(TABLE_ROW_CELL_JOINER));
			writer.write(TABLE_STRING_REPR_COL_DELIMITER);
			writer.write(imgVizInfoDesc.collect(TABLE_ROW_CELL_JOINER));

			for (final LanguageDatum langDatum : langDataToWrite) {
				writer.write(TABLE_STRING_REPR_COL_DELIMITER);
				final String datumValue = langDatum.apply(diagUtts, uttDiagReprFactory);
				writer.write(datumValue);
			}
		}
	}

	private List<List<String>> createColHeaders() {
		final List<List<String>> imgViewDescColHeaders = ImageVisualizationInfoTableRowCellFactory
				.createColumnHeaders(IMG_VIZ_INFO_ATTRS_TO_WRITE);
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

	private Stream<String> createImgVizInfoDesc(final Event firstDiagEvent, final LocalDateTime gameStartTime,
			final List<ImageVisualizationInfo.Datum> imgVizInfoData) {
		final Stream<String> result;
		final Move move = (Move) firstDiagEvent.get(GameManagementEvent.Attribute.MOVE.toString());
		if (move == null) {
			result = imgVizInfoDescFactory.getBlankDescription().stream();
		} else {
			final Integer selectedPieceId = move.getPieceId();
			result = imgVizInfoDescFactory.createDescription(selectedPieceId, gameStartTime, imgVizInfoData);
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

	void accept(final SessionGame sessionGame, final Writer writer) throws IOException {
		final GameHistory history = sessionGame.getHistory();
		final List<EventDialogue> eventDiags = sessionGame.getEventDialogues();
		accept(history, eventDiags, writer);
	}

}