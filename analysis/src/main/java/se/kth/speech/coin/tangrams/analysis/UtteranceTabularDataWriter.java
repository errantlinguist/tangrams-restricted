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
import java.time.Duration;
import java.time.LocalDateTime;
import java.time.temporal.TemporalAccessor;
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

import se.kth.speech.TimestampArithmetic;
import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeature;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfoTableRowCellFactory;
import se.kth.speech.coin.tangrams.iristk.EventTimes;
import se.kth.speech.coin.tangrams.iristk.GameEvent;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;
import se.kth.speech.coin.tangrams.iristk.events.Move;

final class UtteranceTabularDataWriter {

	private enum EventDatum implements Function<UtteranceTabularDataWriter.EventDatum.Context, String> {
		LAST_RND_TIME {
			@Override
			public String apply(final EventDatum.Context ctx) {
				return ctx.optLastRoundEvent.map(GameEvent::getTime).map(TIMESTAMP_FORMATTER)
						.orElseGet(ctx.nullValueReprSupplier);
			}
		},
		LAST_RND_TIME_DIFF_SECS {
			@Override
			public String apply(final EventDatum.Context ctx) {
				return ctx.optLastRoundEvent
						.map(lastRoundEvent -> calculateDecimalSecondDifference(lastRoundEvent, ctx.firstDiagEvent))
						.map(SECS_FORMATTER).orElseGet(ctx.nullValueReprSupplier);
			}
		},
		LAST_RND_TIME_OFFSET {
			@Override
			public String apply(final EventDatum.Context ctx) {
				return ctx.optLastRoundEvent
						.map(lastRoundEvent -> createTimeDifferenceRepr(ctx.gameStartTime, lastRoundEvent))
						.orElseGet(ctx.nullValueReprSupplier);
			}
		},
		LAST_RND_TIME_OFFSET_SECS {
			@Override
			public String apply(final EventDatum.Context ctx) {
				return ctx.optLastRoundEvent
						.map(lastRoundEvent -> calculateDecimalSecondDifference(ctx.gameStartTime, lastRoundEvent))
						.map(SECS_FORMATTER).orElseGet(ctx.nullValueReprSupplier);
			}
		},
		MOVE_SUBMITTER {
			@Override
			public String apply(final EventDatum.Context ctx) {
				return (String) ctx.firstDiagEvent.getGameAttrs().get(GameManagementEvent.Attribute.PLAYER_ID);
			}
		},
		NAME {
			@Override
			public String apply(final EventDatum.Context ctx) {
				return ctx.firstDiagEvent.getName();
			}
		},
		ROUND {
			@Override
			public String apply(final EventDatum.Context ctx) {
				return Integer.toString(ctx.roundId);
			}
		},
		TIME {

			@Override
			public String apply(final EventDatum.Context ctx) {
				return TIMESTAMP_FORMATTER.apply(ctx.firstDiagEvent.getTime());
			}
		},
		TIME_OFFSET {
			@Override
			public String apply(final EventDatum.Context ctx) {
				return createTimeDifferenceRepr(ctx.gameStartTime, ctx.firstDiagEvent);
			}
		},
		TIME_OFFSET_SECS {
			@Override
			public String apply(final EventDatum.Context ctx) {
				return SECS_FORMATTER.apply(calculateDecimalSecondDifference(ctx.gameStartTime, ctx.firstDiagEvent));
			}
		};

		private static class Context {

			private final GameEvent firstDiagEvent;

			private final LocalDateTime gameStartTime;

			private final Supplier<String> nullValueReprSupplier;

			private final Optional<? extends GameEvent> optLastRoundEvent;

			private final int roundId;

			private Context(final int roundId, final GameEvent firstDiagEvent,
					final Optional<? extends GameEvent> optLastRoundEvent, final LocalDateTime gameStartTime,
					final Supplier<String> nullValueReprSupplier) {
				this.roundId = roundId;
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
				if (roundId != other.roundId) {
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
				result = prime * result + roundId;
				return result;
			}

			/*
			 * (non-Javadoc)
			 *
			 * @see java.lang.Object#toString()
			 */
			@Override
			public String toString() {
				final StringBuilder builder = new StringBuilder(128);
				builder.append("Context [firstDiagEvent=");
				builder.append(firstDiagEvent);
				builder.append(", gameStartTime=");
				builder.append(gameStartTime);
				builder.append(", nullValueReprSupplier=");
				builder.append(nullValueReprSupplier);
				builder.append(", optLastRoundEvent=");
				builder.append(optLastRoundEvent);
				builder.append(", roundId=");
				builder.append(roundId);
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

	private static final MathContext EVT_TIME_DIFF_CTX = new MathContext(16, RoundingMode.HALF_UP);

	private static final List<ImageVisualizationInfoTableRowCellFactory.Attribute> IMG_VIZ_INFO_ATTRS_TO_WRITE;

	private static final Logger LOGGER = LoggerFactory.getLogger(UtteranceTabularDataWriter.class);

	private static final Function<BigDecimal, String> SECS_FORMATTER = BigDecimal::toString;

	private static final Collector<CharSequence, ?, String> TABLE_ROW_CELL_JOINER;

	private static final Collector<CharSequence, ?, String> TABLE_ROW_JOINER;

	private static final String TABLE_STRING_REPR_COL_DELIMITER;

	private static final String TABLE_STRING_REPR_ROW_DELIMITER;

	private static final Function<TemporalAccessor, String> TIMESTAMP_FORMATTER = EventTimes.FORMATTER::format;

	static {
		TABLE_STRING_REPR_ROW_DELIMITER = System.lineSeparator();
		TABLE_ROW_JOINER = Collectors.joining(TABLE_STRING_REPR_ROW_DELIMITER);

		TABLE_STRING_REPR_COL_DELIMITER = "\t";
		TABLE_ROW_CELL_JOINER = Collectors.joining(TABLE_STRING_REPR_COL_DELIMITER);

		IMG_VIZ_INFO_ATTRS_TO_WRITE = ImageVisualizationInfoTableRowCellFactory.Attribute.getCanonicalOrdering();
	}

	private static BigDecimal calculateDecimalSecondDifference(final GameEvent firstEvt,
			final GameEvent nextEvt) {
		final LocalDateTime firstTime = firstEvt.getTime();
		return calculateDecimalSecondDifference(firstTime, nextEvt);
	}

	private static BigDecimal calculateDecimalSecondDifference(final LocalDateTime firstTime,
			final GameEvent nextEvt) {
		final LocalDateTime nextTime = nextEvt.getTime();
		return TimestampArithmetic.calculateDecimalSecondDifference(firstTime, nextTime, EVT_TIME_DIFF_CTX);
	}

	private static String createTimeDifferenceRepr(final LocalDateTime firstTime, final GameEvent nextEvt) {
		final LocalDateTime nextTime = nextEvt.getTime();
		return createTimeDifferenceRepr(firstTime, nextTime);
	}

	private static String createTimeDifferenceRepr(final LocalDateTime firstTime, final LocalDateTime nextTime) {
		final Duration duration = Duration.between(firstTime, nextTime);
		return TimestampArithmetic.formatDurationHours(duration);
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
		final List<ImageVisualizationInfo.Datum> imgVizInfoData = history.getInitialState().getImageVisualizationInfo()
				.getData();

		Optional<GameEvent> optLastRoundEvent = Optional.empty();
		int roundId = 1;
		for (final ListIterator<EventDialogue> eventDiagIter = eventDiags.listIterator(); eventDiagIter.hasNext();) {
			final EventDialogue eventDiag = eventDiagIter.next();
			writer.write(TABLE_STRING_REPR_ROW_DELIMITER);

			final List<GameEvent> diagEvts = eventDiag.getEvents();
			final List<Utterance> diagUtts = eventDiag.getUtterances();

			final Stream<String> eventDataReprs;
			final Stream<String> imgFeatureVectorReprs;
			final Stream<String> imgVizInfoDesc;
			if (diagEvts.isEmpty()) {
				eventDataReprs = Stream.generate(nullValueReprSupplier).limit(eventDataToWrite.length);
				imgFeatureVectorReprs = entityFeatureVectorDescFactory.createBlankFeatureValueReprs();
				imgVizInfoDesc = imgVizInfoDescFactory.getBlankDescription().stream();
			} else {
				final GameEvent firstDiagEvent = diagEvts.iterator().next();
				{
					final int streamRoundId = roundId;
					final Optional<GameEvent> streamOptLastRoundEvent = optLastRoundEvent;
					eventDataReprs = Arrays.stream(eventDataToWrite)
							.map(eventDatum -> eventDatum.apply(new EventDatum.Context(streamRoundId, firstDiagEvent,
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
				roundId++;
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

	private Stream<String> createImgVizInfoDesc(final GameEvent firstDiagEvent, final LocalDateTime gameStartTime,
			final List<ImageVisualizationInfo.Datum> imgVizInfoData) {
		final Stream<String> result;
		final Move move = (Move) firstDiagEvent.getGameAttrs().get(GameManagementEvent.Attribute.MOVE);
		if (move == null) {
			result = imgVizInfoDescFactory.getBlankDescription().stream();
		} else {
			final Integer selectedPieceId = move.getPieceId();
			result = imgVizInfoDescFactory.createDescription(selectedPieceId, gameStartTime, imgVizInfoData);
		}
		return result;
	}

	private String createNoEventUtterancesMsg(final GameEvent event, final List<EventDialogue> eventDiags,
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
				final GameEvent prevEvent = prevEventDiag.getFirstEvent().orElse(null);
				final List<Utterance> prevUtts = prevEventDiag.getUtterances();

				if (prevUtts.isEmpty()) {
					sb.append(String.format("Last utt before event: (NONE); event ID: \"%s\"; event time: \"%s\"",
							prevEvent.getId(), prevEvent.getTime()));
				} else {
					final Utterance prevUtt = prevUtts.get(prevUtts.size() - 1);
					final String speakingPlayerId = prevUtt.getSpeakerId();
					sb.append(String.format(
							"Last utt before event: \"%s\"; speaking player ID: \"%s\"; start: %f; end: %f; segment ID: \"%s\"; event ID: \"%s\"; event time: \"%s\"",
							prevUtt.createTokenString(), speakingPlayerId, prevUtt.getStartTime(), prevUtt.getEndTime(),
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
				final GameEvent nextEvent = nextEventDiag.getFirstEvent().orElse(null);
				final List<Utterance> nextUtts = nextEventDiag.getUtterances();
				if (nextUtts.isEmpty()) {
					LOGGER.debug("No utterances for dialogue ID \"{}\".", nextEvent.getId());
				} else {
					final Utterance nextUtt = nextUtts.get(0);
					final String speakingPlayerId = nextUtt.getSpeakerId();
					sb.append(String.format(
							"Next utt after event: \"%s\"; speaking player ID: \"%s\"; start: %f; end: %f; segment ID: \"%s\"; event ID: \"%s\"; event time: \"%s\"",
							nextUtt.createTokenString(), speakingPlayerId, nextUtt.getStartTime(), nextUtt.getEndTime(),
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