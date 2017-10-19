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

import java.time.temporal.TemporalAccessor;
import java.util.Collection;
import java.util.EnumMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.stream.Stream;

import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.EventDialogueTestResults;
import se.kth.speech.coin.tangrams.iristk.EventTimes;
import se.kth.speech.coin.tangrams.iristk.GameEvent;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since May 18, 2017
 */
final class DialogueAnalysisSummaryFactory implements
		Function<DialogueAnalysisSummaryFactory.Input, Map<DialogueAnalysisSummaryFactory.SummaryDatum, Object>> {

	public static final class Input {

		private final String desc;

		private final Entry<EventDialogue, EventDialogueTestResults> diagTestResults;

		private final Integer iterNo;

		private final Object key;

		private final Integer sequenceOrder;

		public Input(final Object key, final String desc, final Integer iterNo, final Integer sequenceOrder,
				final Entry<EventDialogue, EventDialogueTestResults> diagTestResults) {
			this.key = key;
			this.desc = desc;
			this.iterNo = iterNo;
			this.sequenceOrder = sequenceOrder;
			this.diagTestResults = diagTestResults;
		}
	}

	public enum SummaryDatum implements BiFunction<Input, Function<? super Iterator<Utterance>, String>, Object> {
		DESCRIPTION {
			/*
			 * (non-Javadoc)
			 *
			 * @see java.util.function.BiFunction#apply(java.lang.Object,
			 * java.lang.Object)
			 */
			@Override
			public Object apply(final Input input,
					final Function<? super Iterator<Utterance>, String> uttDiagReprFactory) {
				return input.desc;
			}
		},
		DIALOGUE {
			/*
			 * (non-Javadoc)
			 *
			 * @see java.util.function.BiFunction#apply(java.lang.Object,
			 * java.lang.Object)
			 */
			@Override
			public Object apply(final Input input,
					final Function<? super Iterator<Utterance>, String> uttDiagReprFactory) {
				final EventDialogue diag = input.diagTestResults.getKey();
				return uttDiagReprFactory.apply(diag.getUtterances().iterator());
			}
		},
		DIALOGUE_AS_TESTED {
			/*
			 * (non-Javadoc)
			 *
			 * @see java.util.function.BiFunction#apply(java.lang.Object,
			 * java.lang.Object)
			 */
			@Override
			public Object apply(final Input input,
					final Function<? super Iterator<Utterance>, String> uttDiagReprFactory) {
				final EventDialogueTestResults testResults = input.diagTestResults.getValue();
				final Stream<Utterance> uttsTested = testResults.testedUtterances();
				return uttDiagReprFactory.apply(uttsTested.iterator());
			}

		},
		DYAD {
			/*
			 * (non-Javadoc)
			 *
			 * @see java.util.function.BiFunction#apply(java.lang.Object,
			 * java.lang.Object)
			 */
			@Override
			public Object apply(final Input input,
					final Function<? super Iterator<Utterance>, String> uttDiagReprFactory) {
				return input.key;
			}
		},

		EVENT_TIME {
			/*
			 * (non-Javadoc)
			 *
			 * @see java.util.function.BiFunction#apply(java.lang.Object,
			 * java.lang.Object)
			 */
			@Override
			public Object apply(final Input input,
					final Function<? super Iterator<Utterance>, String> uttDiagReprFactory) {
				final EventDialogue diag = input.diagTestResults.getKey();
				return diag.getFirstEvent().map(GameEvent::getTime).map(TIMESTAMP_FORMATTER).orElse("?");
			}
		},
		GOLD_STD_ID {
			/*
			 * (non-Javadoc)
			 *
			 * @see java.util.function.BiFunction#apply(java.lang.Object,
			 * java.lang.Object)
			 */
			@Override
			public Object apply(final Input input,
					final Function<? super Iterator<Utterance>, String> uttDiagReprFactory) {
				return input.diagTestResults.getValue().getGoldStandardReferentId();
			}
		},
		MEAN_DIAG_UTTS_TESTED {
			/*
			 * (non-Javadoc)
			 *
			 * @see java.util.function.BiFunction#apply(java.lang.Object,
			 * java.lang.Object)
			 */
			@Override
			public Object apply(final Input input,
					final Function<? super Iterator<Utterance>, String> uttDiagReprFactory) {
				final int tested = input.diagTestResults.getValue().testedUtteranceCount();
				final int total = input.diagTestResults.getValue().totalUtteranceCount();
				return tested / (double) total;
			}
		},
		MEAN_TOKENS_PER_UTT {
			/*
			 * (non-Javadoc)
			 *
			 * @see java.util.function.BiFunction#apply(java.lang.Object,
			 * java.lang.Object)
			 */
			@Override
			public Object apply(final Input input,
					final Function<? super Iterator<Utterance>, String> uttDiagReprFactory) {
				return input.diagTestResults.getValue().meanTokensPerTestedUtterance();
			}
		},
		RANK {
			/*
			 * (non-Javadoc)
			 *
			 * @see java.util.function.BiFunction#apply(java.lang.Object,
			 * java.lang.Object)
			 */
			@Override
			public Object apply(final Input input,
					final Function<? super Iterator<Utterance>, String> uttDiagReprFactory) {
				return input.diagTestResults.getValue().rank();
			}
		},
		RR {
			/*
			 * (non-Javadoc)
			 *
			 * @see java.util.function.BiFunction#apply(java.lang.Object,
			 * java.lang.Object)
			 */
			@Override
			public Object apply(final Input input,
					final Function<? super Iterator<Utterance>, String> uttDiagReprFactory) {
				return input.diagTestResults.getValue().reciprocalRank();
			}
		},
		SESSION_ORDER {
			/*
			 * (non-Javadoc)
			 *
			 * @see java.util.function.BiFunction#apply(java.lang.Object,
			 * java.lang.Object)
			 */
			@Override
			public Object apply(final Input input,
					final Function<? super Iterator<Utterance>, String> uttDiagReprFactory) {
				return input.sequenceOrder;
			}
		},
		TEST_ITER {
			/*
			 * (non-Javadoc)
			 *
			 * @see java.util.function.BiFunction#apply(java.lang.Object,
			 * java.lang.Object)
			 */
			@Override
			public Object apply(final Input input,
					final Function<? super Iterator<Utterance>, String> uttDiagReprFactory) {
				return input.iterNo;
			}
		},
		TESTED_UTT_COUNT {
			/*
			 * (non-Javadoc)
			 *
			 * @see java.util.function.BiFunction#apply(java.lang.Object,
			 * java.lang.Object)
			 */
			@Override
			public Object apply(final Input input,
					final Function<? super Iterator<Utterance>, String> uttDiagReprFactory) {
				return input.diagTestResults.getValue().testedUtteranceCount();
			}
		},
		TOKEN_COUNT {
			/*
			 * (non-Javadoc)
			 *
			 * @see java.util.function.BiFunction#apply(java.lang.Object,
			 * java.lang.Object)
			 */
			@Override
			public Object apply(final Input input,
					final Function<? super Iterator<Utterance>, String> uttDiagReprFactory) {
				return input.diagTestResults.getValue().testedTokenCount();
			}
		},
		TOTAL_UTT_COUNT {
			/*
			 * (non-Javadoc)
			 *
			 * @see java.util.function.BiFunction#apply(java.lang.Object,
			 * java.lang.Object)
			 */
			@Override
			public Object apply(final Input input,
					final Function<? super Iterator<Utterance>, String> uttDiagReprFactory) {
				return input.diagTestResults.getValue().totalUtteranceCount();
			}
		};
	}

	private static final Function<TemporalAccessor, String> TIMESTAMP_FORMATTER = EventTimes.FORMATTER::format;

	private final Collection<SummaryDatum> dataToCreate;

	private final Function<? super Iterator<Utterance>, String> uttDiagReprFactory;

	public DialogueAnalysisSummaryFactory(final Function<? super Iterator<Utterance>, String> uttDiagReprFactory,
			final Collection<SummaryDatum> dataToCreate) {
		this.uttDiagReprFactory = uttDiagReprFactory;
		this.dataToCreate = dataToCreate;
	}

	@Override
	public Map<SummaryDatum, Object> apply(final Input input) {
		final Map<SummaryDatum, Object> result = new EnumMap<>(SummaryDatum.class);
		for (final SummaryDatum datum : dataToCreate) {
			final Object val = datum.apply(input, uttDiagReprFactory);
			result.put(datum, val);
		}
		assert result.size() == dataToCreate.size();
		return result;
	}

}
