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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues;

import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.ToDoubleFunction;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import it.unimi.dsi.fastutil.ints.Int2DoubleMap;
import it.unimi.dsi.fastutil.objects.Object2DoubleMap;
import it.unimi.dsi.fastutil.objects.Object2DoubleOpenHashMap;
import se.kth.speech.coin.tangrams.analysis.GameContext;
import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.coin.tangrams.analysis.features.ClassificationException;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.ReferentConfidenceMapFactory;
import se.kth.speech.coin.tangrams.iristk.GameEvent;
import weka.classifiers.Classifier;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Jul 9, 2017
 *
 */
public final class DialogicEventDialogueClassifier implements EventDialogueClassifier {

	private static final Logger LOGGER = LoggerFactory.getLogger(DialogicEventDialogueClassifier.class);

	private final BiFunction<? super EventDialogue, ? super GameContext, ? extends Function<? super String, ? extends Classifier>> diagWordClassifierFactory;

	private final Function<? super Collection<UtteranceRelation>, EntityReferringLanguageWordClasses> entityRefLangExFactory;

	private final ReferentConfidenceMapFactory referentConfidenceMapFactory;

	private final ToDoubleFunction<? super Utterance> uttAcceptanceRanker;

	public DialogicEventDialogueClassifier(
			final BiFunction<? super EventDialogue, ? super GameContext, ? extends Function<? super String, ? extends Classifier>> diagWordClassifierFactory,
			final ToDoubleFunction<? super Utterance> uttAcceptanceRanker,
			final Function<? super Collection<UtteranceRelation>, EntityReferringLanguageWordClasses> entityRefLangExFactory,
			final ReferentConfidenceMapFactory referentConfidenceMapFactory) {
		this.diagWordClassifierFactory = diagWordClassifierFactory;
		this.uttAcceptanceRanker = uttAcceptanceRanker;
		this.entityRefLangExFactory = entityRefLangExFactory;
		this.referentConfidenceMapFactory = referentConfidenceMapFactory;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.
	 * dialogues.
	 * EventDialogueClassifier#apply(se.kth.speech.coin.tangrams.analysis.
	 * EventDialogue, se.kth.speech.coin.tangrams.analysis.GameContext)
	 */
	@Override
	public Optional<Int2DoubleMap> apply(final EventDialogue transformedDiag, final GameContext ctx)
			throws ClassificationException {
		final Optional<Int2DoubleMap> result;
		final Optional<GameEvent> optFirstEvent = transformedDiag.getFirstEvent();
		if (optFirstEvent.isPresent()) {
			final GameEvent event = optFirstEvent.get();
			LOGGER.debug("Classifying utterances for event: {}", event);
			final List<Utterance> allUtts = transformedDiag.getUtterances();
			if (allUtts.isEmpty()) {
				LOGGER.debug("No utterances to classify for {}.", transformedDiag);
				result = Optional.empty();
			} else {
				final Function<? super String, ? extends Classifier> wordClassifierGetter = diagWordClassifierFactory
						.apply(transformedDiag, ctx);
				final DialogicEventDialogueUtteranceSorter uttSorter = new DialogicEventDialogueUtteranceSorter(
						uttAcceptanceRanker);
				final List<UtteranceRelation> uttRels = uttSorter.apply(allUtts, event);
				final EntityReferringLanguageWordClasses entityRefLangExs = entityRefLangExFactory.apply(uttRels);
				final Object2DoubleMap<String> refPosExs = entityRefLangExs.getRefPosExamples();
				final Object2DoubleMap<String> refNegExs = entityRefLangExs.getRefNegExamples();
				final Object2DoubleMap<String> refExs = new Object2DoubleOpenHashMap<>(
						refPosExs.size() + refNegExs.size());
				refPosExs.object2DoubleEntrySet().stream()
						.forEach(entry -> refExs.put(entry.getKey(), entry.getDoubleValue()));
				refNegExs.object2DoubleEntrySet().stream()
						.forEach(entry -> refExs.put(entry.getKey(), -entry.getDoubleValue()));
				result = Optional.of(referentConfidenceMapFactory.apply(refExs, ctx, wordClassifierGetter));
			}
		} else {
			result = Optional.empty();
		}
		return result;
	}

}
