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
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.ToDoubleFunction;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import it.unimi.dsi.fastutil.objects.Object2DoubleMap;
import it.unimi.dsi.fastutil.objects.Object2DoubleOpenHashMap;
import se.kth.speech.coin.tangrams.analysis.GameContext;
import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.coin.tangrams.analysis.features.ClassificationException;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.ReferentConfidenceData;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.ReferentConfidenceMapFactory;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.EventDialogueContextWordClassifierTrainer;
import se.kth.speech.coin.tangrams.iristk.GameEvent;
import weka.classifiers.Classifier;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Jul 9, 2017
 *
 */
public final class DialogicEventDialogueClassifier implements EventDialogueClassifier {

	private static final Logger LOGGER = LoggerFactory.getLogger(DialogicEventDialogueClassifier.class);

	private static final Optional<ReferentConfidenceData> NULL_RESULT = Optional.empty();

	private final EventDialogueContextWordClassifierTrainer<?> diagWordClassifierFactory;

	private final Function<? super Collection<UtteranceRelation>, EntityReferringLanguageWordClasses> entityRefLangExFactory;

	private final ReferentConfidenceMapFactory referentConfidenceMapFactory;

	private final ToDoubleFunction<? super Utterance> uttAcceptanceRanker;

	public DialogicEventDialogueClassifier(final EventDialogueContextWordClassifierTrainer<?> diagWordClassifierFactory,
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
	public Optional<ReferentConfidenceData> apply(final EventDialogue diag, final GameContext ctx)
			throws ClassificationException {
		final Optional<ReferentConfidenceData> result;
		final Optional<GameEvent> optFirstEvent = diag.getFirstEvent();
		if (optFirstEvent.isPresent()) {
			final GameEvent event = optFirstEvent.get();
			LOGGER.debug("Classifying utterances for event: {}", event);
			final List<Utterance> allUtts = diag.getUtterances();
			if (allUtts.isEmpty()) {
				LOGGER.debug("No utterances to classify for {}.", diag);
				result = NULL_RESULT;
			} else {
				final Map<String, ? extends Classifier> wordClassifiers = diagWordClassifierFactory
						.apply(diag, ctx);
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
				result = referentConfidenceMapFactory.apply(refExs, ctx, wordClassifiers);
			}
		} else {
			result = NULL_RESULT;
		}
		return result;
	}

}
