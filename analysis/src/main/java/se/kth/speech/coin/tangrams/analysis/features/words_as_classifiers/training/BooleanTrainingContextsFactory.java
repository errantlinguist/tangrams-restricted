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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training;

import java.util.ArrayList;
import java.util.List;
import java.util.function.BiFunction;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import it.unimi.dsi.fastutil.ints.IntList;
import se.kth.speech.coin.tangrams.analysis.GameContext;
import se.kth.speech.coin.tangrams.analysis.GameHistory;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeature;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeatureExtractionContextFactory;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.UtteranceGameContexts;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Jul 13, 2017
 *
 */
final class BooleanTrainingContextsFactory
		implements BiFunction<Utterance, GameHistory, BooleanTrainingContexts> {

	private static final Logger LOGGER = LoggerFactory.getLogger(BooleanTrainingContextsFactory.class);

	private final EntityFeatureExtractionContextFactory extCtxFactory;

	BooleanTrainingContextsFactory(final EntityFeatureExtractionContextFactory extCtxFactory) {
		this.extCtxFactory = extCtxFactory;
	}

	@Override
	public BooleanTrainingContexts apply(final Utterance utt, final GameHistory history) {
		final GameContext uttCtx = UtteranceGameContexts.createSingleContext(utt, history);
		final int selectedEntityId = uttCtx.findLastSelectedEntityId().getAsInt();
		LOGGER.debug("Creating positive and negative examples for entity ID \"{}\".", selectedEntityId);
		return createTrainingContexts(uttCtx, selectedEntityId);
	}

	private BooleanTrainingContexts createTrainingContexts(final GameContext uttCtx, final int selectedEntityId) {
		final IntList entityIds = uttCtx.getEntityIds();
		final List<EntityFeature.Extractor.Context> positive = new ArrayList<>(1);
		final List<EntityFeature.Extractor.Context> negative = new ArrayList<>(entityIds.size() - 1);
		for (final int entityId : uttCtx.getEntityIds()) {
			final EntityFeature.Extractor.Context context = extCtxFactory.apply(uttCtx, entityId);
			final boolean examplePolarity = entityId == selectedEntityId;
			if (examplePolarity) {
				positive.add(context);
			} else {
				negative.add(context);
			}
		}
		return new BooleanTrainingContexts(positive, negative);
	}

}
