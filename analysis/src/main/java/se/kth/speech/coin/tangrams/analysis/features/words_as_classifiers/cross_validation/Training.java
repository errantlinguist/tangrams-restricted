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

import java.util.Map.Entry;
import java.util.Random;
import java.util.function.Function;

import org.springframework.context.ApplicationContext;

import se.kth.speech.MutablePair;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeatureExtractionContextFactory;
import se.kth.speech.coin.tangrams.analysis.features.weka.EntityInstanceAttributeContext;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.OnePositiveMaximumNegativeInstancesFactory;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.OnePositiveOneNegativeInstanceFactory;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.SentimentAnalyzingInstancesFactory;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.TrainingInstancesFactory;
import se.kth.speech.nlp.stanford.StanfordCoreNLPConfigurationVariant;

enum Training implements Function<TrainingContext, Entry<TrainingInstancesFactory, Integer>> {
	ALL_NEG {
		@Override
		public Entry<TrainingInstancesFactory, Integer> apply(final TrainingContext trainingCtx) {
			final ApplicationContext appCtx = trainingCtx.getAppCtx();
			final EntityInstanceAttributeContext entityInstAttrCtx = appCtx
					.getBean(EntityInstanceAttributeContext.class);
			final EntityFeatureExtractionContextFactory extCtxFactory = appCtx
					.getBean(EntityFeatureExtractionContextFactory.class);
			final OnePositiveMaximumNegativeInstancesFactory instsFactory = new OnePositiveMaximumNegativeInstancesFactory(
					entityInstAttrCtx, trainingCtx.getDiagTransformer(), extCtxFactory);
			return new MutablePair<>(instsFactory, 1);
		}
	},
	ONE_NEG {

		@Override
		public Entry<TrainingInstancesFactory, Integer> apply(final TrainingContext trainingCtx) {
			final ApplicationContext appCtx = trainingCtx.getAppCtx();
			final EntityInstanceAttributeContext entityInstAttrCtx = appCtx
					.getBean(EntityInstanceAttributeContext.class);
			final EntityFeatureExtractionContextFactory extCtxFactory = appCtx
					.getBean(EntityFeatureExtractionContextFactory.class);
			final OnePositiveOneNegativeInstanceFactory instsFactory = new OnePositiveOneNegativeInstanceFactory(
					entityInstAttrCtx, trainingCtx.getDiagTransformer(), extCtxFactory, RND);
			return new MutablePair<>(instsFactory, 5);
		}

	},
	SENTIMENT {
		@Override
		public Entry<TrainingInstancesFactory, Integer> apply(final TrainingContext trainingCtx) {
			final ApplicationContext appCtx = trainingCtx.getAppCtx();
			final EntityInstanceAttributeContext entityInstAttrCtx = appCtx
					.getBean(EntityInstanceAttributeContext.class);
			final EntityFeatureExtractionContextFactory extCtxFactory = appCtx
					.getBean(EntityFeatureExtractionContextFactory.class);
			final SentimentAnalyzingInstancesFactory instsFactory = new SentimentAnalyzingInstancesFactory(
					entityInstAttrCtx, trainingCtx.getDiagTransformer(), extCtxFactory,
					StanfordCoreNLPConfigurationVariant.TOKENIZING_PARSING_SENTIMENT
							.get(),
					ESTIMATED_UNIQUE_UTT_COUNT);
			return new MutablePair<>(instsFactory, 1);
		}
	};

	private static final int ESTIMATED_UNIQUE_UTT_COUNT = 2000;

	private static final Random RND = new Random(1);

}