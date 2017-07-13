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

import java.lang.ref.SoftReference;
import java.util.Map.Entry;
import java.util.function.Function;

import org.springframework.context.ApplicationContext;

import edu.stanford.nlp.pipeline.StanfordCoreNLP;
import se.kth.speech.MutablePair;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeatureExtractionContextFactory;
import se.kth.speech.coin.tangrams.analysis.features.weka.EntityInstanceAttributeContext;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.ReferentConfidenceMapFactory;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.EventDialogueClassifier;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.SentimentAnalyzingEventDialogueClassifier;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.OnePositiveMaximumNegativeInstancesFactory;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.OnePositiveOneNegativeInstanceFactory;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.SentimentAnalyzingInstancesFactory;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.TrainingInstancesFactory;
import se.kth.speech.nlp.stanford.CachingUtteranceSentimentRanker;
import se.kth.speech.nlp.stanford.StanfordCoreNLPConfigurationVariant;

enum Training {
	ALL_NEG {

		private final Function<TrainingContext, Entry<TrainingInstancesFactory, Integer>> trainingInstsFactoryFactory = trainingCtx -> {
			final ApplicationContext appCtx = trainingCtx.getAppCtx();
			final EntityInstanceAttributeContext entityInstAttrCtx = appCtx
					.getBean(EntityInstanceAttributeContext.class);
			final EntityFeatureExtractionContextFactory extCtxFactory = appCtx
					.getBean(EntityFeatureExtractionContextFactory.class);
			final OnePositiveMaximumNegativeInstancesFactory instsFactory = new OnePositiveMaximumNegativeInstancesFactory(
					entityInstAttrCtx, trainingCtx.getDiagTransformer(), extCtxFactory);
			return new MutablePair<>(instsFactory, 1);
		};

		@Override
		public Function<ReferentConfidenceMapFactory, EventDialogueClassifier> getClassifierFactory() {
			return TrainingConstants.SIMPLE_CLASSIFIER_FACTORY;
		}

		@Override
		public Function<TrainingContext, Entry<TrainingInstancesFactory, Integer>> getTrainingInstsFactoryFactory() {
			return trainingInstsFactoryFactory;
		}
	},
	ONE_NEG {

		private final Function<TrainingContext, Entry<TrainingInstancesFactory, Integer>> trainingInstsFactoryFactory = trainingCtx -> {
			final ApplicationContext appCtx = trainingCtx.getAppCtx();
			final EntityInstanceAttributeContext entityInstAttrCtx = appCtx
					.getBean(EntityInstanceAttributeContext.class);
			final EntityFeatureExtractionContextFactory extCtxFactory = appCtx
					.getBean(EntityFeatureExtractionContextFactory.class);
			final OnePositiveOneNegativeInstanceFactory instsFactory = new OnePositiveOneNegativeInstanceFactory(
					entityInstAttrCtx, trainingCtx.getDiagTransformer(), extCtxFactory, TrainingConstants.RND);
			return new MutablePair<>(instsFactory, 5);
		};

		@Override
		public Function<ReferentConfidenceMapFactory, EventDialogueClassifier> getClassifierFactory() {
			return TrainingConstants.SIMPLE_CLASSIFIER_FACTORY;
		}

		@Override
		public Function<TrainingContext, Entry<TrainingInstancesFactory, Integer>> getTrainingInstsFactoryFactory() {
			return trainingInstsFactoryFactory;
		}
	},
	SENTIMENT {

		private final Function<TrainingContext, Entry<TrainingInstancesFactory, Integer>> trainingInstsFactoryFactory = trainingCtx -> {
			final ApplicationContext appCtx = trainingCtx.getAppCtx();
			final EntityInstanceAttributeContext entityInstAttrCtx = appCtx
					.getBean(EntityInstanceAttributeContext.class);
			final EntityFeatureExtractionContextFactory extCtxFactory = appCtx
					.getBean(EntityFeatureExtractionContextFactory.class);
			final SentimentAnalyzingInstancesFactory instsFactory = new SentimentAnalyzingInstancesFactory(
					entityInstAttrCtx, trainingCtx.getDiagTransformer(), extCtxFactory,
					fetchUttSentimentRanker());
			return new MutablePair<>(instsFactory, 1);
		};

		private SoftReference<CachingUtteranceSentimentRanker> uttSentimentRanker = new SoftReference<>(null);

		@Override
		public Function<ReferentConfidenceMapFactory, EventDialogueClassifier> getClassifierFactory() {
			return (refConfMapFactory) -> new SentimentAnalyzingEventDialogueClassifier(fetchUttSentimentRanker(),
					refConfMapFactory);
		}

		@Override
		public Function<TrainingContext, Entry<TrainingInstancesFactory, Integer>> getTrainingInstsFactoryFactory() {
			return trainingInstsFactoryFactory;
		}

		private CachingUtteranceSentimentRanker createUttSentimentRanker() {
			final StanfordCoreNLP pipeline = StanfordCoreNLPConfigurationVariant.TOKENIZING_PARSING_SENTIMENT.get();
			return new CachingUtteranceSentimentRanker(pipeline, TrainingConstants.ESTIMATED_UNIQUE_UTT_COUNT);
		}

		private CachingUtteranceSentimentRanker fetchUttSentimentRanker() {
			CachingUtteranceSentimentRanker result = uttSentimentRanker.get();
			if (result == null) {
				synchronized (SENTIMENT) {
					result = uttSentimentRanker.get();
					if (result == null) {
						result = createUttSentimentRanker();
						uttSentimentRanker = new SoftReference<>(result);
					}
				}
			}
			return result;
		}
	};

	/**
	 * @return the classifierFactory
	 */
	public abstract Function<ReferentConfidenceMapFactory, EventDialogueClassifier> getClassifierFactory();

	/**
	 * @return the trainingMethod
	 */
	public abstract Function<TrainingContext, Entry<TrainingInstancesFactory, Integer>> getTrainingInstsFactoryFactory();

}