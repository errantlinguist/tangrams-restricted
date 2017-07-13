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
import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import org.springframework.context.ApplicationContext;

import edu.stanford.nlp.pipeline.StanfordCoreNLP;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeatureExtractionContextFactory;
import se.kth.speech.coin.tangrams.analysis.features.weka.EntityInstanceAttributeContext;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.ReferentConfidenceMapFactory;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.CachingEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.ChainedEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.EventDialogueClassifier;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.EventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.InstructorUtteranceFilteringEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.SentimentAnalyzingEventDialogueClassifier;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.OnePositiveMaximumNegativeInstancesFactory;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.OnePositiveOneNegativeInstanceFactory;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.SentimentAnalyzingInstancesFactory;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.TrainingInstancesFactory;
import se.kth.speech.nlp.stanford.CachingUtteranceSentimentRanker;
import se.kth.speech.nlp.stanford.StanfordCoreNLPConfigurationVariant;

enum Training {
	ALL_NEG(1) {

		@Override
		public CachingEventDialogueTransformer createSymmetricalTrainingTestingEvgDiagTransformer(
				final List<EventDialogueTransformer> diagTransformers) {
			return createInstrUttFilteringTransformer(diagTransformers);
		}

		@Override
		public TrainingInstancesFactory createTrainingInstsFactoryIterCount(final TrainingContext trainingCtx) {
			final ApplicationContext appCtx = trainingCtx.getAppCtx();
			final EntityInstanceAttributeContext entityInstAttrCtx = appCtx
					.getBean(EntityInstanceAttributeContext.class);
			final EntityFeatureExtractionContextFactory extCtxFactory = appCtx
					.getBean(EntityFeatureExtractionContextFactory.class);
			return new OnePositiveMaximumNegativeInstancesFactory(entityInstAttrCtx, trainingCtx.getDiagTransformer(),
					extCtxFactory);
		}

		@Override
		public Function<ReferentConfidenceMapFactory, EventDialogueClassifier> getClassifierFactory() {
			return TrainingConstants.SIMPLE_CLASSIFIER_FACTORY;
		}
	},
	DIALOGIC(5) {

		private SoftReference<CachingUtteranceSentimentRanker> uttSentimentRanker = new SoftReference<>(null);

		@Override
		public CachingEventDialogueTransformer createSymmetricalTrainingTestingEvgDiagTransformer(
				final List<EventDialogueTransformer> diagTransformers) {
			final ChainedEventDialogueTransformer chainedTransformer = new ChainedEventDialogueTransformer(
					diagTransformers);
			return new CachingEventDialogueTransformer(chainedTransformer);
		}

		@Override
		public TrainingInstancesFactory createTrainingInstsFactoryIterCount(final TrainingContext trainingCtx) {
			final ApplicationContext appCtx = trainingCtx.getAppCtx();
			final EntityInstanceAttributeContext entityInstAttrCtx = appCtx
					.getBean(EntityInstanceAttributeContext.class);
			final EntityFeatureExtractionContextFactory extCtxFactory = appCtx
					.getBean(EntityFeatureExtractionContextFactory.class);
			return new SentimentAnalyzingInstancesFactory(entityInstAttrCtx, trainingCtx.getDiagTransformer(),
					extCtxFactory, fetchUttSentimentRanker());
		}

		@Override
		public Function<ReferentConfidenceMapFactory, EventDialogueClassifier> getClassifierFactory() {
			return (refConfMapFactory) -> new SentimentAnalyzingEventDialogueClassifier(fetchUttSentimentRanker(),
					refConfMapFactory);
		}

		private CachingUtteranceSentimentRanker createUttSentimentRanker() {
			final StanfordCoreNLP pipeline = StanfordCoreNLPConfigurationVariant.TOKENIZING_PARSING_SENTIMENT.get();
			return new CachingUtteranceSentimentRanker(pipeline, TrainingConstants.ESTIMATED_UNIQUE_UTT_COUNT);
		}

		private CachingUtteranceSentimentRanker fetchUttSentimentRanker() {
			CachingUtteranceSentimentRanker result = uttSentimentRanker.get();
			if (result == null) {
				synchronized (DIALOGIC) {
					result = uttSentimentRanker.get();
					if (result == null) {
						result = createUttSentimentRanker();
						uttSentimentRanker = new SoftReference<>(result);
					}
				}
			}
			return result;
		}
	},
	ONE_NEG(1) {

		@Override
		public CachingEventDialogueTransformer createSymmetricalTrainingTestingEvgDiagTransformer(
				final List<EventDialogueTransformer> diagTransformers) {
			return createInstrUttFilteringTransformer(diagTransformers);
		}

		@Override
		public TrainingInstancesFactory createTrainingInstsFactoryIterCount(final TrainingContext trainingCtx) {
			final ApplicationContext appCtx = trainingCtx.getAppCtx();
			final EntityInstanceAttributeContext entityInstAttrCtx = appCtx
					.getBean(EntityInstanceAttributeContext.class);
			final EntityFeatureExtractionContextFactory extCtxFactory = appCtx
					.getBean(EntityFeatureExtractionContextFactory.class);
			return new OnePositiveOneNegativeInstanceFactory(entityInstAttrCtx, trainingCtx.getDiagTransformer(),
					extCtxFactory, TrainingConstants.RND);
		}

		@Override
		public Function<ReferentConfidenceMapFactory, EventDialogueClassifier> getClassifierFactory() {
			return TrainingConstants.SIMPLE_CLASSIFIER_FACTORY;
		}
	};

	private static final InstructorUtteranceFilteringEventDialogueTransformer INSTR_UTT_FILTER = new InstructorUtteranceFilteringEventDialogueTransformer();

	private static CachingEventDialogueTransformer createInstrUttFilteringTransformer(
			final List<EventDialogueTransformer> diagTransformers) {
		final List<EventDialogueTransformer> chain = new ArrayList<>(diagTransformers.size() + 1);
		chain.add(INSTR_UTT_FILTER);
		chain.addAll(diagTransformers);
		final ChainedEventDialogueTransformer chainedTransformer = new ChainedEventDialogueTransformer(chain);
		return new CachingEventDialogueTransformer(chainedTransformer);
	}

	private int iterCount;

	private Training(final int iterCount) {
		this.iterCount = iterCount;
	}

	public abstract CachingEventDialogueTransformer createSymmetricalTrainingTestingEvgDiagTransformer(
			final List<EventDialogueTransformer> diagTransformers);

	public abstract TrainingInstancesFactory createTrainingInstsFactoryIterCount(TrainingContext trainingCtx);

	/**
	 * @return the classifierFactory
	 */
	public abstract Function<ReferentConfidenceMapFactory, EventDialogueClassifier> getClassifierFactory();

	public int getIterCount() {
		return iterCount;
	}

}