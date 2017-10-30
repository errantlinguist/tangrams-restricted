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

import java.lang.ref.Reference;
import java.lang.ref.SoftReference;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.function.Function;
import java.util.function.ToDoubleFunction;

import org.springframework.context.ApplicationContext;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;

import it.unimi.dsi.fastutil.objects.Object2DoubleMap;
import it.unimi.dsi.fastutil.objects.Object2DoubleOpenHashMap;
import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeatureExtractionContextFactory;
import se.kth.speech.coin.tangrams.analysis.features.weka.EntityInstanceAttributeContext;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.WordClassDiscountingSmoother;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues.CachingEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues.ChainedEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues.ClassificationContext;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues.DialogicEventDialogueClassifier;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues.DialogicWeightedWordClassFactory;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues.EventDialogueClassifier;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues.EventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues.InstructorUtteranceFilteringEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues.IsolatedUtteranceEventDialogueClassifier;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.AbstractInstanceExtractor;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.DialogicInstanceExtractor;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.IterativeWordLogisticClassifierTrainer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.OnePositiveMaximumNegativeInstanceExtractor;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.OnePositiveOneNegativeInstanceExtractor;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.ParallelizedWordLogisticClassifierTrainer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.SizeEstimatingInstancesMapFactory;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.TrainingInstancesFactory;
import se.kth.speech.nlp.PatternMatchingUtteranceAcceptanceRanker;
import weka.classifiers.functions.Logistic;

enum Training {
	ALL_NEG(1) {

		@Override
		public CachingEventDialogueTransformer createSymmetricalTrainingTestingEvgDiagTransformer(
				final List<EventDialogueTransformer> diagTransformers) {
			return createInstrUttFilteringTransformer(diagTransformers);
		}

		@Override
		public TrainingInstancesFactory createTrainingInstsFactory(final TrainingContext trainingCtx) {
			final ApplicationContext appCtx = trainingCtx.getAppCtx();
			final EntityInstanceAttributeContext entityInstAttrCtx = appCtx
					.getBean(EntityInstanceAttributeContext.class);
			final EntityFeatureExtractionContextFactory extCtxFactory = appCtx
					.getBean(EntityFeatureExtractionContextFactory.class);
			final AbstractInstanceExtractor instExtractor = new OnePositiveMaximumNegativeInstanceExtractor(
					entityInstAttrCtx, trainingCtx.getDiagTransformer(), extCtxFactory);
			final Map<WordClassifierTrainingParameter, Object> trainingParams = trainingCtx.getTrainingParams();
			return new SizeEstimatingInstancesMapFactory(instExtractor, entityInstAttrCtx,
					(Double) trainingParams
							.get(WordClassifierTrainingParameter.BACKGROUND_DATA_POSITIVE_EXAMPLE_WEIGHT_FACTOR),
					(Double) trainingParams
							.get(WordClassifierTrainingParameter.BACKGROUND_DATA_NEGATIVE_EXAMPLE_WEIGHT_FACTOR));
		}

		@Override
		public Function<ClassificationContext, EventDialogueClassifier> getClassifierFactory(
				final TrainingContext trainingCtx) {
			return createSimpleClassifierFactory(trainingCtx);
		}
	},
	ALL_NEG_ITERATIVE(1) {

		@Override
		public CachingEventDialogueTransformer createSymmetricalTrainingTestingEvgDiagTransformer(
				final List<EventDialogueTransformer> diagTransformers) {
			return createInstrUttFilteringTransformer(diagTransformers);
		}

		@Override
		public TrainingInstancesFactory createTrainingInstsFactory(final TrainingContext trainingCtx) {
			final ApplicationContext appCtx = trainingCtx.getAppCtx();
			final EntityInstanceAttributeContext entityInstAttrCtx = appCtx
					.getBean(EntityInstanceAttributeContext.class);
			final EntityFeatureExtractionContextFactory extCtxFactory = appCtx
					.getBean(EntityFeatureExtractionContextFactory.class);
			final Map<WordClassifierTrainingParameter, Object> trainingParams = trainingCtx.getTrainingParams();
			return new SizeEstimatingInstancesMapFactory(
					new OnePositiveMaximumNegativeInstanceExtractor(entityInstAttrCtx, trainingCtx.getDiagTransformer(),
							extCtxFactory),
					entityInstAttrCtx,
					(Double) trainingParams
							.get(WordClassifierTrainingParameter.BACKGROUND_DATA_POSITIVE_EXAMPLE_WEIGHT_FACTOR),
					(Double) trainingParams
							.get(WordClassifierTrainingParameter.BACKGROUND_DATA_NEGATIVE_EXAMPLE_WEIGHT_FACTOR));
		}

		@Override
		public Function<ClassificationContext, EventDialogueClassifier> getClassifierFactory(
				final TrainingContext trainingCtx) {
			return createSimpleIterativeClassifierFactory(trainingCtx);
		}
	},
	DIALOGIC(1) {

		private final Map<TrainingContext, Reference<ToDoubleFunction<Utterance>>> ctxAcceptanceRankers = new ConcurrentHashMap<>(
				2);

		private final ConcurrentMap<TrainingContext, Reference<DialogicWeightedWordClassFactory>> ctxWordClassFactories = new ConcurrentHashMap<>(
				2);

		@Override
		public CachingEventDialogueTransformer createSymmetricalTrainingTestingEvgDiagTransformer(
				final List<EventDialogueTransformer> diagTransformers) {
			final ChainedEventDialogueTransformer chainedTransformer = new ChainedEventDialogueTransformer(
					diagTransformers);
			return new CachingEventDialogueTransformer(createTransformedDialogueCache(chainedTransformer));
		}

		@Override
		public TrainingInstancesFactory createTrainingInstsFactory(final TrainingContext trainingCtx) {
			final ApplicationContext appCtx = trainingCtx.getAppCtx();
			final EntityInstanceAttributeContext entityInstAttrCtx = appCtx
					.getBean(EntityInstanceAttributeContext.class);
			final EntityFeatureExtractionContextFactory extCtxFactory = appCtx
					.getBean(EntityFeatureExtractionContextFactory.class);
			final Map<WordClassifierTrainingParameter, Object> trainingParams = trainingCtx.getTrainingParams();
			return new SizeEstimatingInstancesMapFactory(
					new DialogicInstanceExtractor(entityInstAttrCtx, trainingCtx.getDiagTransformer(), extCtxFactory,
							fetchCachingUttAcceptanceRanker(trainingCtx), fetchWordClassFactory(trainingCtx),
							trainingCtx.getUttRelHandler()),
					entityInstAttrCtx,
					(Double) trainingParams
							.get(WordClassifierTrainingParameter.BACKGROUND_DATA_POSITIVE_EXAMPLE_WEIGHT_FACTOR),
					(Double) trainingParams
							.get(WordClassifierTrainingParameter.BACKGROUND_DATA_NEGATIVE_EXAMPLE_WEIGHT_FACTOR));
		}

		@Override
		public Function<ClassificationContext, EventDialogueClassifier> getClassifierFactory(
				final TrainingContext trainingCtx) {
			final ApplicationContext appCtx = trainingCtx.getAppCtx();
			final WordClassDiscountingSmoother smoother = appCtx.getBean(WordClassDiscountingSmoother.class);
			return classificationContext -> {
				final ParallelizedWordLogisticClassifierTrainer trainer = new ParallelizedWordLogisticClassifierTrainer(
						classificationContext.getBackgroundJobExecutor(), smoother);
				final Function<String, Logistic> wordClassifiers = trainer
						.apply(classificationContext.getTrainingData())::get;
				// This classifier is statically-trained, i.e. the word models
				// used for classification are the same no matter what dialogue
				// is being classified
				return new DialogicEventDialogueClassifier((diagToClassify, ctx) -> wordClassifiers,
						fetchCachingUttAcceptanceRanker(trainingCtx), fetchWordClassFactory(trainingCtx),
						classificationContext.getReferentConfidenceMapFactory());
			};
		};

		private ToDoubleFunction<Utterance> fetchCachingUttAcceptanceRanker(final TrainingContext trainingCtx) {
			return fetchCachingUttAcceptanceRanker(ctxAcceptanceRankers, trainingCtx);
		}

		private DialogicWeightedWordClassFactory fetchWordClassFactory(final TrainingContext trainingCtx) {
			return fetchWordClassFactory(ctxWordClassFactories, trainingCtx);
		}

	},
	DIALOGIC_ITERATIVE(1) {

		private final Map<TrainingContext, Reference<ToDoubleFunction<Utterance>>> ctxAcceptanceRankers = new ConcurrentHashMap<>(
				2);

		private final ConcurrentMap<TrainingContext, Reference<DialogicWeightedWordClassFactory>> ctxWordClassFactories = new ConcurrentHashMap<>(
				2);

		@Override
		public CachingEventDialogueTransformer createSymmetricalTrainingTestingEvgDiagTransformer(
				final List<EventDialogueTransformer> diagTransformers) {
			final ChainedEventDialogueTransformer chainedTransformer = new ChainedEventDialogueTransformer(
					diagTransformers);
			return new CachingEventDialogueTransformer(createTransformedDialogueCache(chainedTransformer));
		}

		@Override
		public TrainingInstancesFactory createTrainingInstsFactory(final TrainingContext trainingCtx) {
			final ApplicationContext appCtx = trainingCtx.getAppCtx();
			final EntityInstanceAttributeContext entityInstAttrCtx = appCtx
					.getBean(EntityInstanceAttributeContext.class);
			final EntityFeatureExtractionContextFactory extCtxFactory = appCtx
					.getBean(EntityFeatureExtractionContextFactory.class);
			final Map<WordClassifierTrainingParameter, Object> trainingParams = trainingCtx.getTrainingParams();
			return new SizeEstimatingInstancesMapFactory(
					new DialogicInstanceExtractor(entityInstAttrCtx, trainingCtx.getDiagTransformer(), extCtxFactory,
							fetchCachingUttAcceptanceRanker(trainingCtx), fetchWordClassFactory(trainingCtx),
							trainingCtx.getUttRelHandler()),
					entityInstAttrCtx,
					(Double) trainingParams
							.get(WordClassifierTrainingParameter.BACKGROUND_DATA_POSITIVE_EXAMPLE_WEIGHT_FACTOR),
					(Double) trainingParams
							.get(WordClassifierTrainingParameter.BACKGROUND_DATA_NEGATIVE_EXAMPLE_WEIGHT_FACTOR));
		}

		@Override
		public Function<ClassificationContext, EventDialogueClassifier> getClassifierFactory(
				final TrainingContext trainingCtx) {
			final ApplicationContext appCtx = trainingCtx.getAppCtx();
			final WordClassDiscountingSmoother smoother = appCtx.getBean(WordClassDiscountingSmoother.class);
			final EntityInstanceAttributeContext entityInstAttrCtx = appCtx
					.getBean(EntityInstanceAttributeContext.class);
			final EntityFeatureExtractionContextFactory extCtxFactory = appCtx
					.getBean(EntityFeatureExtractionContextFactory.class);
			return classificationContext -> {
				final ParallelizedWordLogisticClassifierTrainer trainer = new ParallelizedWordLogisticClassifierTrainer(
						classificationContext.getBackgroundJobExecutor(), smoother);
				final AbstractInstanceExtractor instExtractor = new OnePositiveMaximumNegativeInstanceExtractor(
						entityInstAttrCtx, trainingCtx.getDiagTransformer(), extCtxFactory);
				final Map<WordClassifierTrainingParameter, Object> trainingParams = trainingCtx.getTrainingParams();
				final IterativeWordLogisticClassifierTrainer<Logistic> iterativeTrainer = new IterativeWordLogisticClassifierTrainer<>(
						trainer, classificationContext.getTrainingData(), instExtractor,
						(Double) trainingParams
								.get(WordClassifierTrainingParameter.INTERACTION_DATA_POSITIVE_EXAMPLE_WEIGHT_FACTOR),
						(Double) trainingParams
								.get(WordClassifierTrainingParameter.INTERACTION_DATA_NEGATIVE_EXAMPLE_WEIGHT_FACTOR));
				return new DialogicEventDialogueClassifier(iterativeTrainer,
						fetchCachingUttAcceptanceRanker(trainingCtx), fetchWordClassFactory(trainingCtx),
						classificationContext.getReferentConfidenceMapFactory());
			};
		};

		private ToDoubleFunction<Utterance> fetchCachingUttAcceptanceRanker(final TrainingContext trainingCtx) {
			return fetchCachingUttAcceptanceRanker(ctxAcceptanceRankers, trainingCtx);
		}

		private DialogicWeightedWordClassFactory fetchWordClassFactory(final TrainingContext trainingCtx) {
			return fetchWordClassFactory(ctxWordClassFactories, trainingCtx);
		}

	},
	ONE_NEG(5) {

		@Override
		public CachingEventDialogueTransformer createSymmetricalTrainingTestingEvgDiagTransformer(
				final List<EventDialogueTransformer> diagTransformers) {
			return createInstrUttFilteringTransformer(diagTransformers);
		}

		@Override
		public TrainingInstancesFactory createTrainingInstsFactory(final TrainingContext trainingCtx) {
			final ApplicationContext appCtx = trainingCtx.getAppCtx();
			final EntityInstanceAttributeContext entityInstAttrCtx = appCtx
					.getBean(EntityInstanceAttributeContext.class);
			final EntityFeatureExtractionContextFactory extCtxFactory = appCtx
					.getBean(EntityFeatureExtractionContextFactory.class);
			final Map<WordClassifierTrainingParameter, Object> trainingParams = trainingCtx.getTrainingParams();
			return new SizeEstimatingInstancesMapFactory(
					new OnePositiveOneNegativeInstanceExtractor(entityInstAttrCtx, trainingCtx.getDiagTransformer(),
							extCtxFactory, TrainingConstants.RND),
					entityInstAttrCtx,
					(Double) trainingParams
							.get(WordClassifierTrainingParameter.BACKGROUND_DATA_POSITIVE_EXAMPLE_WEIGHT_FACTOR),
					(Double) trainingParams
							.get(WordClassifierTrainingParameter.BACKGROUND_DATA_NEGATIVE_EXAMPLE_WEIGHT_FACTOR));
		}

		@Override
		public Function<ClassificationContext, EventDialogueClassifier> getClassifierFactory(
				final TrainingContext trainingCtx) {
			return createSimpleClassifierFactory(trainingCtx);
		}
	},
	ONE_NEG_ITERATIVE(5) {

		@Override
		public CachingEventDialogueTransformer createSymmetricalTrainingTestingEvgDiagTransformer(
				final List<EventDialogueTransformer> diagTransformers) {
			return createInstrUttFilteringTransformer(diagTransformers);
		}

		@Override
		public TrainingInstancesFactory createTrainingInstsFactory(final TrainingContext trainingCtx) {
			final ApplicationContext appCtx = trainingCtx.getAppCtx();
			final EntityInstanceAttributeContext entityInstAttrCtx = appCtx
					.getBean(EntityInstanceAttributeContext.class);
			final EntityFeatureExtractionContextFactory extCtxFactory = appCtx
					.getBean(EntityFeatureExtractionContextFactory.class);
			final Map<WordClassifierTrainingParameter, Object> trainingParams = trainingCtx.getTrainingParams();
			return new SizeEstimatingInstancesMapFactory(
					new OnePositiveOneNegativeInstanceExtractor(entityInstAttrCtx, trainingCtx.getDiagTransformer(),
							extCtxFactory, TrainingConstants.RND),
					entityInstAttrCtx,
					(Double) trainingParams
							.get(WordClassifierTrainingParameter.BACKGROUND_DATA_POSITIVE_EXAMPLE_WEIGHT_FACTOR),
					(Double) trainingParams
							.get(WordClassifierTrainingParameter.BACKGROUND_DATA_NEGATIVE_EXAMPLE_WEIGHT_FACTOR));
		}

		@Override
		public Function<ClassificationContext, EventDialogueClassifier> getClassifierFactory(
				final TrainingContext trainingCtx) {
			return createSimpleIterativeClassifierFactory(trainingCtx);
		}

	};

	private static final int ESTIMATED_MIN_SESSION_DIALOGUE_COUNT = 50;

	private static final int EVENT_DIALOGUE_PROCESSING_CONCURRENCY = 1;

	private static final InstructorUtteranceFilteringEventDialogueTransformer INSTR_UTT_FILTER = new InstructorUtteranceFilteringEventDialogueTransformer();

	private static final long MAXIMUM_TRANSFORMED_DIAG_CACHE_SIZE = 1000;

	private static ToDoubleFunction<Utterance> createCachingUttAcceptanceRanker(
			final Map<WordClassifierTrainingParameter, Object> trainingParams) {
		final Object2DoubleMap<Utterance> cache = new Object2DoubleOpenHashMap<>(
				(Integer) trainingParams.get(WordClassifierTrainingParameter.EXPECTED_UNIQUE_UTTERANCE_COUNT));
		cache.defaultReturnValue(Double.NaN);
		final PatternMatchingUtteranceAcceptanceRanker ranker = new PatternMatchingUtteranceAcceptanceRanker();
		return utt -> {
			double result = cache.getDouble(utt);
			if (Double.isNaN(result)) {
				synchronized (cache) {
					result = cache.getDouble(utt);
					if (Double.isNaN(result)) {
						result = ranker.applyAsDouble(utt);
						cache.put(utt, result);
					}
				}
			}
			return result;
		};
	}

	private static CachingEventDialogueTransformer createInstrUttFilteringTransformer(
			final List<EventDialogueTransformer> diagTransformers) {
		final List<EventDialogueTransformer> chain = new ArrayList<>(diagTransformers.size() + 1);
		chain.add(INSTR_UTT_FILTER);
		chain.addAll(diagTransformers);
		final ChainedEventDialogueTransformer chainedTransformer = new ChainedEventDialogueTransformer(chain);
		return new CachingEventDialogueTransformer(createTransformedDialogueCache(chainedTransformer));
	}

	private static Function<ClassificationContext, EventDialogueClassifier> createSimpleClassifierFactory(
			final TrainingContext trainingCtx) {
		final ApplicationContext appCtx = trainingCtx.getAppCtx();
		final WordClassDiscountingSmoother smoother = appCtx.getBean(WordClassDiscountingSmoother.class);
		return classificationContext -> {
			final ParallelizedWordLogisticClassifierTrainer trainer = new ParallelizedWordLogisticClassifierTrainer(
					classificationContext.getBackgroundJobExecutor(), smoother);
			final Function<String, Logistic> wordClassifiers = trainer
					.apply(classificationContext.getTrainingData())::get;
			// This classifier is statically-trained, i.e. the word models
			// used for classification are the same no matter what dialogue
			// is being classified
			return new IsolatedUtteranceEventDialogueClassifier((diagToClassify, ctx) -> wordClassifiers,
					classificationContext.getReferentConfidenceMapFactory());
		};
	}

	private static Function<ClassificationContext, EventDialogueClassifier> createSimpleIterativeClassifierFactory(
			final TrainingContext trainingCtx) {
		final ApplicationContext appCtx = trainingCtx.getAppCtx();
		final WordClassDiscountingSmoother smoother = appCtx.getBean(WordClassDiscountingSmoother.class);
		final EntityInstanceAttributeContext entityInstAttrCtx = appCtx.getBean(EntityInstanceAttributeContext.class);
		final EntityFeatureExtractionContextFactory extCtxFactory = appCtx
				.getBean(EntityFeatureExtractionContextFactory.class);
		return classificationContext -> {
			final ParallelizedWordLogisticClassifierTrainer trainer = new ParallelizedWordLogisticClassifierTrainer(
					classificationContext.getBackgroundJobExecutor(), smoother);

			final AbstractInstanceExtractor instExtractor = new OnePositiveMaximumNegativeInstanceExtractor(
					entityInstAttrCtx, trainingCtx.getDiagTransformer(), extCtxFactory);
			final Map<WordClassifierTrainingParameter, Object> trainingParams = trainingCtx.getTrainingParams();
			final IterativeWordLogisticClassifierTrainer<Logistic> iterativeTrainer = new IterativeWordLogisticClassifierTrainer<>(
					trainer, classificationContext.getTrainingData(), instExtractor,
					(Double) trainingParams
							.get(WordClassifierTrainingParameter.INTERACTION_DATA_POSITIVE_EXAMPLE_WEIGHT_FACTOR),
					(Double) trainingParams
							.get(WordClassifierTrainingParameter.INTERACTION_DATA_NEGATIVE_EXAMPLE_WEIGHT_FACTOR));
			return new IsolatedUtteranceEventDialogueClassifier(iterativeTrainer,
					classificationContext.getReferentConfidenceMapFactory());
		};
	}

	private static LoadingCache<EventDialogue, EventDialogue> createTransformedDialogueCache(
			final EventDialogueTransformer transformer) {
		return CacheBuilder.newBuilder().softValues().initialCapacity(ESTIMATED_MIN_SESSION_DIALOGUE_COUNT)
				.maximumSize(MAXIMUM_TRANSFORMED_DIAG_CACHE_SIZE)
				.concurrencyLevel(EVENT_DIALOGUE_PROCESSING_CONCURRENCY).build(CacheLoader.from(transformer::apply));
	}

	private static ToDoubleFunction<Utterance> fetchCachingUttAcceptanceRanker(
			final Map<TrainingContext, Reference<ToDoubleFunction<Utterance>>> ctxAcceptanceRankers,
			final TrainingContext trainingCtx) {
		return ctxAcceptanceRankers.compute(trainingCtx, (key, oldRef) -> {
			final Reference<ToDoubleFunction<Utterance>> newRef;
			if (oldRef == null || oldRef.get() == null) {
				newRef = new SoftReference<>(createCachingUttAcceptanceRanker(trainingCtx.getTrainingParams()));
			} else {
				newRef = oldRef;
			}
			return newRef;
		}).get();
	}

	private static DialogicWeightedWordClassFactory fetchWordClassFactory(
			final Map<TrainingContext, Reference<DialogicWeightedWordClassFactory>> ctxWordClassFactories,
			final TrainingContext trainingCtx) {
		return ctxWordClassFactories.compute(trainingCtx, (key, oldRef) -> {
			final Reference<DialogicWeightedWordClassFactory> newRef;
			if (oldRef == null || oldRef.get() == null) {
				final Map<WordClassifierTrainingParameter, Object> trainingParams = key.getTrainingParams();
				final DialogicWeightedWordClassFactory newInst = new DialogicWeightedWordClassFactory(
						(Double) trainingParams
								.get(WordClassifierTrainingParameter.INSTRUCTOR_UTTERANCE_OBSERVATION_WEIGHT),
						(Double) trainingParams
								.get(WordClassifierTrainingParameter.OTHER_UTTERANCE_OBSERVATION_WEIGHT));
				newRef = new SoftReference<>(newInst);
			} else {
				newRef = oldRef;
			}
			return newRef;
		}).get();
	}

	private final int iterCount;

	private Training(final int iterCount) {
		this.iterCount = iterCount;
	}

	public abstract CachingEventDialogueTransformer createSymmetricalTrainingTestingEvgDiagTransformer(
			final List<EventDialogueTransformer> diagTransformers);

	public abstract TrainingInstancesFactory createTrainingInstsFactory(TrainingContext trainingCtx);

	/**
	 * @return the classifierFactory
	 */
	public abstract Function<ClassificationContext, EventDialogueClassifier> getClassifierFactory(
			TrainingContext trainingCtx);

	public int getIterCount() {
		return iterCount;
	}

}