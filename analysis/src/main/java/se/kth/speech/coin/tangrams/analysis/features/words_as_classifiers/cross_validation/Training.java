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
import java.math.BigDecimal;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.function.ToDoubleFunction;

import org.springframework.context.ApplicationContext;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.google.common.collect.Lists;

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
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues.IsolatedUtteranceEventDialogueClassifier;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.AbstractInstanceExtractor;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.DialogicInstanceExtractor;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.EventDialogueContextWordClassifierTrainer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.OnePositiveMaximumNegativeInstanceExtractor;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.OnePositiveOneNegativeInstanceExtractor;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.ParallelizedLogisticWordClassifierTrainer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.SizeEstimatingInstancesMapFactory;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.TrainingException;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.TrainingInstancesFactory;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.UpdatingLogisticWordClassifierTrainer;
import se.kth.speech.nlp.PatternMatchingUtteranceAcceptanceRanker;
import weka.classifiers.Classifier;
import weka.classifiers.functions.Logistic;

enum Training {
	ALL_NEG(1) {

		@Override
		public CachingEventDialogueTransformer createSymmetricalTrainingTestingEventDiagTransformer(
				final List<EventDialogueTransformer> diagTransformers) {
			return createCachingChainedTransformer(diagTransformers);
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
		public Function<ClassificationContext, IsolatedUtteranceEventDialogueClassifier> getClassifierFactory(
				final TrainingContext trainingCtx) {
			return new SimpleClassifierFactory(trainingCtx);
		}
	},
	ALL_NEG_ITERATIVE(1) {

		@Override
		public CachingEventDialogueTransformer createSymmetricalTrainingTestingEventDiagTransformer(
				final List<EventDialogueTransformer> diagTransformers) {
			return createCachingChainedTransformer(diagTransformers);
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
		public Function<ClassificationContext, IsolatedUtteranceEventDialogueClassifier> getClassifierFactory(
				final TrainingContext trainingCtx) {
			return new SimpleIterativeClassifierFactory(trainingCtx);
		}
	},
	DIALOGIC(1) {

		@Override
		public CachingEventDialogueTransformer createSymmetricalTrainingTestingEventDiagTransformer(
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
					new DialogicInstanceExtractor(
							entityInstAttrCtx, trainingCtx
									.getDiagTransformer(),
							extCtxFactory, fetchCachingUttAcceptanceRanker(trainingCtx),
							fetchDialogicWordClassFactory(trainingCtx), trainingCtx.getUttRelHandler()),
					entityInstAttrCtx,
					(Double) trainingParams
							.get(WordClassifierTrainingParameter.BACKGROUND_DATA_POSITIVE_EXAMPLE_WEIGHT_FACTOR),
					(Double) trainingParams
							.get(WordClassifierTrainingParameter.BACKGROUND_DATA_NEGATIVE_EXAMPLE_WEIGHT_FACTOR));
		}

		@Override
		public Function<ClassificationContext, DialogicEventDialogueClassifier> getClassifierFactory(
				final TrainingContext trainingCtx) {
			final ApplicationContext appCtx = trainingCtx.getAppCtx();
			final Integer smoothingMinCount = (Integer) trainingCtx.getTrainingParams()
					.get(WordClassifierTrainingParameter.SMOOTHING_MIN_COUNT);
			final WordClassDiscountingSmoother smoother = appCtx.getBean(WordClassDiscountingSmoother.class,
					smoothingMinCount);
			return classificationContext -> {
				final ParallelizedLogisticWordClassifierTrainer trainer = new ParallelizedLogisticWordClassifierTrainer(
						classificationContext.getBackgroundJobExecutor(), smoother);
				// This classifier is statically-trained, i.e. the word models
				// used for classification are the same no matter what dialogue
				// is being classified
				final EventDialogueContextWordClassifierTrainer<Logistic> diagWordClassifierFactory;
				{
					final Future<ConcurrentMap<String, Logistic>> futureWordClassifiers = trainer
							.apply(classificationContext.getTrainingData()).getFutureWordClassifiers();
					diagWordClassifierFactory = (diagToClassify,
							ctx) -> new FutureWordClassifierTrainingResultsGetter<>(futureWordClassifiers)
									.get();
				}
				return new DialogicEventDialogueClassifier(diagWordClassifierFactory,
						fetchCachingUttAcceptanceRanker(trainingCtx), fetchDialogicWordClassFactory(trainingCtx),
						classificationContext.getReferentConfidenceMapFactory());
			};
		};

	},
	DIALOGIC_ITERATIVE(1) {

		@Override
		public CachingEventDialogueTransformer createSymmetricalTrainingTestingEventDiagTransformer(
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
					new DialogicInstanceExtractor(
							entityInstAttrCtx, trainingCtx
									.getDiagTransformer(),
							extCtxFactory, fetchCachingUttAcceptanceRanker(trainingCtx),
							fetchDialogicWordClassFactory(trainingCtx), trainingCtx.getUttRelHandler()),
					entityInstAttrCtx,
					(Double) trainingParams
							.get(WordClassifierTrainingParameter.BACKGROUND_DATA_POSITIVE_EXAMPLE_WEIGHT_FACTOR),
					(Double) trainingParams
							.get(WordClassifierTrainingParameter.BACKGROUND_DATA_NEGATIVE_EXAMPLE_WEIGHT_FACTOR));
		}

		@Override
		public Function<ClassificationContext, DialogicEventDialogueClassifier> getClassifierFactory(
				final TrainingContext trainingCtx) {
			final ApplicationContext appCtx = trainingCtx.getAppCtx();
			final EntityInstanceAttributeContext entityInstAttrCtx = appCtx
					.getBean(EntityInstanceAttributeContext.class);
			final EntityFeatureExtractionContextFactory extCtxFactory = appCtx
					.getBean(EntityFeatureExtractionContextFactory.class);
			return classificationContext -> {
				final AbstractInstanceExtractor instExtractor = new OnePositiveMaximumNegativeInstanceExtractor(
						entityInstAttrCtx, trainingCtx.getDiagTransformer(), extCtxFactory);
				final Map<WordClassifierTrainingParameter, Object> trainingParams = trainingCtx.getTrainingParams();
				final Integer smoothingMinCount = (Integer) trainingCtx.getTrainingParams()
						.get(WordClassifierTrainingParameter.SMOOTHING_MIN_COUNT);
				final WordClassDiscountingSmoother smoother = appCtx.getBean(WordClassDiscountingSmoother.class,
						smoothingMinCount);
				final UpdatingLogisticWordClassifierTrainer iterativeTrainer = new UpdatingLogisticWordClassifierTrainer(
						classificationContext.getBackgroundJobExecutor(), smoother,
						classificationContext.getTrainingData(), instExtractor,
						(Double) trainingParams
								.get(WordClassifierTrainingParameter.INTERACTION_DATA_POSITIVE_EXAMPLE_WEIGHT_FACTOR),
						(Double) trainingParams
								.get(WordClassifierTrainingParameter.INTERACTION_DATA_NEGATIVE_EXAMPLE_WEIGHT_FACTOR));
				return new DialogicEventDialogueClassifier(iterativeTrainer,
						fetchCachingUttAcceptanceRanker(trainingCtx), fetchDialogicWordClassFactory(trainingCtx),
						classificationContext.getReferentConfidenceMapFactory());
			};
		};

	},
	ONE_NEG(5) {

		@Override
		public CachingEventDialogueTransformer createSymmetricalTrainingTestingEventDiagTransformer(
				final List<EventDialogueTransformer> diagTransformers) {
			return createCachingChainedTransformer(diagTransformers);
		}

		@Override
		public TrainingInstancesFactory createTrainingInstsFactory(final TrainingContext trainingCtx) {
			final ApplicationContext appCtx = trainingCtx.getAppCtx();
			final EntityInstanceAttributeContext entityInstAttrCtx = appCtx
					.getBean(EntityInstanceAttributeContext.class);
			final EntityFeatureExtractionContextFactory extCtxFactory = appCtx
					.getBean(EntityFeatureExtractionContextFactory.class);
			final Map<WordClassifierTrainingParameter, Object> trainingParams = trainingCtx.getTrainingParams();
			final Random rnd = new Random((Long) trainingParams.get(WordClassifierTrainingParameter.RANDOM_SEED));
			return new SizeEstimatingInstancesMapFactory(
					new OnePositiveOneNegativeInstanceExtractor(entityInstAttrCtx, trainingCtx.getDiagTransformer(),
							extCtxFactory, rnd),
					entityInstAttrCtx,
					(Double) trainingParams
							.get(WordClassifierTrainingParameter.BACKGROUND_DATA_POSITIVE_EXAMPLE_WEIGHT_FACTOR),
					(Double) trainingParams
							.get(WordClassifierTrainingParameter.BACKGROUND_DATA_NEGATIVE_EXAMPLE_WEIGHT_FACTOR));
		}

		@Override
		public Function<ClassificationContext, IsolatedUtteranceEventDialogueClassifier> getClassifierFactory(
				final TrainingContext trainingCtx) {
			return new SimpleClassifierFactory(trainingCtx);
		}
	},
	ONE_NEG_ITERATIVE(5) {

		@Override
		public CachingEventDialogueTransformer createSymmetricalTrainingTestingEventDiagTransformer(
				final List<EventDialogueTransformer> diagTransformers) {
			return createCachingChainedTransformer(diagTransformers);
		}

		@Override
		public TrainingInstancesFactory createTrainingInstsFactory(final TrainingContext trainingCtx) {
			final ApplicationContext appCtx = trainingCtx.getAppCtx();
			final EntityInstanceAttributeContext entityInstAttrCtx = appCtx
					.getBean(EntityInstanceAttributeContext.class);
			final EntityFeatureExtractionContextFactory extCtxFactory = appCtx
					.getBean(EntityFeatureExtractionContextFactory.class);
			final Map<WordClassifierTrainingParameter, Object> trainingParams = trainingCtx.getTrainingParams();
			final Random rnd = new Random((Long) trainingParams.get(WordClassifierTrainingParameter.RANDOM_SEED));
			return new SizeEstimatingInstancesMapFactory(
					new OnePositiveOneNegativeInstanceExtractor(entityInstAttrCtx, trainingCtx.getDiagTransformer(),
							extCtxFactory, rnd),
					entityInstAttrCtx,
					(Double) trainingParams
							.get(WordClassifierTrainingParameter.BACKGROUND_DATA_POSITIVE_EXAMPLE_WEIGHT_FACTOR),
					(Double) trainingParams
							.get(WordClassifierTrainingParameter.BACKGROUND_DATA_NEGATIVE_EXAMPLE_WEIGHT_FACTOR));
		}

		@Override
		public Function<ClassificationContext, IsolatedUtteranceEventDialogueClassifier> getClassifierFactory(
				final TrainingContext trainingCtx) {
			return new SimpleIterativeClassifierFactory(trainingCtx);
		}

	};

	private static class FutureWordClassifierTrainingResultsGetter<C extends Classifier>
			implements Supplier<ConcurrentMap<String, ? extends Classifier>> {

		/**
		 * A {@link CompletableFuture future} of the {@link ConcurrentMap} of word
		 * classifiers to use for the next dialogue being classified.
		 */
		private final Future<? extends ConcurrentMap<String, C>> futureWordClassifiers;

		private FutureWordClassifierTrainingResultsGetter(
				final Future<ConcurrentMap<String, C>> futureWordClassifiers) {
			this.futureWordClassifiers = futureWordClassifiers;
		}

		@Override
		public ConcurrentMap<String, C> get() {
			ConcurrentMap<String, C> result;
			try {
				result = futureWordClassifiers.get();
			} catch (final ExecutionException | InterruptedException e) {
				throw new TrainingException(e);
			}
			return result;
		}
	}

	private static class SimpleClassifierFactory
			implements Function<ClassificationContext, IsolatedUtteranceEventDialogueClassifier> {

		private final TrainingContext trainingCtx;

		private SimpleClassifierFactory(final TrainingContext trainingCtx) {
			this.trainingCtx = trainingCtx;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.util.function.Function#apply(java.lang.Object)
		 */
		@Override
		public IsolatedUtteranceEventDialogueClassifier apply(final ClassificationContext classificationContext) {
			final ApplicationContext appCtx = trainingCtx.getAppCtx();
			final Map<WordClassifierTrainingParameter, Object> trainingParams = trainingCtx.getTrainingParams();
			final Integer smoothingMinCount = (Integer) trainingParams
					.get(WordClassifierTrainingParameter.SMOOTHING_MIN_COUNT);
			final WordClassDiscountingSmoother smoother = appCtx.getBean(WordClassDiscountingSmoother.class,
					smoothingMinCount);
			final ParallelizedLogisticWordClassifierTrainer trainer = new ParallelizedLogisticWordClassifierTrainer(
					classificationContext.getBackgroundJobExecutor(), smoother);

			// This classifier is statically-trained, i.e. the word models
			// used for classification are the same no matter what dialogue
			// is being classified
			final EventDialogueContextWordClassifierTrainer<Logistic> diagWordClassifierFactory;
			{
				final Future<ConcurrentMap<String, Logistic>> futureWordClassifiers = trainer
						.apply(classificationContext.getTrainingData()).getFutureWordClassifiers();
				diagWordClassifierFactory = (diagToClassify,
						ctx) -> new FutureWordClassifierTrainingResultsGetter<>(futureWordClassifiers).get();
			}

			final BigDecimal instrUttObsWeight = (BigDecimal) trainingParams
					.get(WordClassifierTrainingParameter.INSTRUCTOR_UTTERANCE_OBSERVATION_WEIGHT);
			final BigDecimal otherUttObsWeight = (BigDecimal) trainingParams
					.get(WordClassifierTrainingParameter.OTHER_UTTERANCE_OBSERVATION_WEIGHT);
			return new IsolatedUtteranceEventDialogueClassifier(diagWordClassifierFactory,
					classificationContext.getReferentConfidenceMapFactory(), instrUttObsWeight, otherUttObsWeight);
		}

	}

	private static class SimpleIterativeClassifierFactory
			implements Function<ClassificationContext, IsolatedUtteranceEventDialogueClassifier> {

		private final TrainingContext trainingCtx;

		private SimpleIterativeClassifierFactory(final TrainingContext trainingCtx) {
			this.trainingCtx = trainingCtx;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.util.function.Function#apply(java.lang.Object)
		 */
		@Override
		public IsolatedUtteranceEventDialogueClassifier apply(final ClassificationContext classificationContext) {
			final ApplicationContext appCtx = trainingCtx.getAppCtx();
			final Map<WordClassifierTrainingParameter, Object> trainingParams = trainingCtx.getTrainingParams();
			final Integer smoothingMinCount = (Integer) trainingParams
					.get(WordClassifierTrainingParameter.SMOOTHING_MIN_COUNT);
			final WordClassDiscountingSmoother smoother = appCtx.getBean(WordClassDiscountingSmoother.class,
					smoothingMinCount);
			final EntityInstanceAttributeContext entityInstAttrCtx = appCtx
					.getBean(EntityInstanceAttributeContext.class);
			final EntityFeatureExtractionContextFactory extCtxFactory = appCtx
					.getBean(EntityFeatureExtractionContextFactory.class);

			final AbstractInstanceExtractor instExtractor = new OnePositiveMaximumNegativeInstanceExtractor(
					entityInstAttrCtx, trainingCtx.getDiagTransformer(), extCtxFactory);
			final UpdatingLogisticWordClassifierTrainer iterativeTrainer = new UpdatingLogisticWordClassifierTrainer(
					classificationContext.getBackgroundJobExecutor(), smoother, classificationContext.getTrainingData(),
					instExtractor,
					(Double) trainingParams
							.get(WordClassifierTrainingParameter.INTERACTION_DATA_POSITIVE_EXAMPLE_WEIGHT_FACTOR),
					(Double) trainingParams
							.get(WordClassifierTrainingParameter.INTERACTION_DATA_NEGATIVE_EXAMPLE_WEIGHT_FACTOR));

			final BigDecimal instrUttObsWeight = (BigDecimal) trainingParams
					.get(WordClassifierTrainingParameter.INSTRUCTOR_UTTERANCE_OBSERVATION_WEIGHT);
			final BigDecimal otherUttObsWeight = (BigDecimal) trainingParams
					.get(WordClassifierTrainingParameter.OTHER_UTTERANCE_OBSERVATION_WEIGHT);
			return new IsolatedUtteranceEventDialogueClassifier(iterativeTrainer,
					classificationContext.getReferentConfidenceMapFactory(), instrUttObsWeight, otherUttObsWeight);
		}

	}

	/**
	 * <strong>NOTE:</strong> This uses {@link TrainingContext} as keys because
	 * individual contexts also determine how {@link Utterance} instances are
	 * created.
	 */
	private static final ConcurrentMap<TrainingContext, Reference<ToDoubleFunction<Utterance>>> CTX_ACCEPTANCE_RANKERS = new ConcurrentHashMap<>(
			3);

	/**
	 * <strong>NOTE:</strong> This uses {@link TrainingContext} as keys because
	 * individual contexts also determine how {@link Utterance} instances are
	 * created.
	 */
	private static final ConcurrentMap<TrainingContext, Reference<DialogicWeightedWordClassFactory>> CTX_DIALOGIC_WORD_CLASS_FACTORIES = new ConcurrentHashMap<>(
			3);

	private static final int ESTIMATED_MIN_SESSION_DIALOGUE_COUNT = 50;

	private static final int EVENT_DIALOGUE_PROCESSING_CONCURRENCY = 1;

	private static final long MAXIMUM_TRANSFORMED_DIAG_CACHE_SIZE = 1000;

	private static CachingEventDialogueTransformer createCachingChainedTransformer(
			final List<EventDialogueTransformer> diagTransformers) {
		final EventDialogueTransformer decorated;
		if (diagTransformers.size() == 1) {
			decorated = diagTransformers.get(0);
		} else {
			decorated = new ChainedEventDialogueTransformer(diagTransformers);
		}
		return new CachingEventDialogueTransformer(createTransformedDialogueCache(decorated));
	}

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

	private static LoadingCache<EventDialogue, EventDialogue> createTransformedDialogueCache(
			final EventDialogueTransformer transformer) {
		return CacheBuilder.newBuilder().softValues().initialCapacity(ESTIMATED_MIN_SESSION_DIALOGUE_COUNT)
				.maximumSize(MAXIMUM_TRANSFORMED_DIAG_CACHE_SIZE)
				.concurrencyLevel(EVENT_DIALOGUE_PROCESSING_CONCURRENCY).build(CacheLoader.from(transformer::apply));
	}

	private static ToDoubleFunction<Utterance> fetchCachingUttAcceptanceRanker(final TrainingContext trainingCtx) {
		return CTX_ACCEPTANCE_RANKERS.compute(trainingCtx, (key, oldRef) -> {
			final Reference<ToDoubleFunction<Utterance>> newRef;
			if (oldRef == null || oldRef.get() == null) {
				newRef = new SoftReference<>(createCachingUttAcceptanceRanker(trainingCtx.getTrainingParams()));
			} else {
				newRef = oldRef;
			}
			return newRef;
		}).get();
	}

	private static DialogicWeightedWordClassFactory fetchDialogicWordClassFactory(final TrainingContext trainingCtx) {
		return CTX_DIALOGIC_WORD_CLASS_FACTORIES.compute(trainingCtx, (key, oldRef) -> {
			final Reference<DialogicWeightedWordClassFactory> newRef;
			if (oldRef == null || oldRef.get() == null) {
				final Map<WordClassifierTrainingParameter, Object> trainingParams = key.getTrainingParams();
				final BigDecimal instrUttObsWeight = (BigDecimal) trainingParams
						.get(WordClassifierTrainingParameter.INSTRUCTOR_UTTERANCE_OBSERVATION_WEIGHT);
				final BigDecimal otherUttObsWeight = (BigDecimal) trainingParams
						.get(WordClassifierTrainingParameter.OTHER_UTTERANCE_OBSERVATION_WEIGHT);
				final DialogicWeightedWordClassFactory newInst = new DialogicWeightedWordClassFactory(instrUttObsWeight,
						otherUttObsWeight);
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

	public final CachingEventDialogueTransformer createSymmetricalTrainingTestingEventDiagTransformer(
			final EventDialogueTransformer firstDiagTransformer,
			final EventDialogueTransformer... nextDiagTransformers) {
		return createSymmetricalTrainingTestingEventDiagTransformer(
				Lists.asList(firstDiagTransformer, nextDiagTransformers));
	}

	public abstract CachingEventDialogueTransformer createSymmetricalTrainingTestingEventDiagTransformer(
			final List<EventDialogueTransformer> diagTransformers);

	public abstract TrainingInstancesFactory createTrainingInstsFactory(TrainingContext trainingCtx);

	/**
	 * @return the classifierFactory
	 */
	public abstract Function<ClassificationContext, ? extends EventDialogueClassifier> getClassifierFactory(
			TrainingContext trainingCtx);

	public int getIterCount() {
		return iterCount;
	}

}