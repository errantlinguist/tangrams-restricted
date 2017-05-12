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
package se.kth.speech.coin.tangrams.analysis.features;

import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Stream;

import javax.xml.bind.JAXBException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.BiMap;
import com.google.common.collect.Maps;

import se.kth.speech.coin.tangrams.analysis.GameContext;
import se.kth.speech.coin.tangrams.analysis.GameContextModelFactory;
import se.kth.speech.coin.tangrams.analysis.GameHistory;
import se.kth.speech.coin.tangrams.analysis.SegmentUtteranceFactory;
import se.kth.speech.coin.tangrams.analysis.SessionDataManager;
import se.kth.speech.coin.tangrams.analysis.TemporalGameContexts;
import se.kth.speech.coin.tangrams.analysis.Utterance;
import se.kth.speech.coin.tangrams.analysis.UtterancePlayerIdMapFactory;
import se.kth.speech.coin.tangrams.content.IconImages;
import se.kth.speech.hat.xsd.Annotation;
import weka.core.Attribute;
import weka.core.DenseInstance;
import weka.core.Instance;
import weka.core.Instances;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Apr 17, 2017
 * @see <a href="http://anthology.aclweb.org/W/W15/W15-0124.pdf">Casey
 *      Kennington, Livia Dia, &amp; David Schlangen. &ldquo;A Discriminative
 *      Model for Perceptually-Grounded Incremental Reference Resolution.&rdquo;
 *      In <em>Proceedings of IWCS 2015</em><a>.
 *
 */
public final class WordsAsClassifiersInstancesMapFactory {

	private static class MultiClassDataCollector {

		private static final ImageEdgeCounter IMG_EDGE_COUNTER = new ImageEdgeCounter();

		private static final SegmentUtteranceFactory SEG_UTT_FACTORY = new SegmentUtteranceFactory();

		private static final Function<GameContext, Optional<Integer>> SELECTED_ENTITY_ID_GETTER = ctx -> ctx
				.findLastSelectedEntityId();

		private final Function<? super String, Instances> classInstanceGetter;

		private final Function<? super Path, Map<String, GameHistory>> gameHistoryFactory;

		private final Function<? super Path, Annotation> hatAnnotationFactory;

		private final Function<GameContext, Integer> negativeExampleEntityIdGetter;

		private final int numAttributes;

		private MultiClassDataCollector(final Function<? super Path, Annotation> hatAnnotationFactory,
				final Function<? super Path, Map<String, GameHistory>> gameHistoryFactory,
				final Function<? super String, Instances> classInstanceGetter, final int numAttributes,
				final Function<GameContext, Integer> negativeExampleEntityIdGetter) {
			this.hatAnnotationFactory = hatAnnotationFactory;
			this.gameHistoryFactory = gameHistoryFactory;
			this.negativeExampleEntityIdGetter = negativeExampleEntityIdGetter;
			this.classInstanceGetter = classInstanceGetter;
			this.numAttributes = numAttributes;
		}

		private void accept(final SessionDataManager sessionData) {
			final Path hatInfilePath = sessionData.getHATFilePath();
			final Annotation uttAnnots = hatAnnotationFactory.apply(hatInfilePath);

			final Path eventLogPath = sessionData.getCanonicalEventLogPath();
			LOGGER.info("Reading events from \"{}\".", eventLogPath);
			// final Map<String, GameHistory> gameHistories =
			// LoggedEvents.parseGameHistories(Files.lines(eventLogPath),
			// LoggedEvents.VALID_MODEL_MIN_REQUIRED_EVENT_MATCHER);
			final Map<String, GameHistory> gameHistories = gameHistoryFactory.apply(eventLogPath);
			final int uniqueModelDescriptionCount = gameHistories.values().size();

			final EntityFeatureExtractionContextFactory extractionContextFactory = new EntityFeatureExtractionContextFactory(
					new GameContextModelFactory(uniqueModelDescriptionCount), IMG_EDGE_COUNTER);

			final BiMap<String, String> playerSourceIds = sessionData.getPlayerData().getPlayerSourceIds();
			final Function<String, String> sourcePlayerIdGetter = playerSourceIds.inverse()::get;
			final Map<Utterance, String> uttPlayerIds = new UtterancePlayerIdMapFactory(SEG_UTT_FACTORY::create,
					sourcePlayerIdGetter).apply(uttAnnots.getSegments().getSegment());
			final List<Utterance> utts = Arrays
					.asList(uttPlayerIds.keySet().stream().sorted().toArray(Utterance[]::new));

			for (final Entry<String, GameHistory> gameHistory : gameHistories.entrySet()) {
				final String gameId = gameHistory.getKey();
				LOGGER.debug("Processing game \"{}\".", gameId);
				final GameHistory history = gameHistory.getValue();
				for (final String perspectivePlayerId : playerSourceIds.keySet()) {
					LOGGER.info("Processing game from perspective of player \"{}\".", perspectivePlayerId);

					utts.forEach(utt -> {
						final String uttPlayerId = uttPlayerIds.get(utt);
						if (perspectivePlayerId.equals(uttPlayerId)) {
							final Stream<GameContext> uttContexts = TemporalGameContexts.create(history,
									utt.getStartTime(), utt.getEndTime(), perspectivePlayerId);
							uttContexts.forEach(uttContext -> {
								SELECTED_ENTITY_ID_GETTER.apply(uttContext).ifPresent(selectedEntityId -> {
									LOGGER.debug(
											"Creating positive and negative examples for entity ID \"{}\", which is selected by player \"{}\".",
											selectedEntityId, perspectivePlayerId);
									// Add positive training examples
									final EntityFeature.Extractor.Context positiveContext = extractionContextFactory
											.createExtractionContext(uttContext, selectedEntityId);
									addTokenInstances(utt, uttContext, positiveContext, Boolean.TRUE.toString());
									// Add negative training examples
									final EntityFeature.Extractor.Context negativeContext = extractionContextFactory
											.createExtractionContext(uttContext,
													negativeExampleEntityIdGetter.apply(uttContext));
									addTokenInstances(utt, uttContext, negativeContext, Boolean.FALSE.toString());
								});
							});
						} else {
							LOGGER.debug(
									"Skipping the extraction of features for utterance with segment ID \"{}\" because the utterance is from player \"{}\" rather than the player whose perspective is being used for extracting features (\"{}\")",
									utt.getSegmentId(), uttPlayerId, perspectivePlayerId);
						}
					});
				}
			}
		}

		private void addTokenInstances(final Utterance utt, final GameContext uttContext,
				final EntityFeature.Extractor.Context extractionContext, final String classValue) {
			utt.getTokens().forEach(token -> {
				final Instances classInstances = classInstanceGetter.apply(token);
				final Instance inst = new DenseInstance(numAttributes);
				inst.setDataset(classInstances);
				EXTRACTOR.accept(inst, extractionContext);
				inst.setClassValue(classValue);
				classInstances.add(inst);
			});
		}
	}

	private static final ArrayList<Attribute> ATTRS;

	private static final Attribute CLASS_ATTR;

	private static final EntityFeature.Extractor EXTRACTOR;

	private static final Logger LOGGER = LoggerFactory.getLogger(WordsAsClassifiersInstancesMapFactory.class);

	static {
		final List<String> shapeFeatureVals = new ArrayList<>(IconImages.getImageResources().keySet());
		shapeFeatureVals.sort(Comparator.naturalOrder());
		EXTRACTOR = new EntityFeature.Extractor(shapeFeatureVals);
		final Map<EntityFeature, Attribute> featureAttrs = EXTRACTOR.getFeatureAttrs();
		ATTRS = new ArrayList<>(featureAttrs.size() + 1);
		ATTRS.addAll(featureAttrs.values());
		CLASS_ATTR = new Attribute("REFERENT", Arrays.asList(Boolean.TRUE.toString(), Boolean.FALSE.toString()));
		ATTRS.add(CLASS_ATTR);
	}

	private static int estimateVocabTokenCount(final String token, final Collection<SessionDataManager> sessionData) {
		return sessionData.size() * 10;
	}

	private static int estimateVocabTypeCount(final Collection<SessionDataManager> sessionData) {
		return Math.toIntExact(Math.round(Math.ceil(Math.log(sessionData.size() * 850))));
	}

	private final Function<? super Path, Map<String, GameHistory>> gameHistoryFactory;

	private final Function<? super Path, Annotation> hatAnnotationFactory;

	private final Function<GameContext, Integer> negativeExampleEntityIdGetter;

	public WordsAsClassifiersInstancesMapFactory(final Function<? super Path, Annotation> hatAnnotationFactory,
			final Function<? super Path, Map<String, GameHistory>> gameHistoryFactory,
			final Function<GameContext, Integer> negativeExampleEntityIdGetter) {
		this.hatAnnotationFactory = hatAnnotationFactory;
		this.gameHistoryFactory = gameHistoryFactory;
		this.negativeExampleEntityIdGetter = negativeExampleEntityIdGetter;
	}

	public Map<String, Instances> apply(final Collection<SessionDataManager> sessionData)
			throws JAXBException, IOException {
		final Map<String, Instances> result = Maps.newHashMapWithExpectedSize(estimateVocabTypeCount(sessionData));
		final Function<String, Instances> classInstanceFetcher = className -> result.computeIfAbsent(className, k -> {
			final Instances instances = new Instances("referent_for_token-" + k, ATTRS,
					estimateVocabTokenCount(k, sessionData));
			instances.setClass(CLASS_ATTR);
			return instances;
		});
		final MultiClassDataCollector coll = new MultiClassDataCollector(hatAnnotationFactory, gameHistoryFactory,
				classInstanceFetcher, ATTRS.size(), negativeExampleEntityIdGetter);
		for (final SessionDataManager sessionDatum : sessionData) {
			coll.accept(sessionDatum);
		}
		return result;
	}

}
