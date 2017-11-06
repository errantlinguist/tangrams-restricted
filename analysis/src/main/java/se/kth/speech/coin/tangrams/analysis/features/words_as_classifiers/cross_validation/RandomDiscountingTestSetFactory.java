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

import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Random;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Stream;

import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.cache.LoadingCache;

import se.kth.speech.coin.tangrams.analysis.SessionGameManager;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.TrainingInstancesFactory;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.WordClassificationData;
import se.kth.speech.coin.tangrams.analysis.io.SessionDataManager;

/**
 * Creates cross-validation test/training dataset pairs.
 *
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 6 Nov 2017
 * @see
 *      <ul>
 *      <li><a href="http://anthology.aclweb.org/W/W15/W15-0124.pdf">Casey
 *      Kennington, Livia Dia, &amp; David Schlangen. &ldquo;A Discriminative
 *      Model for Perceptually-Grounded Incremental Reference Resolution.&rdquo;
 *      In <em>Proceedings of IWCS 2015</em><a>.</li>
 *      <li><a href="http://www.aclweb.org/anthology/P15-1029">Casey Kennington,
 *      &amp; David Schlangen. &ldquo;Simple Learning and Compositional
 *      Application of Perceptually Grounded Word Meanings for Incremental
 *      Reference Resolution&rdquo;. In <em>Proceedings of the 53<sup>rd</sup>
 *      Annual Meeting of the Association for Computational Linguistics and the
 *      7<sup>th</sup> International Joint Conference on Natural Language
 *      Processing</em><a>.</li>
 *      </ul>
 *
 */
public final class RandomDiscountingTestSetFactory
		implements Function<Map<SessionDataManager, Path>, Stream<Entry<SessionDataManager, WordClassificationData>>> {

	private static final Logger LOGGER = LoggerFactory.getLogger(RandomDiscountingTestSetFactory.class);

	private static final int MIN_INPUT_SIZE = 2;

	private final TrainingInstancesFactory instancesFactory;

	private final LoadingCache<? super SessionDataManager, SessionGameManager> sessionGameMgrs;

	private final RandomTrainingComplementSetFactory trainingSetFactory;

	public RandomDiscountingTestSetFactory(final TrainingInstancesFactory instancesFactory,
			final LoadingCache<? super SessionDataManager, SessionGameManager> sessionGameMgrs, final Random random,
			final int trainingSetSizeDiscountingConstant) {
		this.instancesFactory = instancesFactory;
		this.sessionGameMgrs = sessionGameMgrs;
		trainingSetFactory = new RandomTrainingComplementSetFactory(random, trainingSetSizeDiscountingConstant);
	}

	@Override
	public Stream<Entry<SessionDataManager, WordClassificationData>> apply(
			final Map<SessionDataManager, Path> allSessions) {
		if (allSessions.size() < MIN_INPUT_SIZE) {
			throw new IllegalArgumentException(
					String.format("Session count is %d but at least %d is required for cross-validation.",
							allSessions.size(), MIN_INPUT_SIZE));
		}
		final Stream.Builder<Entry<SessionDataManager, WordClassificationData>> resultBuilder = Stream.builder();
		for (final Entry<SessionDataManager, Path> testSessionDataEntry : allSessions.entrySet()) {
			final Path testSessionDataFilePath = testSessionDataEntry.getValue();
			LOGGER.info("Creating {}-fold cross-validation set for testing on session data from \"{}\".",
					allSessions.size(), testSessionDataFilePath);
			final SessionDataManager testSessionDataMgr = testSessionDataEntry.getKey();
			createTestSets(testSessionDataMgr, allSessions.keySet()).forEach(resultBuilder);
		}
		return resultBuilder.build();
	}

	private Stream<Entry<SessionDataManager, WordClassificationData>> createTestSets(
			final SessionDataManager testSessionDataMgr, final Set<SessionDataManager> allSessions) {
		final List<SessionDataManager> allTrainingSessionDataMgrs = Arrays.asList(allSessions.stream()
				.filter(sessionData -> !sessionData.equals(testSessionDataMgr)).toArray(SessionDataManager[]::new));
		final Set<Set<SessionDataManager>> discountedTrainingSets = trainingSetFactory
				.apply(allTrainingSessionDataMgrs);
		return discountedTrainingSets.stream().map(discountedTrainingSet -> {
			final List<SessionGameManager> trainingSessionEvtDiagMgrs = Arrays.asList(discountedTrainingSet.stream()
					.map(sessionGameMgrs::getUnchecked).toArray(SessionGameManager[]::new));
			final WordClassificationData trainingData = instancesFactory.apply(trainingSessionEvtDiagMgrs);
			LOGGER.info("Created training data for {} class(es).", trainingData.getClassInstances().size());
			return Pair.of(testSessionDataMgr, trainingData);
		});
	}

}
