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

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.Queue;
import java.util.Random;
import java.util.Set;
import java.util.function.Function;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Sets;

import se.kth.speech.coin.tangrams.analysis.io.SessionDataManager;

/**
 *
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 6 Nov 2017
 *
 */
final class RandomTrainingComplementSetFactory
		implements Function<Collection<SessionDataManager>, Set<Set<SessionDataManager>>> {

	private static final Logger LOGGER = LoggerFactory.getLogger(RandomTrainingComplementSetFactory.class);

//	private final int maxUniqueLeftOutSessionCount;

	private final int minValidTrainingSetSize;

	private final Random random;

	private final int trainingSetSizeDiscountingConstant;

	public RandomTrainingComplementSetFactory(final Random random, final int trainingSetSizeDiscountingConstant) {
		if (trainingSetSizeDiscountingConstant < 1) {
			throw new IllegalArgumentException("Discounting constant must be positive.");
		}
		this.random = random;
		this.trainingSetSizeDiscountingConstant = trainingSetSizeDiscountingConstant;
		minValidTrainingSetSize = Math.addExact(trainingSetSizeDiscountingConstant, 1);
//		maxUniqueLeftOutSessionCount = randomIters * trainingSetSizeDiscountingConstant;
	}

	@Override
	public Set<Set<SessionDataManager>> apply(final Collection<SessionDataManager> allTrainingSessionDataMgrs) {
		final Set<Set<SessionDataManager>> result;

		if (allTrainingSessionDataMgrs.size() < minValidTrainingSetSize) {
			throw new IllegalArgumentException(String.format("Expected at least %d session(s) but was passed only %d.",
					minValidTrainingSetSize, allTrainingSessionDataMgrs.size()));

		} else if (allTrainingSessionDataMgrs.size() >= trainingSetSizeDiscountingConstant) {
			// Each random iteration can have entirely-unique sessions left out
			result = createUniqueTrainingComplementSets(allTrainingSessionDataMgrs);
			LOGGER.debug("Created {} entirely-unique complement set(s).", result.size());
		} else {
			// result =
			// createInitialUniqueTrainingComplementSets(allTrainingSessionDataMgrs);
			// final int resultSetDeficit = randomIters - result.size();
			// LOGGER.info("Created {} initial entirely-unique complement
			// set(s); Still need to create {} more.", result.size(),
			// resultSetDeficit);
			// TODO: Finish
			throw new IllegalArgumentException("Creating non-unique training sets not yet implemented.");
		}

		return result;
	}

	// private Set<Set<SessionDataManager>>
	// createInitialUniqueTrainingComplementSets(
	// final Collection<SessionDataManager> allTrainingSessionDataMgrs) {
	// final Set<Set<SessionDataManager>> result =
	// Sets.newHashSetWithExpectedSize(allTrainingSessionDataMgrs.size());
	// final Queue<SessionDataManager> sessionsToDiscount =
	// createShuffledQueue(allTrainingSessionDataMgrs);
	//
	// while (result.size() < randomIters && !sessionsToDiscount.isEmpty()) {
	// final Set<SessionDataManager> currentComplementSet = Sets
	// .newHashSetWithExpectedSize(trainingSetSizeDiscountingConstant);
	// do {
	// final SessionDataManager leftOutTrainingSessionDataMgr =
	// sessionsToDiscount.remove();
	// final boolean wasAddedToCurrentComplementSet =
	// currentComplementSet.add(leftOutTrainingSessionDataMgr);
	// // Since the given session has never been left out before, it
	// // should also have not been used for the current set being
	// // built
	// assert wasAddedToCurrentComplementSet;
	// } while (currentComplementSet.size() < trainingSetSizeDiscountingConstant
	// && !sessionsToDiscount.isEmpty());
	// result.add(currentComplementSet);
	// }
	// LOGGER.info(
	// "{} sessions which could have still been left out were not because enough
	// complement sets had already been made.",
	// sessionsToDiscount.size());
	// return result;
	// }

	private <T> Queue<T> createShuffledQueue(final Collection<? extends T> elems) {
		final LinkedList<T> result = new LinkedList<>(elems);
		Collections.shuffle(result, random);
		return result;
	}

	private Set<Set<SessionDataManager>> createUniqueTrainingComplementSets(
			final Collection<SessionDataManager> allTrainingSessionDataMgrs) {
		assert allTrainingSessionDataMgrs.size() >= trainingSetSizeDiscountingConstant;
		final Set<Set<SessionDataManager>> result = Sets.newHashSetWithExpectedSize(allTrainingSessionDataMgrs.size());
		final Queue<SessionDataManager> sessionsToDiscount = createShuffledQueue(allTrainingSessionDataMgrs);

		final Set<SessionDataManager> currentComplementSet = Sets
				.newHashSetWithExpectedSize(trainingSetSizeDiscountingConstant);
		do {
			final SessionDataManager leftOutTrainingSessionDataMgr = sessionsToDiscount.remove();
			final boolean wasAddedToCurrentComplementSet = currentComplementSet.add(leftOutTrainingSessionDataMgr);
			// Since the given session has never been left out before, it
			// should also have not been used for the current set being
			// built
			assert wasAddedToCurrentComplementSet;
		} while (currentComplementSet.size() < trainingSetSizeDiscountingConstant);
		result.add(currentComplementSet);
		LOGGER.debug(
				"{} sessions which could have still been left out were not because enough complement sets had already been made.",
				sessionsToDiscount.size());
		return result;
	}

}
