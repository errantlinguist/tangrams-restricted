/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.analysis.
 *
 *  se.kth.speech.coin.tangrams-restricted.analysis is free software: you can redistribute it and/or modify
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

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Random;

import org.apache.commons.lang3.SystemUtils;
import org.junit.Assert;
import org.junit.Assume;
import org.junit.BeforeClass;
import org.junit.Test;

import se.kth.speech.coin.tangrams.analysis.io.SessionDataManager;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 6 Nov 2017
 *
 */
public final class RandomTrainingComplementSetFactoryTest {

	private static List<SessionDataManager> allSessions;

	private static final Random RANDOM = new Random(1);

	@BeforeClass
	public static void readTestSessionData() throws IOException {
		final Path testSessionDir = getDefaultTestSessionDir();
		final Map<SessionDataManager, Path> testSessionData = TestSessionData
				.readTestSessionData(getDefaultInpaths(testSessionDir));
		allSessions = Arrays.asList(testSessionData.keySet().toArray(new SessionDataManager[testSessionData.size()]));
	}

	private static List<Path> getDefaultInpaths(final Path testSessionDir) throws IOException {
		Assume.assumeTrue(String.format(
				"The generated default session directory path \"%s\" does not refer to a directory on this system.",
				testSessionDir), Files.isDirectory(testSessionDir));
		Assert.assertTrue(String.format(
				"The generated default session directory path \"%s\" does refer to a directory on this system but the directory is not readable",
				testSessionDir), Files.isReadable(testSessionDir));
		Assume.assumeTrue(String.format(
				"The generated default session directory path \"%s\" refers to a readable but empty directory.",
				testSessionDir), Files.list(testSessionDir).findAny().isPresent());
		return Collections.singletonList(testSessionDir);
	}

	private static Path getDefaultTestSessionDir() {
		final String homeDir = System.getProperty("user.home");
		final String sessionDirSuffix;
		if (SystemUtils.IS_OS_WINDOWS) {
			sessionDirSuffix = "Documents\\Projects\\Tangrams\\Data\\Ready";
		} else {
			sessionDirSuffix = "Projects/tangrams-restricted/Data/Ready";
		}
		return Paths.get(homeDir, sessionDirSuffix);
	}

	private static void testApply(final int trainingSetSizeDiscountingConstant, final int randomIters)
			throws IOException {
		Assume.assumeTrue(String.format(
				"Number of sessions found is not greater than the training set size discounting constant (%d).",
				trainingSetSizeDiscountingConstant), allSessions.size() > trainingSetSizeDiscountingConstant);
		Assume.assumeTrue(String.format(
				"Number of sessions found is not greater than or equal to the required number of random iterations (%d).",
				randomIters), allSessions.size() >= randomIters);
		final RandomTrainingComplementSetFactory testInst = new RandomTrainingComplementSetFactory(RANDOM,
				trainingSetSizeDiscountingConstant, randomIters);
		final Collection<? extends Collection<SessionDataManager>> complementSets = testInst.apply(allSessions);
		Assert.assertEquals(randomIters, complementSets.size());
		for (final Collection<SessionDataManager> complementSet : complementSets) {
			Assert.assertEquals(trainingSetSizeDiscountingConstant, complementSet.size());
		}
		// Assert.assertThat(complementSets,
		// Every.everyItem(IsCollectionWithSize.hasSize(trainingSetSizeDiscountingConstant)));
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.cross_validation.RandomTrainingComplementSetFactory#apply(se.kth.speech.coin.tangrams.analysis.io.SessionDataManager[])}.
	 *
	 * @throws IOException
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testApplyMaxComplementSetSizeMaxIters() throws IOException {
		final int trainingSetSizeDiscountingConstant = allSessions.size() - 1;
		final int randomIters = allSessions.size();
		testApply(trainingSetSizeDiscountingConstant, randomIters);
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.cross_validation.RandomTrainingComplementSetFactory#apply(se.kth.speech.coin.tangrams.analysis.io.SessionDataManager[])}.
	 *
	 * @throws IOException
	 */
	@Test
	public void testApplyMaxComplementSetSizeMinIters() throws IOException {
		final int trainingSetSizeDiscountingConstant = allSessions.size() - 1;
		final int randomIters = 1;
		testApply(trainingSetSizeDiscountingConstant, randomIters);
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.cross_validation.RandomTrainingComplementSetFactory#apply(se.kth.speech.coin.tangrams.analysis.io.SessionDataManager[])}.
	 *
	 * @throws IOException
	 */
	@Test
	public void testApplyMaxIterWithUniqueComplementSets() throws IOException {
		// NOTE: Hard-coded for session count of 39
		final int trainingSetSizeDiscountingConstant = 3;
		final int randomIters = allSessions.size() / trainingSetSizeDiscountingConstant;
		Assume.assumeTrue(
				String.format(
						"Cannot divide number of total sessions (%d) by hard-coded training set size discounting constant %d.",
						allSessions.size(), trainingSetSizeDiscountingConstant),
				allSessions.size() % trainingSetSizeDiscountingConstant == 0);
		testApply(trainingSetSizeDiscountingConstant, randomIters);
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.cross_validation.RandomTrainingComplementSetFactory#apply(se.kth.speech.coin.tangrams.analysis.io.SessionDataManager[])}.
	 *
	 * @throws IOException
	 */
	@Test
	public void testApplyMinComplementSetSizeMaxIters() throws IOException {
		final int trainingSetSizeDiscountingConstant = 1;
		final int randomIters = allSessions.size();
		testApply(trainingSetSizeDiscountingConstant, randomIters);
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.cross_validation.RandomTrainingComplementSetFactory#apply(se.kth.speech.coin.tangrams.analysis.io.SessionDataManager[])}.
	 *
	 * @throws IOException
	 */
	@Test
	public void testApplyMinComplementSetSizeMinIters() throws IOException {
		final int trainingSetSizeDiscountingConstant = 1;
		final int randomIters = 1;
		testApply(trainingSetSizeDiscountingConstant, randomIters);
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.cross_validation.RandomTrainingComplementSetFactory#apply(se.kth.speech.coin.tangrams.analysis.io.SessionDataManager[])}.
	 *
	 * @throws IOException
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testApplyTooHighDiscountingConstant() throws IOException {
		final int trainingSetSizeDiscountingConstant = allSessions.size();
		// Assume.assumeTrue(String.format(
		// "Number of sessions found is not lesser than or equal to the training
		// set size discounting constant (%d).",
		// trainingSetSizeDiscountingConstant), allSessions.size() <=
		// trainingSetSizeDiscountingConstant);
		final int randomIters = 1;
		Assume.assumeTrue(String.format(
				"Number of sessions found is not greater than or equal to the required number of random iterations (%d).",
				randomIters), allSessions.size() >= randomIters);

		final RandomTrainingComplementSetFactory testInst = new RandomTrainingComplementSetFactory(RANDOM,
				trainingSetSizeDiscountingConstant, randomIters);
		testInst.apply(allSessions);
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.cross_validation.RandomTrainingComplementSetFactory#apply(se.kth.speech.coin.tangrams.analysis.io.SessionDataManager[])}.
	 *
	 * @throws IOException
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testApplyTooManyRandomIters() throws IOException {
		final int trainingSetSizeDiscountingConstant = 1;
		Assume.assumeTrue(String.format(
				"Number of sessions found is not greater than the training set size discounting constant (%d).",
				trainingSetSizeDiscountingConstant), allSessions.size() > trainingSetSizeDiscountingConstant);
		final int randomIters = allSessions.size() + 1;
		Assume.assumeTrue(String.format(
				"Number of sessions found is not lesser than the required number of random iterations (%d).",
				randomIters), allSessions.size() < randomIters);

		final RandomTrainingComplementSetFactory testInst = new RandomTrainingComplementSetFactory(RANDOM,
				trainingSetSizeDiscountingConstant, randomIters);
		testInst.apply(allSessions);
	}

}
