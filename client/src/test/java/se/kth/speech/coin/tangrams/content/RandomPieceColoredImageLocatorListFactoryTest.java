/*
 *  This file is part of se.kth.speech.coin.tangrams.client.
 *
 *  se.kth.speech.coin.tangrams.client is free software: you can redistribute it and/or modify
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
package se.kth.speech.coin.tangrams.content;

import java.awt.Color;
import java.net.URL;
import java.util.Collection;
import java.util.List;
import java.util.Map.Entry;
import java.util.Random;

import org.hamcrest.collection.IsIterableContainingInOrder;
import org.junit.Assert;
import org.junit.experimental.theories.DataPoints;
import org.junit.experimental.theories.Theories;
import org.junit.experimental.theories.Theory;
import org.junit.runner.RunWith;

import se.kth.speech.coin.tangrams.content.ContentTests.TestDescription;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 21 Feb 2017
 *
 */
@RunWith(Theories.class)
public final class RandomPieceColoredImageLocatorListFactoryTest {

	@DataPoints("testDescs")
	public static Collection<TestDescription> getTestDescs() {
		return ContentTests.getNamedTestDescMap().values();
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.content.RandomPieceColoredImageLocatorListFactory#apply(java.util.Random)}.
	 */
	@Theory
	public final void testApply(final TestDescription testDesc) {
		final List<Entry<URL, Color>> expectedColoredImgResourceLocs = testDesc.getColoredImgResourceLocators();
		final List<Entry<URL, Color>> actualColoredImgResourceLocs = new RandomPieceColoredImageLocatorListFactory()
				.apply(new Random(testDesc.getSeed()));
		Assert.assertThat(actualColoredImgResourceLocs,
				IsIterableContainingInOrder.contains(expectedColoredImgResourceLocs.toArray()));
	}

}
