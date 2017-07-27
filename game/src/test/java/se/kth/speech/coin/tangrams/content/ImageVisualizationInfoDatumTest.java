/*
 *  This file is part of client.
 *
 *  client is free software: you can redistribute it and/or modify
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
import java.util.Collection;
import java.util.Random;
import java.util.function.Supplier;
import java.util.stream.Stream;

import org.junit.Assert;
import org.junit.Assume;
import org.junit.experimental.theories.DataPoints;
import org.junit.experimental.theories.Theories;
import org.junit.experimental.theories.Theory;
import org.junit.runner.RunWith;

import se.kth.speech.RandomStringFactory;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 10 Mar 2017
 *
 */
@RunWith(Theories.class)
public final class ImageVisualizationInfoDatumTest {

	@DataPoints
	public static final String[] RANDOM_TEST_RESOURCES;

	@DataPoints
	public static final Collection<String> STANDARD_TEST_RESOURCES;

	@DataPoints
	public static final Color[] TEST_COLORS = Stream.generate(() -> createRandomColor(new Random())).distinct().limit(4)
			.toArray(Color[]::new);

	@DataPoints
	public static final ImageSize[] TEST_SIZES = ImageSize.values();

	static {
		STANDARD_TEST_RESOURCES = IconImages.getImageResourceNames();
		final Supplier<String> randomStrFactory = new RandomStringFactory(4);
		RANDOM_TEST_RESOURCES = STANDARD_TEST_RESOURCES.stream().map(url -> appendAnyString(url, randomStrFactory))
				.toArray(String[]::new);
	}

	private static String appendAnyString(final String url, final Supplier<String> strSupplier) {
		final String suffix = strSupplier.get();
		return url + '/' + suffix;
	}

	private static Color createRandomColor(final Random rnd) {
		final int r = rnd.nextInt(256);
		final int g = rnd.nextInt(256);
		final int b = rnd.nextInt(256);
		return new Color(r, g, b);
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.content.ImageVisualizationInfo.Datum#equals(java.lang.Object)}.
	 */
	@Theory
	public void testEqualsObjectCommutativity(final String u1, final Color c1, final ImageSize s1, final String u2,
			final Color c2, final ImageSize s2) {
		final ImageVisualizationInfo.Datum o1 = new ImageVisualizationInfo.Datum(u1, c1, s1);
		final ImageVisualizationInfo.Datum o2 = new ImageVisualizationInfo.Datum(u2, c2, s2);
		final boolean o1Test = o1.equals(o2);
		final boolean o2Test = o2.equals(o1);
		Assert.assertEquals(o1Test, o2Test);
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.content.ImageVisualizationInfo.Datum#equals(java.lang.Object)}.
	 */
	@Theory
	public void testEqualsObjectNegative(final String u1, final Color c1, final ImageSize s1, final String u2,
			final Color c2, final ImageSize s2) {
		Assume.assumeFalse(u1.equals(u2) && c1.equals(c2) && s1.equals(s2));
		final ImageVisualizationInfo.Datum o1 = new ImageVisualizationInfo.Datum(u1, c1, s1);
		final ImageVisualizationInfo.Datum o2 = new ImageVisualizationInfo.Datum(u2, c2, s2);
		Assert.assertNotEquals(o1, o2);
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.content.ImageVisualizationInfo.Datum#equals(java.lang.Object)}.
	 */
	@Theory
	public void testEqualsObjectPositive(final String u1, final Color c1, final ImageSize s1, final String u2,
			final Color c2, final ImageSize s2) {
		Assume.assumeTrue(u1.equals(u2));
		Assume.assumeTrue(c1.equals(c2));
		Assume.assumeTrue(s1.equals(s2));
		final ImageVisualizationInfo.Datum o1 = new ImageVisualizationInfo.Datum(u1, c1, s1);
		final ImageVisualizationInfo.Datum o2 = new ImageVisualizationInfo.Datum(u2, c2, s2);
		Assert.assertEquals(o1, o2);
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.content.ImageVisualizationInfo.Datum#hashCode()}.
	 */
	@Theory
	public void testHashCodeEqual(final String u1, final Color c1, final ImageSize s1, final String u2, final Color c2,
			final ImageSize s2) {
		final ImageVisualizationInfo.Datum o1 = new ImageVisualizationInfo.Datum(u1, c1, s1);
		final ImageVisualizationInfo.Datum o2 = new ImageVisualizationInfo.Datum(u2, c2, s2);
		Assume.assumeTrue(o1.equals(o2));
		final int h1 = o1.hashCode();
		final int h2 = o2.hashCode();
		Assert.assertEquals(h1, h2);
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.content.ImageVisualizationInfo.Datum#hashCode()}.
	 */
	@Theory
	public void testHashCodeSameData(final String u1, final Color c1, final ImageSize s1, final String u2,
			final Color c2, final ImageSize s2) {
		Assume.assumeTrue(u1.equals(u2));
		Assume.assumeTrue(c1.equals(c2));
		Assume.assumeTrue(s1.equals(s2));
		final ImageVisualizationInfo.Datum o1 = new ImageVisualizationInfo.Datum(u1, c1, s1);
		final ImageVisualizationInfo.Datum o2 = new ImageVisualizationInfo.Datum(u2, c2, s2);
		final int h1 = o1.hashCode();
		final int h2 = o2.hashCode();
		Assert.assertEquals(h1, h2);
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.content.ImageVisualizationInfo.Datum#Datum(java.lang.String, java.awt.Color, se.kth.speech.coin.tangrams.content.ImageSize)}.
	 */
	@Theory
	public void testImageVisualizationInfoDatum(final String u, final Color c, final ImageSize s) {
		new ImageVisualizationInfo.Datum(u, c, s);
	}

}
