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
import java.net.MalformedURLException;
import java.net.URL;
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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.RandomStringFactory;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 10 Mar 2017
 *
 */
@RunWith(Theories.class)
public final class ImageVisualizationInfoTest {

	private static final Logger LOGGER = LoggerFactory.getLogger(ImageVisualizationInfoTest.class);

	@DataPoints
	public static final Color[] TEST_COLORS = Stream.generate(() -> createRandomColor(new Random())).distinct().limit(4)
			.toArray(Color[]::new);

	@DataPoints
	public static final ImageSize[] TEST_SIZES = ImageSize.values();

	@DataPoints
	public static final Collection<URL> STANDARD_TEST_URLS;

	@DataPoints
	public static final URL[] RANDOM_TEST_URLS;

	static {
		STANDARD_TEST_URLS = IconImages.getImageResources().values();
		final Supplier<String> randomStrFactory = new RandomStringFactory(4);
		RANDOM_TEST_URLS = STANDARD_TEST_URLS.stream().map(url -> appendAnyString(url, randomStrFactory))
				.toArray(URL[]::new);
	}

	private static URL appendAnyString(final URL url, final Supplier<String> strSupplier) {
		URL result = null;
		do {
			final String suffix = strSupplier.get();
			try {
				result = new URL(url, suffix);
			} catch (final MalformedURLException e) {
				LOGGER.debug("Swallowing exception.", e);
			}
		} while (result == null);
		return result;
	}

	private static Color createRandomColor(final Random rnd) {
		final int r = rnd.nextInt(256);
		final int g = rnd.nextInt(256);
		final int b = rnd.nextInt(256);
		return new Color(r, g, b);
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.content.ImageVisualizationInfo#equals(java.lang.Object)}.
	 */
	@Theory
	public final void testEqualsObjectCommutativity(final URL u1, final Color c1, final ImageSize s1, final URL u2,
			final Color c2, final ImageSize s2) {
		final ImageVisualizationInfo o1 = new ImageVisualizationInfo(u1, c1, s1);
		final ImageVisualizationInfo o2 = new ImageVisualizationInfo(u2, c2, s2);
		final boolean o1Test = o1.equals(o2);
		final boolean o2Test = o2.equals(o1);
		Assert.assertEquals(o1Test, o2Test);
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.content.ImageVisualizationInfo#equals(java.lang.Object)}.
	 */
	@Theory
	public final void testEqualsObjectNegative(final URL u1, final Color c1, final ImageSize s1, final URL u2,
			final Color c2, final ImageSize s2) {
		Assume.assumeFalse(u1.equals(u2) && c1.equals(c2) && s1.equals(s2));
		final ImageVisualizationInfo o1 = new ImageVisualizationInfo(u1, c1, s1);
		final ImageVisualizationInfo o2 = new ImageVisualizationInfo(u2, c2, s2);
		Assert.assertNotEquals(o1, o2);
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.content.ImageVisualizationInfo#equals(java.lang.Object)}.
	 */
	@Theory
	public final void testEqualsObjectPositive(final URL u1, final Color c1, final ImageSize s1, final URL u2,
			final Color c2, final ImageSize s2) {
		Assume.assumeTrue(u1.equals(u2));
		Assume.assumeTrue(c1.equals(c2));
		Assume.assumeTrue(s1.equals(s2));
		final ImageVisualizationInfo o1 = new ImageVisualizationInfo(u1, c1, s1);
		final ImageVisualizationInfo o2 = new ImageVisualizationInfo(u2, c2, s2);
		Assert.assertEquals(o1, o2);
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.content.ImageVisualizationInfo#hashCode()}.
	 */
	@Theory
	public final void testHashCodeEqual(final URL u1, final Color c1, final ImageSize s1, final URL u2, final Color c2,
			final ImageSize s2) {
		final ImageVisualizationInfo o1 = new ImageVisualizationInfo(u1, c1, s1);
		final ImageVisualizationInfo o2 = new ImageVisualizationInfo(u2, c2, s2);
		Assume.assumeTrue(o1.equals(o2));
		final int h1 = o1.hashCode();
		final int h2 = o2.hashCode();
		Assert.assertEquals(h1, h2);
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.content.ImageVisualizationInfo#hashCode()}.
	 */
	@Theory
	public final void testHashCodeSameData(final URL u1, final Color c1, final ImageSize s1, final URL u2,
			final Color c2, final ImageSize s2) {
		Assume.assumeTrue(u1.equals(u2));
		Assume.assumeTrue(c1.equals(c2));
		Assume.assumeTrue(s1.equals(s2));
		final ImageVisualizationInfo o1 = new ImageVisualizationInfo(u1, c1, s1);
		final ImageVisualizationInfo o2 = new ImageVisualizationInfo(u2, c2, s2);
		final int h1 = o1.hashCode();
		final int h2 = o2.hashCode();
		Assert.assertEquals(h1, h2);
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.content.ImageVisualizationInfo#ImageVisualizationInfo(java.net.URL, java.awt.Color, se.kth.speech.coin.tangrams.content.ImageSize)}.
	 */
	@Theory
	public final void testImageVisualizationInfo(final URL u, final Color c, final ImageSize s) {
		new ImageVisualizationInfo(u, c, s);
	}

}
