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
import java.awt.Component;
import java.awt.Container;
import java.awt.Image;
import java.awt.MediaTracker;
import java.awt.image.BufferedImage;
import java.net.URL;
import java.util.Collection;
import java.util.List;
import java.util.Map.Entry;
import java.util.Random;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import org.junit.Assert;
import org.junit.Assume;
import org.junit.experimental.theories.DataPoints;
import org.junit.experimental.theories.Theories;
import org.junit.experimental.theories.Theory;
import org.junit.runner.RunWith;

import se.kth.speech.awt.BufferedImages;
import se.kth.speech.awt.ColorFilteredImageFactory;
import se.kth.speech.coin.tangrams.content.ContentTests.TestDescription;
import se.kth.speech.junit.IteratorEqualityAsserter;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 20 Feb 2017
 *
 */
@RunWith(Theories.class)
public final class PieceIdImageFactoryFactoryTest {

	private static class WaitingImageLoader implements Function<Image, Image> {

		private final MediaTracker mt;

		private final AtomicInteger nextId = new AtomicInteger(0);

		private WaitingImageLoader() {
			this(new Container());
		}

		private WaitingImageLoader(final Component mtComp) {
			this(new MediaTracker(mtComp));
		}

		private WaitingImageLoader(final MediaTracker mt) {
			this.mt = mt;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.util.function.Function#apply(java.lang.Object)
		 */
		@Override
		public Image apply(final Image img) {
			final int id = nextId.getAndIncrement();
			mt.addImage(img, id);
			try {
				// http://stackoverflow.com/a/42401667/1391325
				mt.waitForID(id);
			} catch (final InterruptedException e) {
				Assume.assumeNoException("The image-loading wait thread was interrupted.", e);
			}
			return img;
		}

	}

	private static final BiConsumer<BufferedImage, BufferedImage> PIXEL_EQUALITY_ASSERTER = (img1, img2) -> {
		Assert.assertTrue("Images are not equal!", BufferedImages.areEqualByPixel(img1, img2));
	};

	@DataPoints("testDescs")
	public static Collection<TestDescription> getTestDescs() {
		return ContentTests.getNamedTestDescMap().values();
	}

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.content.PieceIdImageFactoryFactory#apply(int)}.
	 */
	@Theory
	public final void testApply(final TestDescription testDesc) {
		final List<Entry<URL, Color>> expectedColoredImgResourceLocs = testDesc.getColoredImgResourceLocators();
		final Stream<Image> expectedColoredImgs = IntStream.range(0, expectedColoredImgResourceLocs.size()).boxed()
				.map(new ColorFilteredImageFactory(expectedColoredImgResourceLocs));
		final Stream<Image> actualColoredImgs = IntStream.range(0, expectedColoredImgResourceLocs.size()).boxed()
				.map(new PieceIdImageFactoryFactory(new Random(testDesc.getSeed()))
						.apply(testDesc.calculateOccupiedCellCount()));
		final Function<Image, Image> imgLoadWaiter = new WaitingImageLoader();
		final Stream<BufferedImage> bufferedExpectedColoredImgs = expectedColoredImgs.map(imgLoadWaiter)
				.map(BufferedImages::toBufferedImage);
		final Stream<BufferedImage> bufferedActualColoredImgs = actualColoredImgs.map(imgLoadWaiter)
				.map(BufferedImages::toBufferedImage);
		new IteratorEqualityAsserter<>(PIXEL_EQUALITY_ASSERTER).accept(bufferedExpectedColoredImgs.iterator(),
				bufferedActualColoredImgs.iterator());
	}

}
