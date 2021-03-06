/*
 *  This file is part of Tangrams-restricted.
 *
 *  Tangrams-restricted is free software: you can redistribute it and/or modify
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

import java.awt.Image;
import java.awt.Toolkit;
import java.awt.image.BufferedImage;
import java.awt.image.FilteredImageSource;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.net.URL;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.IntSupplier;

import javax.imageio.ImageIO;

import com.google.common.collect.Maps;

import se.kth.speech.MutablePair;
import se.kth.speech.awt.ColorReplacementImageFilter;

/**
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since Mar 19, 2017
 *
 */
public final class ImageLoadingImageViewInfoFactory
		implements Function<ImageVisualizationInfo.Datum, Entry<ImageViewInfo, Image>> {

	private static Function<URL, BufferedImage> createResourceImgFactory(final int expectedUniqueResourceImgCount) {
		final Map<URL, BufferedImage> resourceImgs = Maps.newHashMapWithExpectedSize(expectedUniqueResourceImgCount);
		return imgResourceLoc -> resourceImgs.computeIfAbsent(imgResourceLoc, loc -> {
			try {
				return ImageIO.read(loc);
			} catch (final IOException e) {
				throw new UncheckedIOException(e);
			}
		});
	}

	private final BiFunction<? super Image, ? super Toolkit, ? extends Image> postColoringImgTransformer;

	private final Function<? super URL, ? extends BufferedImage> resourceImgFactory;

	private final Toolkit toolkit;

	public ImageLoadingImageViewInfoFactory(final Toolkit toolkit,
			final BiFunction<? super Image, ? super Toolkit, ? extends Image> postColoringImgTransformer,
			final int expectedUniqueResourceImgCount) {
		this(toolkit, postColoringImgTransformer, createResourceImgFactory(expectedUniqueResourceImgCount));
	}

	private ImageLoadingImageViewInfoFactory(final Toolkit toolkit,
			final BiFunction<? super Image, ? super Toolkit, ? extends Image> postColoringImgTransformer,
			final Function<? super URL, ? extends BufferedImage> resourceImgFactory) {
		this.toolkit = toolkit;
		this.postColoringImgTransformer = postColoringImgTransformer;
		this.resourceImgFactory = resourceImgFactory;
	}

	@Override
	public Entry<ImageViewInfo, Image> apply(final ImageVisualizationInfo.Datum imgVisualizationInfoDatum) {
		final String imgResourceName = imgVisualizationInfoDatum.getResourceName();
		final BufferedImage initialImg = resourceImgFactory
				.apply(IconImages.getImageUrl(ImageType.ICON, imgResourceName));
		final IntSupplier widthGetter = initialImg::getWidth;
		final IntSupplier heightGetter = initialImg::getHeight;
		// Size/aspect ratio calculation
		final ImageViewInfo.RasterizationInfo imgRasterizationInfo = new ImageViewInfo.RasterizationInfo(widthGetter,
				heightGetter);
		final Image coloredImg = toolkit.createImage(new FilteredImageSource(initialImg.getSource(),
				new ColorReplacementImageFilter(imgVisualizationInfoDatum.getColor())));
		final ImageViewInfo imgInfo = new ImageViewInfo(imgVisualizationInfoDatum, imgRasterizationInfo);
		final Image transformedImg = postColoringImgTransformer.apply(coloredImg, toolkit);
		return new MutablePair<>(imgInfo, transformedImg);
	}

}
