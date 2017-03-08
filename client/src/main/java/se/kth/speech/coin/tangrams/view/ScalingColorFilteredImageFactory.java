/*
 *  This file is part of tangrams.
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
package se.kth.speech.coin.tangrams.view;

import java.awt.Color;
import java.awt.Image;
import java.awt.Toolkit;
import java.awt.image.BufferedImage;
import java.awt.image.FilteredImageSource;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.net.URL;
import java.util.function.Function;

import javax.imageio.ImageIO;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.awt.ColorReplacementImageFilter;
import se.kth.speech.coin.tangrams.content.ImageDatum;
import se.kth.speech.coin.tangrams.content.ImageSize;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 1 Jan 2017
 *
 */
final class ScalingColorFilteredImageFactory implements Function<ImageDatum, Image> {
	
	private static final Logger LOGGER = LoggerFactory.getLogger(ScalingColorFilteredImageFactory.class);

	private final Toolkit toolkit;

	public ScalingColorFilteredImageFactory() {
		this(Toolkit.getDefaultToolkit());
	}

	public ScalingColorFilteredImageFactory(final Toolkit toolkit) {
		this.toolkit = toolkit;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Function#apply(java.lang.Object)
	 */
	@Override
	public Image apply(final ImageDatum imgDatum) {
		final URL imgResourceLoc = imgDatum.getResourceLoc();
		LOGGER.debug("Reading image data from \"{}\".", imgResourceLoc);
		try {
			BufferedImage origImg = ImageIO.read(imgResourceLoc);
			final int width = origImg.getWidth();
			final int height = origImg.getHeight();
			
			final boolean isSquare;
			if (width < height){
				isSquare = false;
			} else if (height < width){
				isSquare = false;
			} else {
				isSquare = true;
			}
			final ImageSize size = imgDatum.getSize();
			switch (size){
			case LARGE : {
				
				break;
			}
			case NORMAL : {
				break;
			}
			case SMALL : {
				break;
			}
			default : {
				break;
			}
			}
			final Color color = imgDatum.getColor();
			return toolkit
					.createImage(new FilteredImageSource(origImg.getSource(), new ColorReplacementImageFilter(color)));
		} catch (IOException e) {
			throw new UncheckedIOException(e);
		}
	
	}
}
