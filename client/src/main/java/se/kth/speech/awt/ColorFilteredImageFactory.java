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
package se.kth.speech.awt;

import java.awt.Color;
import java.awt.Image;
import java.awt.Toolkit;
import java.awt.image.FilteredImageSource;
import java.net.URL;
import java.util.function.BiFunction;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 1 Jan 2017
 *
 */
public final class ColorFilteredImageFactory implements BiFunction<URL, Color, Image> {

	private final Toolkit toolkit;

	public ColorFilteredImageFactory() {
		this(Toolkit.getDefaultToolkit());
	}

	public ColorFilteredImageFactory(final Toolkit toolkit) {
		this.toolkit = toolkit;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Function#apply(java.lang.Object)
	 */
	@Override
	public Image apply(final URL imgResourceLoc, final Color color) {
		final Image origImg = toolkit.getImage(imgResourceLoc);
			return toolkit
					.createImage(new FilteredImageSource(origImg.getSource(), new ColorReplacementImageFilter(color)));
	}
}
