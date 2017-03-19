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
import java.awt.image.ColorModel;
import java.awt.image.RGBImageFilter;

public final class ColorReplacementImageFilter extends RGBImageFilter {

	private static final ColorModel COLOR_MODEL = ColorModel.getRGBdefault();

	private final Color filterColor;

	public ColorReplacementImageFilter(final Color filterColor) {
		this.filterColor = filterColor;
		// The filter's operation does not depend on the
		// pixel's location, so IndexColorModels can be
		// filtered directly.
		canFilterIndexColorModel = true;
	}

	@Override
	public int filterRGB(final int x, final int y, final int rgb) {
		final int alpha = COLOR_MODEL.getAlpha(rgb);
		final int red = filterColor.getRed();
		final int green = filterColor.getGreen();
		final int blue = filterColor.getBlue();
		return alpha << 24 | red << 16 | green << 8 | blue;
	}
}