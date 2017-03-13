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

import java.awt.image.ColorModel;
import java.awt.image.RGBImageFilter;

public final class OpaqueTransparencyReplacementImageFilter extends RGBImageFilter {

	private static final ColorModel COLOR_MODEL = ColorModel.getRGBdefault();
	
	private final int alpha;

	public OpaqueTransparencyReplacementImageFilter(final int alpha) {
		this.alpha = alpha;
		// The filter's operation does not depend on the
		// pixel's location, so IndexColorModels can be
		// filtered directly.
		canFilterIndexColorModel = true;
	}

	@Override
	public int filterRGB(final int x, final int y, final int rgb) {
		int alpha = COLOR_MODEL.getAlpha(rgb);
		if (alpha == 0xff) {
			// Replace the opaque alpha with the alpha specified by the instance
			// of this class
			alpha = this.alpha;
		}
		final int red = COLOR_MODEL.getRed(rgb);
		final int green = COLOR_MODEL.getGreen(rgb);
		final int blue = COLOR_MODEL.getBlue(rgb);
		return alpha << 24 | red << 16 | green << 8 | blue;
	}
}