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
package se.kth.speech.awt;

import java.awt.Component;
import java.awt.image.BufferedImage;

/**
 * Create a screenshot of a component.
 *
 * @author Andrew Thompson
 * @see <a href="http://stackoverflow.com/a/5853992/1391325">Original SO
 *      answer</a>
 */
public final class ComponentImageCapture {

	public static BufferedImage createScreenshot(final Component component) {
		final BufferedImage result = new BufferedImage(component.getWidth(), component.getHeight(),
				BufferedImage.TYPE_INT_RGB);
		// call the Component's paint method, using
		// the Graphics object of the image.
		component.paint(result.getGraphics()); // alternately use .printAll(..)
		return result;
	}

	private ComponentImageCapture() {

	}
}