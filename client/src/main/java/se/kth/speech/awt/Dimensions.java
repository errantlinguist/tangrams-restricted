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
package se.kth.speech.awt;

import java.awt.Dimension;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 15 Mar 2017
 *
 */
public final class Dimensions {

	/**
	 * @see <a href="http://stackoverflow.com/a/10245583/1391325">Answer on
	 *      StackOverflow</a>
	 * @param imgSize
	 *            The dimensions to fit inside the given target boundary.
	 * @param boundary
	 *            The dimensions of the boundary to fit into.
	 * @return A new {@link Dimension} instance which is equal to or lesser than
	 *         the boundary dimensions while maintaining the original aspect
	 *         ratio of the supplied image dimensions.
	 */
	public static Dimension createScaledDimension(final Dimension imgSize, final Dimension boundary) {
		final int originalWidth = imgSize.width;
		final int originalHeight = imgSize.height;
		final int boundWidth = boundary.width;
		final int boundHeight = boundary.height;
		int newWidth = originalWidth;
		int newHeight = originalHeight;

		// first check if we need to scale width
		if (originalWidth > boundWidth) {
			// scale width to fit
			newWidth = boundWidth;
			// scale height to maintain aspect ratio
			newHeight = newWidth * originalHeight / originalWidth;
		}

		// then check if we need to scale even with the new height
		if (newHeight > boundHeight) {
			// scale height to fit instead
			newHeight = boundHeight;
			// scale width to maintain aspect ratio
			newWidth = newHeight * originalWidth / originalHeight;
		}

		return new Dimension(newWidth, newHeight);
	}

	private Dimensions() {

	}

}
