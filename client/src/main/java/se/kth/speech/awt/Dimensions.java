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

		final int original_width = imgSize.width;
		final int original_height = imgSize.height;
		final int bound_width = boundary.width;
		final int bound_height = boundary.height;
		int new_width = original_width;
		int new_height = original_height;

		// first check if we need to scale width
		if (original_width > bound_width) {
			// scale width to fit
			new_width = bound_width;
			// scale height to maintain aspect ratio
			new_height = new_width * original_height / original_width;
		}

		// then check if we need to scale even with the new height
		if (new_height > bound_height) {
			// scale height to fit instead
			new_height = bound_height;
			// scale width to maintain aspect ratio
			new_width = new_height * original_width / original_height;
		}

		return new Dimension(new_width, new_height);
	}

	private Dimensions() {

	}

}
