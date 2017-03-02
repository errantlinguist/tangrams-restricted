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
package se.kth.speech.awt;

import java.awt.Dimension;
import java.awt.Image;
import java.util.function.Function;

public final class ScaledImageFactory implements Function<Image, Image> {

	private final int imgWidth;

	private final int imgHeight;

	private final int hints;

	public ScaledImageFactory(final int hints, final Dimension containerDim, final int[] dimDivisors) {
		this(hints, containerDim.width / dimDivisors[0], containerDim.height / dimDivisors[1]);
	}

	public ScaledImageFactory(final int hints, final int imgWidth, final int imgHeight) {
		this.imgWidth = imgWidth;
		this.imgHeight = imgHeight;
		this.hints = hints;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Function#apply(java.lang.Object)
	 */
	@Override
	public Image apply(final Image origImg) {
		return origImg.getScaledInstance(imgWidth, imgHeight, hints);
	}

}