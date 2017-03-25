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

import java.awt.Font;
import java.awt.FontMetrics;
import java.util.function.Function;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 24 Mar 2017
 * @see <a href="http://stackoverflow.com/a/8184372/1391325">StackOverflow</a>
 */
public final class MaximumWidthFontFactory implements Function<String, Font> {

	private final Function<? super Font, FontMetrics> fmFactory;

	private final float endSize;

	private final float increment;

	private final Font oldFont;

	private final float startSize;

	private final int widthPadding;

	private final int maxWidth;

	public MaximumWidthFontFactory(final int maxWidth, final Function<? super Font, FontMetrics> fmFactory,
			final Font oldFont, final float startSize, final float endSize, final float increment,
			final int widthPadding) {
		this.maxWidth = maxWidth;
		this.fmFactory = fmFactory;
		this.oldFont = oldFont;
		this.startSize = startSize;
		this.endSize = endSize;
		this.increment = increment;
		this.widthPadding = widthPadding;
	}

	@Override
	public Font apply(final String text) {
		Font result = oldFont;
		for (float i = startSize; startSize < endSize; i += increment) {
			final Font newFont = oldFont.deriveFont(i);
			final int w = getFontWidth(fmFactory.apply(newFont), text);
			if (w <= maxWidth) {
				result = newFont;
			} else {
				break;
			}
		}
		return result;
	}

	private int getFontWidth(final FontMetrics metrics, final String text) {
		// get the advance of my text in this font and render context
		final int adv = metrics.stringWidth(text);
		// calculate the size of a box to hold the text with some padding.
		return adv + widthPadding;
	}

}
