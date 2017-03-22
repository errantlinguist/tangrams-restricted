/*
 *  This file is part of game.
 *
 *  game is free software: you can redistribute it and/or modify
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

import java.awt.Color;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Random;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 22 Mar 2017
 *
 */
public final class IconColors {

	private static class RandomHueColorFactory implements Supplier<Color> {

		private final Random rnd;

		public RandomHueColorFactory(final Random rnd) {
			this.rnd = rnd;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.util.function.Supplier#get()
		 */
		@Override
		public Color get() {
			final float hue = rnd.nextFloat();
			return Color.getHSBColor(hue, 1.0f, 1.0f);
		}

	}

	public static List<Color> createDefaultLengthRandomColorList(final Random rnd,
			final Collection<? super Color> blacklistedColors) {
		final RandomHueColorFactory colorFactory = new RandomHueColorFactory(rnd);
		final int size = 4;
		return Stream.generate(colorFactory).filter(color -> !blacklistedColors.contains(color)).distinct().limit(size)
				.collect(Collectors.toCollection(() -> new ArrayList<>(size)));
	}

	private IconColors() {

	}
}
