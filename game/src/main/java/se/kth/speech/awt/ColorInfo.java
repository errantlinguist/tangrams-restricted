/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.game.
 *
 *  se.kth.speech.coin.tangrams-restricted.game is free software: you can redistribute it and/or modify
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
import java.util.Arrays;
import java.util.Collections;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 24 Aug 2017
 *
 */
public enum ColorInfo {
	ALPHA, BLUE, BRIGHTNESS, GREEN, HUE, JAVA_NAME, RED, SATURATION;

	private static final List<ColorInfo> CANONICAL_ORDERING;

	private static final Map<Integer, Set<String>> RGB_COLOR_NAMES = Colors.createRGBColorNameMap(String::toUpperCase,
			TreeSet::new);

	static {
		Collections.unmodifiableList(CANONICAL_ORDERING = Arrays.asList(ColorInfo.RED, ColorInfo.GREEN, ColorInfo.BLUE,
				ColorInfo.ALPHA, ColorInfo.HUE, ColorInfo.SATURATION, ColorInfo.BRIGHTNESS, ColorInfo.JAVA_NAME));
		assert CANONICAL_ORDERING.size() == ColorInfo.values().length;
	}

	public static Map<ColorInfo, Object> createInfoMap(final Color color) {
		final Map<ColorInfo, Object> result = new EnumMap<>(ColorInfo.class);
		final int red = color.getRed();
		result.put(ColorInfo.RED, red);
		final int green = color.getGreen();
		result.put(ColorInfo.GREEN, green);
		final int blue = color.getBlue();
		result.put(ColorInfo.BLUE, blue);
		final int alpha = color.getAlpha();
		result.put(ColorInfo.ALPHA, alpha);

		{
			final float[] hsbVals = Color.RGBtoHSB(red, green, blue, null);
			result.put(ColorInfo.HUE, hsbVals[0]);
			result.put(ColorInfo.SATURATION, hsbVals[1]);
			result.put(ColorInfo.BRIGHTNESS, hsbVals[2]);
		}
		final Set<String> colorNames = RGB_COLOR_NAMES.getOrDefault(color.getRGB(), Collections.emptySet());
		result.put(ColorInfo.JAVA_NAME, colorNames);
		assert result.size() == ColorInfo.values().length;
		return result;
	}

	public static List<ColorInfo> getCanonicalOrdering() {
		return CANONICAL_ORDERING;
	}

}
