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
package se.kth.speech.coin.tangrams.content;

import java.awt.Color;
import java.util.Collections;
import java.util.EnumMap;
import java.util.Map;

import javax.swing.UIManager;

/**
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since 23 Mar 2017
 *
 */
public enum BoardArea {
	BACKGROUND, HIGHLIGHT;

	private static final Map<BoardArea, Color> DEFAULT_BOARD_AREA_COLORS = Collections.unmodifiableMap(createDefaultBoardAreaColorMap());

	public static Map<BoardArea, Color> getDefaultBoardAreaColorMap() {
		return DEFAULT_BOARD_AREA_COLORS;
	}

	private static Map<BoardArea, Color> createDefaultBoardAreaColorMap() {
		final Map<BoardArea, Color> result = new EnumMap<>(BoardArea.class);
		// http://stackoverflow.com/a/9993139/1391325
		result.put(BoardArea.BACKGROUND, UIManager.getColor("Panel.background"));
		result.put(BoardArea.HIGHLIGHT, Color.MAGENTA);
		assert result.size() == BoardArea.values().length;
		return result;
	}

}
