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

import java.util.Arrays;
import java.util.Comparator;
import java.util.EnumMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import se.kth.speech.Lists;

/**
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since 7 Mar 2017
 *
 */
public enum ImageSize {
	LARGE, MEDIUM, SMALL;

	private static final Comparator<ImageSize> SIZE_COMPARATOR;

	private static final List<ImageSize> SIZE_ORDERING;

	static {
		SIZE_ORDERING = Arrays.asList(SMALL, MEDIUM, LARGE);
		assert SIZE_ORDERING.size() == ImageSize.values().length;
		SIZE_COMPARATOR = Lists.comparingByIndex(SIZE_ORDERING);
	}

	public static Map<ImageSize, Integer> createDefaultImageSizeFactorMap() {
		final Map<ImageSize, Integer> result = new EnumMap<>(ImageSize.class);
		final Iterator<ImageSize> sizes = ImageSize.getSizeOrdering().iterator();
		result.put(sizes.next(), 2);
		result.put(sizes.next(), 3);
		result.put(sizes.next(), 4);
		assert result.size() == ImageSize.values().length;
		return result;
	}

	/**
	 * @return the sizeComparator
	 */
	public static Comparator<ImageSize> getSizeComparator() {
		return SIZE_COMPARATOR;
	}

	/**
	 * @return the sizeOrdering
	 */
	public static List<ImageSize> getSizeOrdering() {
		return SIZE_ORDERING;
	}

}
