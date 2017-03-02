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
package se.kth.speech.coin.tangrams.view;

import java.awt.Color;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.Objects;
import java.util.concurrent.ConcurrentNavigableMap;
import java.util.concurrent.ConcurrentSkipListMap;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

import javax.swing.BorderFactory;
import javax.swing.border.Border;

import se.kth.speech.Lists;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 17 Jan 2017
 *
 */
final class PlayerHighlightBorderFactory implements Consumer<String>, Supplier<Border> {

	private static final Border MAIN_BORDER = BorderFactory.createDashedBorder(Color.RED, 2.0f, 2.0f, 2.0f, false);

	private final Function<? super String, ? extends Border> playerHighlightBorderFactory;

	private final ConcurrentNavigableMap<String, Boolean> selectingPlayerIds;

	PlayerHighlightBorderFactory() {
		this(Comparator.naturalOrder(), playerId -> MAIN_BORDER);
	}

	PlayerHighlightBorderFactory(final Comparator<? super String> playerIdOrdering,
			final Function<? super String, ? extends Border> playerHighlightBorderFactory) {
		selectingPlayerIds = new ConcurrentSkipListMap<>(playerIdOrdering);
		this.playerHighlightBorderFactory = playerHighlightBorderFactory;
	}

	PlayerHighlightBorderFactory(final String localPlayerId) {
		this(localPlayerId, new Function<String, Border>() {
			final Border secondaryBorder = BorderFactory.createDashedBorder(Color.MAGENTA, 2.0f, 2.0f, 2.0f, false);

			@Override
			public Border apply(final String playerId) {
				return Objects.equals(localPlayerId, playerId) ? MAIN_BORDER : secondaryBorder;
			}
		});
	}

	PlayerHighlightBorderFactory(final String localPlayerId,
			final Function<? super String, ? extends Border> playerHighlightBorderFactory) {
		this(Lists.comparingByIndex(Collections.singletonList(localPlayerId)).thenComparing(Comparator.naturalOrder()),
				playerHighlightBorderFactory);
	}

	@Override
	public void accept(final String playerId) {
		selectingPlayerIds.compute(playerId, (key, oldVal) -> {
			final Boolean result;
			if (oldVal == null) {
				// The given player had not already selected the coordinates
				// represented by this panel
				result = Boolean.TRUE;
			} else {
				// The given player had already selected the coordinates
				// represented by this panel; Remove the player's ID from the
				// map
				result = null;
			}
			return result;
		});
	}

	@Override
	public Border get() {
		final Border result;
		final Iterator<String> selectingPlayerIdIter = selectingPlayerIds.keySet().iterator();
		if (selectingPlayerIdIter.hasNext()) {
			String selectingPlayerId = selectingPlayerIdIter.next();
			Border nextBorder = playerHighlightBorderFactory.apply(selectingPlayerId);
			while (selectingPlayerIdIter.hasNext()) {
				selectingPlayerId = selectingPlayerIdIter.next();
				// The last-created border should now go on the outside of the
				// next to be created
				final Border outsideBorder = nextBorder;
				final Border insideBorder = playerHighlightBorderFactory.apply(selectingPlayerId);
				nextBorder = BorderFactory.createCompoundBorder(outsideBorder, insideBorder);
			}
			result = nextBorder;
		} else {
			result = null;
		}
		return result;
	}

}
